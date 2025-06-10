#' Analyze an entire time series by segmenting it into evolutionary model phases
#'
#' This function takes a time series dataset, segments it into meaningful subgroups based on
#' evolutionary models (Stasis, Unbiased Random Walk, or General Random Walk), and returns
#' information about each segment including the best-fitting model.
#'
#' @param file_path Character string specifying the path to the CSV file containing the time series data.
#'        The file should contain columns for time and measurement data. Can be a full path or a file in the working directory.
#'        If the file doesn't have a .csv extension, it will be added automatically.
#' @param include_nre Logical indicating whether to include the non-random evolution (NRE) model
#'        in the analysis. Default is TRUE.
#' @param min_points Minimum number of data points required for analysis (default: 7)
#' @param evaluation_order Character vector specifying the order of models to evaluate. 
#'   Default is c("Stasis", "URW", "GRW").
#'
#' @return A list with the following components:
#' \item{subgroups}{A list of data frames, each containing information about a segment of the time series}
#' \item{file_path}{The path to the input file}
#' \item{num_subgroups}{The number of segments identified}
#' \item{time_range}{The overall time range of the data}
#' \item{model_summary}{A summary of the models used for each segment}
#'
#' @details The function works by iteratively analyzing segments of the time series, starting from the
#'          oldest data points. It identifies the longest possible segment that can be modeled by one
#'          of the specified evolutionary models, then repeats the process for the remaining data points.
#'          The function ensures that all data points are included in the analysis and that the last
#'          'min_points' points form their own segment if they're not already included in a previous one.
#'
#' @examples
#' \dontrun{
#'   # Analyze a time series file
#'   results <- analyze_entire_series(
#'     "path/to/your/data.csv",
#'     include_nre = TRUE,
#'     min_points = 7,
#'     evaluation_order = c("Stasis", "URW", "GRW")
#'   )
#'   
#'   # Print summary of results
#'   print(results$model_summary)
#'   
#'   # Access individual segments
#'   first_segment <- results$subgroups[[1]]
#'   cat(sprintf("First segment: %.1f to %.1f (model: %s)\n",
#'               first_segment$start_time,
#'               first_segment$end_time,
#'               first_segment$model))
#' }
#'
#' @seealso \code{\link{analyze_stasis_trait}}, \code{\link{analyze_urw_trait}}, \code{\link{analyze_grw_trait}}
#'
# Base R imports
#' @import methods
#' @import stats
#' @import utils
#' @import grDevices
#' @import graphics
#' @import grid

# Package imports
#' @import dplyr
#' @import ggplot2
#' @import qcc

# Import specific functions
#' @importFrom magrittr %>% %<>%
#' @importFrom forecast auto.arima
#' @importFrom nortest ad.test
#' @importFrom tseries jarque.bera.test
#' @importFrom stringr str_extract
#' @export
analyze_entire_series <- function(file_path, include_nre = TRUE, min_points = 7, 
                                evaluation_order = c("Stasis", "URW", "GRW")) {
  # Read and prepare data
  df <- read.csv(file_path)
  colnames(df) <- tolower(colnames(df))
  
  # Handle different column name variations
  if (!'time' %in% colnames(df) && 'tt' %in% colnames(df)) {
    df <- df %>% rename(time = tt)
  }
  if (!'mn' %in% colnames(df) && 'mm' %in% colnames(df)) {
    df <- df %>% rename(mn = mm)
  }
  if (!'sd' %in% colnames(df) && 'vv' %in% colnames(df)) {
    df <- df %>% rename(sd = vv)
  }
  if (!'n' %in% colnames(df) && 'nn' %in% colnames(df)) {
    df <- df %>% rename(n = nn)
  }
  
  # Ensure required columns exist
  required_cols <- c('time', 'mn', 'sd', 'n')
  if (!all(required_cols %in% colnames(df))) {
    stop("Input data must contain columns for time, mean (mn/mm), standard deviation (sd/vv), and sample size (n/nn)")
  }
  
  df <- df %>% arrange(time)  # Ensure data is ordered by time
  
  # Initialize variables
  subgroups <- list()
  current_start <- 1  # Start with the oldest timepoint
  found_valid_segment <- FALSE
  
  # Continue until we've analyzed all points
  while(current_start <= nrow(df)) {
    # Skip if remaining points are too few for meaningful analysis
    if(nrow(df) - current_start + 1 < min_points) {
      break
    }
    
    # Find the youngest timepoint in the entire remaining series where a simple model can be predicted
    youngest_valid_end <- NULL
    youngest_valid_model <- NULL
    
    # Try all possible end points from current_start + min_points - 1 to the end
    for(end_idx in (current_start + min_points - 1):nrow(df)) {
      # Get current segment (always start at current_start)
      segment_data <- df[current_start:end_idx,]
      
      # Try to predict model for this segment
      result <- predict_evolutionary_model(segment_data, evaluation_order = evaluation_order, include_nre = include_nre, verbose = FALSE)
      
      # If we can predict a simple model, update youngest valid endpoint
      if(!is.null(result$best_model) && result$best_model %in% c("stasis", "urw", "grw")) {
        youngest_valid_end <- end_idx
        youngest_valid_model <- result$best_model
        found_valid_segment <- TRUE
      }
    }
    
    # If we found a valid segment, store it
    if(!is.null(youngest_valid_end)) {
      # Store the current subgroup
      subgroups[[length(subgroups) + 1]] <- list(
        start_time = df$time[current_start],
        end_time = df$time[youngest_valid_end],
        model = youngest_valid_model,
        start_idx = current_start,
        end_idx = youngest_valid_end
      )
      
      # The next subgroup starts at the previous end index (overlap)
      if (youngest_valid_end == nrow(df)) break
      current_start <- youngest_valid_end
    } else {
      break
    }
  }
  
  # Always ensure we have a segment that includes the last timepoint
  if (nrow(df) >= min_points) {
    # Check if the last timepoint is already included in a subgroup
    last_time_included <- any(sapply(subgroups, function(sg) sg$end_time == tail(df$time, 1)))
    
    if (!last_time_included) {
      # We need to include the last timepoint in a segment
      # Find the earliest index we can start from to include the last timepoint with min_points
      start_idx <- max(1, nrow(df) - min_points + 1)
      
      # If we already have subgroups, make sure we don't overlap with the last one
      if (length(subgroups) > 0) {
        last_subgroup <- tail(subgroups, 1)[[1]]
        # Start from the point after the last subgroup ends
        start_idx <- max(start_idx, last_subgroup$end_idx + 1)
      }
      
      # Make sure we have enough points
      if (nrow(df) - start_idx + 1 >= min_points) {
        last_segment <- df[start_idx:nrow(df), ]
        cat("\nAnalyzing final segment from", min(last_segment$time), "to", max(last_segment$time), "with", nrow(last_segment), "points\n")
        
        result <- predict_evolutionary_model(last_segment, verbose = FALSE)
        
        if (!is.null(result$best_model) && result$best_model %in% c("stasis", "urw", "grw")) {
          # Add the last segment as a new subgroup
          new_subgroup <- list(
            start_time = df$time[start_idx],
            end_time = df$time[nrow(df)],
            model = result$best_model,
            start_idx = start_idx,
            end_idx = nrow(df)
          )
          subgroups[[length(subgroups) + 1]] <- new_subgroup
          cat("Added final segment:", new_subgroup$start_time, "to", new_subgroup$end_time, 
              "(model:", new_subgroup$model, ")\n")
        } else {
          # If no valid model, add as "NO MODEL PREDICTED"
          new_subgroup <- list(
            start_time = df$time[start_idx],
            end_time = df$time[nrow(df)],
            model = "NO MODEL PREDICTED",
            start_idx = start_idx,
            end_idx = nrow(df)
          )
          subgroups[[length(subgroups) + 1]] <- new_subgroup
          cat("Added final segment with no valid model:", 
              new_subgroup$start_time, "to", new_subgroup$end_time, "\n")
        }
      }
    }
  } else if (nrow(df) >= min_points) {
    # No valid segments found, but we have enough points for at least one segment
    # Try to analyze the last min_points points as a segment
    start_idx <- nrow(df) - min_points + 1
    last_segment <- df[start_idx:nrow(df), ]
    result <- predict_evolutionary_model(last_segment, verbose = FALSE)
    if (!is.null(result$best_model) && result$best_model %in% c("stasis", "urw", "grw")) {
      subgroups[[1]] <- list(
        start_time = df$time[start_idx],
        end_time = df$time[nrow(df)],
        model = result$best_model,
        start_idx = start_idx,
        end_idx = nrow(df)
      )
    } else {
      # If no valid model, return a single "NO MODEL PREDICTED" subgroup
      subgroups <- list(list(
        start_time = df$time[1],
        end_time = df$time[nrow(df)],
        model = "NO MODEL PREDICTED",
        start_idx = 1,
        end_idx = nrow(df)
      ))
    }
  }
  
  # After finding subgroups, ensure the last min_points points form their own subgroup
  if (length(subgroups) > 0) {
    last_subgroup <- tail(subgroups, 1)[[1]]
    last_time <- tail(df$time, 1)
    
    cat("\nLast timepoint in data:", last_time)
    cat("\nLast subgroup ends at:", last_subgroup$end_time, "\n")
    
    if (last_subgroup$end_time < last_time) {
      cat("\nAdjusting to ensure last", min_points, "points form their own subgroup\n")
      
      # Find the timepoint that's exactly min_points points from the end
      # and make sure the last subgroup ends exactly at this timepoint
      new_last_subgroup_end_idx <- nrow(df) - min_points + 1
      
      if (new_last_subgroup_end_idx >= 1) {
        # Update the end of the last subgroup to be exactly at this timepoint
        subgroups[[length(subgroups)]]$end_idx <- new_last_subgroup_end_idx
        subgroups[[length(subgroups)]]$end_time <- df$time[new_last_subgroup_end_idx]
        
        # Re-analyze the adjusted last subgroup
        adjusted_segment <- df[subgroups[[length(subgroups)]]$start_idx:new_last_subgroup_end_idx, ]
        result <- predict_evolutionary_model(adjusted_segment, evaluation_order = evaluation_order, verbose = FALSE)
        
        if (!is.null(result$best_model) && result$best_model %in% c("stasis", "urw", "grw")) {
          subgroups[[length(subgroups)]]$model <- result$best_model
          cat("  Updated last subgroup to end at:", df$time[new_last_subgroup_end_idx], "(model:", result$best_model, ")\n")
        } else {
          subgroups[[length(subgroups)]]$model <- "NO MODEL PREDICTED"
          cat("  Updated last subgroup to end at:", df$time[new_last_subgroup_end_idx], "(no model predicted)\n")
        }
        
        # Add a new subgroup starting exactly at the last subgroup's end time
        last_segment_start_idx <- new_last_subgroup_end_idx
        last_segment <- df[last_segment_start_idx:nrow(df), ]
        result <- predict_evolutionary_model(last_segment, verbose = FALSE)
        
        if (!is.null(result$best_model) && result$best_model %in% c("stasis", "urw", "grw")) {
          new_subgroup <- list(
            start_time = df$time[last_segment_start_idx],
            end_time = last_time,
            model = result$best_model,
            start_idx = last_segment_start_idx,
            end_idx = nrow(df)
          )
          subgroups[[length(subgroups) + 1]] <- new_subgroup
          cat("  Added final subgroup with", nrow(last_segment), "points:", 
              new_subgroup$start_time, "to", new_subgroup$end_time, 
              "(model:", result$best_model, ")\n")
        } else {
          new_subgroup <- list(
            start_time = df$time[last_segment_start_idx],
            end_time = last_time,
            model = "NO MODEL PREDICTED",
            start_idx = last_segment_start_idx,
            end_idx = nrow(df)
          )
          subgroups[[length(subgroups) + 1]] <- new_subgroup
          cat("  Added final subgroup with", nrow(last_segment), "points:", 
              new_subgroup$start_time, "to", new_subgroup$end_time, 
              "(no model predicted)\n")
        }
      } else {
        cat("  Not enough points to create a separate subgroup for the last points\n")
      }
    }
  }
  
  # Generate the visualization
  if (length(subgroups) > 0) {
    visualize_subgroups(df, subgroups, file_path)
  }
  
  # Print results with clear subgroup boundaries
  cat("\nSubgroups found:\n")
  for(i in seq_along(subgroups)) {
    subgroup <- subgroups[[i]]
    cat(sprintf("Subgroup %d: %.1f to %.1f (model: %s)\n", 
                i,
                subgroup$start_time, 
                subgroup$end_time, 
                subgroup$model))
    
    # If this isn't the last subgroup, show the shared boundary
    if(i < length(subgroups)) {
      next_subgroup <- subgroups[[i + 1]]
      cat(sprintf("  -> Shared boundary at %.1f (end of Subgroup %d, start of Subgroup %d)\n",
                  subgroup$end_time, i, i + 1))
    }
  }
  
  # Print the prediction output
  cat(create_prediction_output(subgroups))
  
  # Only check for punctuated equilibrium if we found valid segments
  if (length(subgroups) >= 3) {
    has_punctuation <- FALSE
    for(i in 1:(length(subgroups)-2)) {
      if(subgroups[[i]]$model == "stasis" &&
         subgroups[[i+1]]$model == "grw" &&
         subgroups[[i+2]]$model == "stasis") {
        has_punctuation <- TRUE
        break
      }
    }
    
    cat("\nPunctuated Equilibrium Pattern (Stasis -> GRW -> Stasis): ", 
        if(has_punctuation) "DETECTED" else "NOT DETECTED", "\n")
  }
  
  return(subgroups)
}
