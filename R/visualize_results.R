
#' Setup Control Chart for Evolutionary Model
#' 
#' @name setup_control_chart
#' @description
#' Creates a control chart for evolutionary model analysis with configurable parameters.
#' This function is used internally by the package and is not meant to be called directly by users.
#' @keywords internal

#' @param data A numeric vector of evolutionary measurements
#' @param type Type of control chart to use ("xbar", "r", "s", "p", "np", "c", "u")
#' @param subgroup_size Size of subgroups for xbar, r, and s charts
#' @param center Center line value (if NULL, will be calculated from data)
#' @param std.dev Standard deviation (if NULL, will be calculated from data)
#' @param subgroup_sds Standard deviations for subgroups (if NULL, will be calculated from data)
#' @param calculated_limits Pre-calculated control limits (if NULL, will be calculated from data)
#'
#' @return A qcc object containing the control chart parameters
#' @export
#'
#' @examples
#' data <- rnorm(100, mean = 10, sd = 1)
#' cc <- setup_control_chart(data, type = "xbar", subgroup_size = 5)
# Suppress CRAN NOTE for non-standard evaluation variables used in dplyr/ggplot2 pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c(
  # Variables used in dplyr pipelines and ggplot2
  ".", "mn", "sd", "n", "time", "start_time", "end_time", "model", "label", 
  "text_x", "text_y", "duration", "proportional_size", "calculated_size", 
  "initial_text_x", "row_number", "trait_name", "mm", "vv", "nn", "tt", 
  "df_subset", "value", "is_nre", "yintercept", "model_type", "t_pos", 
  "adjusted_text_x", "delta_x", "segment", "sigma_level", "x", "y", 
  "xend", "yend", "xmin", "xmax", "ymin", "ymax", "group", "x_pos", 
  "y_pos", "hjust_val", "vjust_val", "angle_val", "color_val", "size_val",
  "coord_cartesian", "scale_x_continuous", "scale_y_continuous"
))

#' Setup a Control Chart Object
#'
#' Sets up a control chart object for evolutionary trait data.
#'
#' @param data A numeric vector of trait measurements.
#' @param type Type of control chart to use ("xbar", "r", "s", "p", "np", "c", "u").
#' @param subgroup_size Size of subgroups for xbar, r, and s charts.
#' @param center Optional center line value.
#' @param std.dev Optional standard deviation value.
#' @param subgroup_sds Optional vector of subgroup standard deviations.
#' @param calculated_limits Optional pre-calculated control limits.
#'
#' @return A qcc object containing the control chart parameters.
#' @export
#'
#' @examples
#' data <- rnorm(100, mean = 10, sd = 1)
#' cc <- setup_control_chart(data, type = "xbar", subgroup_size = 5)
setup_control_chart <- function(data, type = "xbar", subgroup_size = NULL,
                              center = NULL, std.dev = NULL, subgroup_sds = NULL, calculated_limits = NULL) {
  # Input validation
  if (!is.numeric(data)) {
    stop("Data must be numeric")
  }

  # If custom calculated limits are provided, use them directly
  if (!is.null(calculated_limits)) {
      center_line <- mean(data, na.rm = TRUE) # Center line is still mean of data
      # Extract limits from provided structure
      lcl_calc <- calculated_limits[1]
      ucl_calc <- calculated_limits[2]
      
      # Create a dummy qcc object structure for compatibility
      cc <- list(
        type = "xbar",
        data.name = deparse(substitute(data)),
        series = data,
        sizes = subgroup_size,
        center = center_line,
        std.dev = (ucl_calc - lcl_calc) / 6, 
        limits = matrix(c(lcl_calc, ucl_calc), ncol = 2),
        violations = NULL
      )
      class(cc) <- "qcc"
      return(cc)
  }

  # Default qcc behavior for other cases
  if (type %in% c("xbar", "r", "s") && is.null(subgroup_size)) {
    stop("Subgroup size must be specified for xbar, r, or s charts")
  } else if (type != "xbar" && subgroup_size == 1) {
       stop("Subgroup size 1 is only typical for xbar charts (Individuals chart).")
  }

  # Calculate standard deviation if not provided
  if (is.null(std.dev)) {
    if (type == "xbar") {
      std.dev <- sd(data, na.rm = TRUE)
    } else {
      std.dev <- "UWAVE-SD"  # Default method for other chart types
    }
  }

  # Default qcc call
  cc <- qcc(data, type = type, sizes = subgroup_size,
            center = center, std.dev = std.dev,
            plot = FALSE)

  return(cc)
}

# --- Original visualize_results function ---
#' Visualize Control Chart Results with Detected Patterns
#'
#' @param data A numeric vector of evolutionary measurements (either raw trait values or differences)
#' @param cc A qcc object from setup_control_chart
#' @param patterns A list of detected patterns from detect_patterns
#' @param analysis_type Character string indicating the type of analysis ("stasis", "urw", or "grw") - affects labels
#' @param time_values A numeric vector of time values corresponding to the data points (optional, used for x-axis if provided)
#'
#' @return A ggplot object showing the control chart with highlighted patterns
#' @export
#'
#' @examples
#' \dontrun{
#' data <- rnorm(100, mean = 10, sd = 1)
#' time <- 1:100
#' cc <- setup_control_chart(data, type = "xbar", subgroup_size = 5)
#' patterns <- detect_patterns(data, cc, rules = 1:4)
#' plot <- visualize_results(data, cc, patterns, time_values = time, analysis_type = "stasis")
#' }
visualize_results <- function(data, cc, patterns, analysis_type = NULL, time_values = NULL) {
  # Check if we have enough data points for visualization
  if(length(data) < 2) {
    warning("Insufficient data points for visualization (need at least 2 points)")
    return(NULL)
  }

  # Use provided time values if available and match data length, otherwise use index
  if (!is.null(time_values) && length(time_values) == length(data)) {
  plot_data <- data.frame(
      time = time_values, # Use actual time values
    value = data
  )
    x_axis_label <- "Time (yrs)" # Set label to Time (yrs) when using time values
  } else {
    plot_data <- data.frame(
      time = 1:length(data), # Use index if time_values not provided or mismatch length
      value = data
    )
    x_axis_label <- "Data Point Index" # Default label when using index
  }

  # Get control limits directly from cc object
  lcl <- cc$limits[,1]
  ucl <- cc$limits[,2]
  center <- cc$center
  sigma <- (ucl - lcl) / 6  # Calculate sigma from the actual control limits

  # Check if control limits are valid
  if (is.null(lcl) || is.null(ucl) || is.null(center) || is.na(sigma)) {
    warning("Control limits are not valid. Check the cc object.")
    return(NULL)
  }

  # Define sigma levels and labels for the legend
  sigma_levels <- c(
    lcl,  # Use actual LCL
    center - 2*sigma,
    center - sigma,
    center,
    center + sigma,
    center + 2*sigma,
    ucl   # Use actual UCL
  )
  # Use more descriptive labels for the legend
  sigma_legend_labels <- c("-3s (LCL)", "-2s", "-1s", "Center", "+1s", "+2s", "+3s (UCL)")

  # Map legend labels to colors for the sigma lines
  sigma_line_colors <- c(
    "-3s (LCL)" = "red", "-2s" = "orange", "-1s" = "yellow",
    "Center" = "green",
    "+1s" = "yellow", "+2s" = "orange", "+3s (UCL)" = "red"
  )

  # Create data for horizontal lines for the legend
  hline_data <- data.frame(
    yintercept = sigma_levels,
    label = factor(sigma_legend_labels, levels = sigma_legend_labels) # Factor to maintain order
  )

  # Identify points involved in any pattern, handling different return types from detect_patterns
  nre_points_indices_list <- lapply(patterns, function(p) {
      if (is.list(p) && !is.null(p$locations)) {
          return(p$locations)
      } else if (!is.null(p)) {
          return(p)
      } else {
          return(NULL)
      }
  })
  nre_points_indices <- unique(unlist(nre_points_indices_list))
  has_patterns <- length(nre_points_indices) > 0

  # Create a column to indicate if a point is part of an NRE
  plot_data$is_nre <- FALSE
  if (has_patterns) {
    # Ensure indices are within the bounds of plot_data
    valid_nre_indices <- nre_points_indices[nre_points_indices >= 1 & nre_points_indices <= nrow(plot_data)]
    plot_data$is_nre[valid_nre_indices] <- TRUE
  }

  # --- Define all legend elements and colors upfront ---
  all_legend_labels <- c("Trait Data", sigma_legend_labels)
  all_colors <- c("Trait Data" = "black", sigma_line_colors)

  # Add NRE legend entry if patterns exist
  nre_legend_label <- NULL
  if (has_patterns) {
    # Get the numeric rule numbers and sort them
    rule_numbers_str <- stringr::str_extract(names(patterns), "\\d+") # Escaped \d+
    rule_numbers <- sort(as.numeric(rule_numbers_str))

    # Create the triggered rules text based on the number of rules
    if (length(rule_numbers) == 1) {
        triggered_rules_text <- paste("Rule:", rule_numbers)
    } else {
        triggered_rules_text <- paste("Rules:", paste(rule_numbers, collapse = ", "))
    }

    # Construct the final legend label
    nre_legend_label <- paste("NRE Points (", triggered_rules_text, ")", sep = "")

    # Add the NRE label and color to the overall legend elements
    all_legend_labels <- c(all_legend_labels, nre_legend_label)
    all_colors[nre_legend_label] <- "red" # Color for NRE points in legend
  }

  # --- Create base plot ---
  p <- ggplot(plot_data, aes(x = time, y = value)) + # Use 'time' from plot_data
    # Main data line mapped to color for legend
    geom_line(aes(color = "Trait Data"), linewidth = 1) +

    # Plot all points in black first (no legend entry for this layer)
    geom_point(size = 2, color = "black")

  # Add NRE points layer with color mapped to its legend label, ONLY if NREs are present
  if (has_patterns) {
    p <- p + geom_point(data = subset(plot_data, is_nre),
                        aes(x = time, y = value, color = nre_legend_label), # Map color to the NRE legend label
                        size = 3) # Increased size for NRE points
  }

  # Add sigma lines using hline_data and mapping color to legend label
  p <- p + geom_hline(data = hline_data, aes(yintercept = yintercept, color = label), linetype = "solid", linewidth = 1)

  # --- Define colors and breaks for scale_color_manual based on whether NREs are present ---
  # Ensure the order in the scale matches the desired order in the legend (Trait Data first, then sigma, then NRE)
  # Reorder all_legend_labels to put NRE last if it exists
  if (has_patterns) {
      ordered_labels <- c("Trait Data", sigma_legend_labels, nre_legend_label)
  } else {
      ordered_labels <- c("Trait Data", sigma_legend_labels)
  }

  # Need to ensure all_colors has an entry for every label in ordered_labels
  # Fill in missing colors with NA (they won't be plotted but won't cause an error)
  final_colors <- all_colors[ordered_labels]
  names(final_colors) <- ordered_labels

  p <- p + scale_color_manual(values = final_colors,
                              breaks = ordered_labels, # Control legend item order and what appears
                              name = "Legend")

  # --- Apply theme and labels ---
  p <- p + theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12, face = "bold"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),

      # Add a box border around the legend
      legend.box.background = element_rect(color = "black", fill = NA, linewidth = 1),

      # Make axes bold arrows with tick marks (copied from visualize_subgroups)
      axis.line.x = element_line(arrow = arrow(length = unit(0.3, "cm"), type = "closed"), color = "black", linewidth = 1.2),
      axis.line.y = element_line(arrow = arrow(length = unit(0.3, "cm"), type = "closed"), color = "black", linewidth = 1.2),
      axis.ticks = element_line(color = "black", linewidth = 1.2), # Major ticks
      axis.ticks.length = unit(0.15, "cm"), # Length for major ticks

      # Add settings for minor ticks (copied from visualize_subgroups)
      axis.ticks.x.minor = element_line(color = "black", linewidth = 0.8), # Minor x-axis ticks
      axis.ticks.y.minor = element_line(color = "black", linewidth = 0.8), # Minor y-axis ticks
      axis.ticks.length.minor = unit(0.1, "cm") # Length for minor ticks (slightly longer)
    ) +
    labs(
      title = if(analysis_type == "stasis") "X-Bar Control Chart for Trait Data" else if(analysis_type == "urw" || analysis_type == "grw") "Individuals Control Chart for Trait Differences (dX)" else "Control Chart", # Adjusted title for URW/GRW
      x = x_axis_label, # Use dynamic x-axis label
      y = if(analysis_type == "stasis") "Mean Trait Value" else if(analysis_type == "urw" || analysis_type == "grw") "Trait Differences (dX)" else "Value" # Adjusted y-label for URW/GRW
    )

  # Adjust plot limits
   p <- p + coord_cartesian(xlim = c(min(plot_data$time), max(plot_data$time)),
                            ylim = c(min(sigma_levels, min(plot_data$value)), max(sigma_levels, max(plot_data$value))))

  return(p)
}

# --- Extended detect_patterns function (all 8 rules) ---
#' Internal function to detect non-random events in time series data
#' This function is not meant to be called directly by users.
#' It is used internally by analyze_stasis_trait, analyze_urw_trait, and analyze_grw_trait.
#'
#' @param data A numeric vector of evolutionary measurements
#' @param cc A qcc object from setup_control_chart
#' @param rules Which rules to check (1-8)
#' @return A list of detected patterns
detect_patterns <- function(data, cc, rules = 1:8) {
  results <- list()
  center <- cc$center
  ucl <- cc$limits[,2]
  lcl <- cc$limits[,1]
  sigma <- (ucl - lcl) / 6
  n_points <- length(data)

  # Rule 1: Points beyond 3 sigma (control limits)
  if (1 %in% rules) {
    # Requires at least 1 point
    if (n_points >= 1) {
      violations <- which(data > ucl | data < lcl)
      if (length(violations) > 0) {
        results$rule1 <- list(description = "Points beyond 3s", locations = violations)
      }
    }
  }
  # Rule 2: 9 points in a row on same side of centerline
  if (2 %in% rules) {
    # Requires at least 9 points
    if (n_points >= 9) {
      for (i in 1:(n_points-8)) {
        if (all(data[i:(i+8)] > center) || all(data[i:(i+8)] < center)) {
          results$rule2 <- unique(c(results$rule2, i:(i+8)))
        }
      }
    }
  }
  # Rule 3: 6 points in a row, all increasing or decreasing
  if (3 %in% rules) {
    # Requires at least 6 points
    if (n_points >= 6) {
      for (i in 1:(n_points-5)) {
        if (all(diff(data[i:(i+5)]) > 0) || all(diff(data[i:(i+5)]) < 0)) {
          results$rule3 <- unique(c(results$rule3, i:(i+5)))
        }
      }
    }
  }
  # Rule 4: 14 points in a row, alternating up and down
  if (4 %in% rules) {
    # Requires at least 14 points
    if (n_points >= 14) {
      for (i in 1:(n_points-13)) {
        segment <- data[i:(i+13)]
        diffs <- diff(segment)
        if (all(diffs[seq(1,13,2)] > 0) && all(diffs[seq(2,13,2)] < 0) ||
            all(diffs[seq(1,13,2)] < 0) && all(diffs[seq(2,13,2)] > 0)) {
          results$rule4 <- unique(c(results$rule4, i:(i+13)))
        }
      }
    }
  }
  # Rule 5: 2 out of 3 points > 2s from centerline (same side)
  if (5 %in% rules) {
    # Requires at least 3 points
    if (n_points >= 3) {
      for (i in 1:(n_points-2)) {
        vals <- data[i:(i+2)]
        above <- vals > (center + 2*sigma)
        below <- vals < (center - 2*sigma)
        if (sum(above) >= 2) results$rule5 <- unique(c(results$rule5, i-1+which(above))) # Adjust index for absolute position
        if (sum(below) >= 2) results$rule5 <- unique(c(results$rule5, i-1+which(below))) # Adjust index for absolute position
      }
    }
  }
  # Rule 6: 4 out of 5 points > 1s from centerline (same side)
  if (6 %in% rules) {
    # Requires at least 5 points
    if (n_points >= 5) {
      for (i in 1:(n_points-4)) {
        vals <- data[i:(i+4)]
        above <- vals > (center + sigma)
        below <- vals < (center - sigma)
        if (sum(above) >= 4) results$rule6 <- unique(c(results$rule6, i-1+which(above))) # Adjust index
        if (sum(below) >= 4) results$rule6 <- unique(c(results$rule6, i-1+which(below))) # Adjust index
      }
    }
  }
  # Rule 7: 15 points in a row within 1s of centerline (either side)
  if (7 %in% rules) {
    # Requires at least 15 points
    if (n_points >= 15) {
      for (i in 1:(n_points-14)) {
        vals <- data[i:(i+14)]
        within <- abs(vals - center) < sigma
        if (all(within)) results$rule7 <- unique(c(results$rule7, i:(i+14)))
      }
    }
  }
  # Rule 8: 8 points in a row > 1s from centerline (either side) -
  if (8 %in% rules) {
    # Requires at least 8 points
    if (n_points >= 8) {
      for (i in 1:(n_points-7)) {
        vals <- data[i:(i+7)]
        out <- abs(vals - center) > sigma
        if (all(out)) results$rule8 <- unique(c(results$rule8, i:(i+7)))
      }
    }
  }
  return(results)
}

# --- subset_time_series function ---
#' Subset time series data to a specific time range
#'
#' @param file_path Path to the CSV file containing trait data
#' @param start_time Start time for the subset
#' @param end_time End time for the subset
#' @return A data frame containing the subsetted data
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a sample dataset
#' sample_data <- data.frame(
#'   time = 1:100,
#'   mn = cumsum(rnorm(100)),
#'   sd = runif(100, 0.1, 0.5),
#'   n = 10
#' )
#' 
#' # Write to a temporary file
#' temp_file <- tempfile(fileext = ".csv")
#' write.csv(sample_data, temp_file, row.names = FALSE)
#' 
#' # Use the function
#' subset_data <- subset_time_series(temp_file, 20, 80)
#' 
#' # Clean up
#' unlink(temp_file)
#' }
subset_time_series <- function(file_path, start_time, end_time) {
  # Read and prepare data
  df <- read.csv(file_path)
  colnames(df) <- tolower(colnames(df))
  # Only rename if original column names are present
  if (all(c("mm", "vv", "nn", "tt") %in% colnames(df))) {
    df <- df %>% rename(mn = mm, sd = vv, n = nn, time = tt)
  }
  
  # Subset data to specified time range
  subset_df <- df %>% 
    filter(time >= start_time & time <= end_time)
  
  if(nrow(subset_df) == 0) {
    stop("No data points found in the specified time range")
  }
  
  return(subset_df)
}

#' Analyze Trait Data for Stasis Pattern
#'
#' This function analyzes trait data to determine if it follows a stasis pattern,
#' which is characterized by minimal net change over time with variation around a stable mean.
#' It uses control charts and statistical tests to detect non-random evolution and test for normality.
#'
#' @param data A data frame containing the time series data with columns:
#'   \itemize{
#'     \item mn: Mean trait values
#'     \item sd: Standard deviations of trait values
#'     \item time: Time points
#'   }
#' @param plot_file Character string specifying the filename for saving the plot (default: "stasis_plot.png")
#' @param include_nre Logical, whether to include non-random event detection in the analysis (default: TRUE)
#' @param verbose Logical, whether to print detailed analysis summary (default: TRUE)
#'
#' @return A list containing:
#'   \item{nre_present}{Logical indicating if non-random events were detected}
#'   \item{rules_triggered}{Names of control chart rules that were triggered}
#'   \item{normality_p}{P-value from the Chi-squared normality test}
#'   \item{normality_pass}{Logical indicating if the data passes the normality test (p >= 0.05)}
#'   \item{slope_check}{Logical indicating if the average difference between consecutive points is < 0.03}
#'   \item{avg_diff}{Average difference between consecutive time points}
#'   \item{stasis}{Logical indicating if the data is consistent with stasis}
#'   \item{plot_file}{Filename where the plot was saved}
#'   \item{calculated_limits}{Vector containing the calculated LCL and UCL}
#'   \item{adjusted_std_dev_for_limits}{Standard deviation adjusted for autocorrelation}
#'
#' @details
#' The function performs the following analyses:
#' 1. Calculates basic statistics (mean, standard deviation) of the trait values
#' 2. Adjusts for autocorrelation in the time series
#' 3. Sets up control charts using the adjusted standard deviation
#' 4. Tests for normality using a Chi-squared test with autocorrelation adjustment
#' 5. Checks for non-random patterns in the data
#' 6. Determines if the data is consistent with stasis based on the test results
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   data <- data.frame(
#'     mn = rnorm(20, mean = 10, sd = 0.5),
#'     sd = rep(0.2, 20),
#'     time = 1:20
#'   )
#'   result <- analyze_stasis_trait(data)
#' }
#'
#' @importFrom stats acf pchisq pnorm sd
#' @importFrom graphics hist
#' @export
analyze_stasis_trait <- function(data = NULL, plot_file = "stasis_plot.png", 
                               include_nre = TRUE, verbose = TRUE) {
  # Use provided data frame
  df <- data
  
  data_values <- df$mn
  subgroup_sds <- df$sd
  time_values <- df$time  # Store time values

  # Calculate differences between consecutive timepoints for slope check
  diffs <- diff(data_values, na.rm = TRUE)
  avg_diff <- mean(diffs, na.rm = TRUE)
  slope_check <- abs(avg_diff) < 0.03

  # Calculate necessary components for limits and normality test
  avg_mm <- mean(data_values, na.rm = TRUE)
  avg_vv <- mean(subgroup_sds, na.rm = TRUE)

  # Calculate the lag-1 autocorrelation coefficient (r) for the mean trait values
  r <- NA # Initialize r as NA
  if (length(data_values) > 2) { # Need at least 3 points for diff to have 2 points
    autocorr_result <- suppressWarnings(acf(data_values, lag.max = 1, plot = FALSE))
    if (!is.null(autocorr_result$acf) && length(autocorr_result$acf) > 1) {
      r <- autocorr_result$acf[2, 1, 1] # Extract the lag-1 autocorrelation coefficient
    } else {
      warning("Could not calculate lag-1 autocorrelation for Stasis analysis.")
    }
  } else {
    warning("Insufficient data points for autocorrelation calculation in Stasis analysis.")
  }

  # Calculate the Adjusted Standard Deviation for Limits using your formula: (Average of vv) / sqrt(1 - r^2)
  if (1 - r^2 < .Machine$double.eps || is.na(r)) {
      warning("Lag-1 autocorrelation close to 1 or NA, cannot reliably calculate adjusted standard deviation for limits.")
      adjusted_std_dev_for_limits <- avg_vv # Fallback or handle as error
  } else {
      adjusted_std_dev_for_limits <- avg_vv / sqrt(1 - r^2)
  }

  # Calculate Control Limits using your formulas
  center_line <- avg_mm
  lcl_calc <- center_line - 3 * adjusted_std_dev_for_limits
  ucl_calc <- center_line + 3 * adjusted_std_dev_for_limits

  # Store all sigma levels for plotting
  sigma_levels_plot <- c(
      center_line - 3 * adjusted_std_dev_for_limits,
      center_line - 2 * adjusted_std_dev_for_limits,
      center_line - 1 * adjusted_std_dev_for_limits,
      center_line,
      center_line + 1 * adjusted_std_dev_for_limits,
      center_line + 2 * adjusted_std_dev_for_limits,
      center_line + 3 * adjusted_std_dev_for_limits
  )
  sigma_labels_plot <- c(
    paste0("-3s (LCL) (", signif(center_line - 3 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("-2s (", signif(center_line - 2 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("-1s (", signif(center_line - 1 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("Center (", signif(center_line, 3), ")"),
    paste0("+1s (", signif(center_line + 1 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("+2s (", signif(center_line + 2 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("+3s (UCL) (", signif(center_line + 3 * adjusted_std_dev_for_limits, 3), ")")
  )

  # 2. Build X-bar chart - pass calculated limits to setup_control_chart
  cc <- setup_control_chart(data_values, type = "xbar", subgroup_size = 1, calculated_limits = c(lcl_calc, ucl_calc))

  # 3. Detect NREs (all 8 rules) - detect_patterns will use the limits from the cc object
  patterns <- detect_patterns(data_values, cc, rules = 1:8)

  # 4. Normality test (Chi-squared with autocorrelation adjustment)
  # The adjusted standard deviation for the normality test is also (sd(data) / sqrt(1 - r^2))
  # We already calculated 'r' and 'data_sd = sd(data, na.rm = TRUE)' earlier
  data_sd <- sd(data_values, na.rm = TRUE)
  if (1 - r^2 < .Machine$double.eps || is.na(r)) {
      warning("Lag-1 autocorrelation close to 1 or NA, normality adjustment may be unstable.")
      adjusted_sd_normality <- data_sd # Fallback
  } else {
      adjusted_sd_normality <- data_sd / sqrt(1 - r^2)
  }

  # Bin data for histogram using the manual method, using actual data length
  data_range <- range(data_values, na.rm = TRUE)
  n_bins <- ceiling(sqrt(length(data_values))) + 1  # Use actual data length
  bin_width <- (data_range[2] - data_range[1]) / (n_bins - 1)  # n_bins-1 because last bin is "More"
  breaks <- c(seq(data_range[1], data_range[2], by = bin_width), Inf)  # Add Inf for "More" bin
  hist_res <- hist(data_values, plot = FALSE, breaks = breaks)
  observed <- hist_res$counts
  breaks <- hist_res$breaks

  # Calculate expected counts using autocorrelation-corrected STDEV
  expected <- diff(pnorm(breaks, mean(data_values, na.rm = TRUE), adjusted_sd_normality)) * sum(!is.na(data_values))
  # Scale expected counts to match observed total
  expected <- expected * sum(observed) / sum(expected)
  # Avoid zero expected counts
  expected[expected == 0] <- 1e-6

  # Perform Chi-squared test manually with correct degrees of freedom
  chi_squared <- sum((observed - expected)^2 / expected)
  df <- n_bins - 3
  p_value <- pchisq(chi_squared, df = df, lower.tail = FALSE)
  norm_pass <- p_value >= 0.05  # Changed to >= 0.05 for correct interpretation

  # 5. Output plot and summary
  # Pass calculated sigma levels and labels to visualize_results for plotting
  # Pass the original data_values and the time_values
  p <- visualize_results(data_values, cc, patterns, "stasis", time_values = time_values)  # Pass time values
  print(p)

  # Summary
  nre_present <- length(unlist(patterns)) > 0
  stasis <- !nre_present && norm_pass && slope_check
  # If any of the required values are NA, set stasis to FALSE
  if (is.na(nre_present) || is.na(norm_pass) || is.na(slope_check)) stasis <- FALSE

  if (verbose) {
    cat("==== Stasis Analysis Summary ====\n")
    cat("non-random events present:", ifelse(nre_present, "YES", "NO"), "\n")
    cat("Normality test (Chi-squared) p-value:", signif(p_value, 3), "-", ifelse(norm_pass, "PASS", "FAIL"), "\n")
    cat("Slope check (|avg_diff| < 0.03):", ifelse(slope_check, "PASS", "FAIL"), "\n")
    cat("Average difference between consecutive points:", signif(avg_diff, 3), "\n")
    cat("Stasis prediction:", ifelse(stasis, "YES", "NO"), "\n")
    if (nre_present) {
      cat("Rules triggered:\n")
      # Print triggered rules and locations
      for (rule_name in names(patterns)) {
          locations <- if(is.list(patterns[[rule_name]])) patterns[[rule_name]]$locations else patterns[[rule_name]]
          # Ensure locations is not NULL or empty before pasting
          if (!is.null(locations) && length(locations) > 0) {
              cat(" -", rule_name, ":", paste(locations, collapse = ", "), "\n")
          }
      }
    }
    cat("=================================\n")
  }

  # Return summary as a list
  return(list(
    nre_present = nre_present,
    rules_triggered = names(patterns),
    normality_p = p_value,
    normality_pass = norm_pass,
    slope_check = slope_check,
    avg_diff = avg_diff,
    stasis = stasis,
    plot_file = plot_file,
    calculated_limits = c(lcl_calc, ucl_calc), # Include calculated limits in return
    adjusted_std_dev_for_limits = adjusted_std_dev_for_limits # Include adjusted SD in return
  ))
}

#' Analyze Trait Data for Unbiased Random Walk (URW) Pattern
#'
#' This function analyzes trait data to determine if it follows an unbiased random walk (URW) pattern,
#' which is characterized by random changes in trait values over time with no consistent direction.
#' It uses control charts and statistical tests to detect non-random evolution and test for normality.
#'
#' @param data A data frame containing the time series data with columns:
#'   \itemize{
#'     \item mn: Mean trait values
#'     \item time: Time points
#'   }
#' @param plot_file Character string specifying the filename for saving the plot (default: "urw_plot.png")
#' @param include_nre Logical, whether to include non-random event detection in the analysis (default: TRUE)
#' @param verbose Logical, whether to print detailed analysis summary (default: TRUE)
#'
#' @return A list containing:
#'   \item{nre_present}{Logical indicating if non-random events were detected}
#'   \item{normality_p}{P-value from the normality test on differences}
#'   \item{normality_pass}{Logical indicating if the differences pass the normality test (p >= 0.05)}
#'   \item{slope_check}{Logical indicating if the average difference between consecutive points is < 0.03}
#'   \item{average_difference}{Average difference between consecutive time points}
#'   \item{urw_prediction}{Logical indicating if the data is consistent with URW}
#'   \item{rules_triggered}{List of control chart rules that were triggered}
#'   \item{calculated_limits}{Vector containing the calculated LCL and UCL}
#'
#' @details
#' The function performs the following analyses:
#' 1. Calculates first differences between consecutive time points
#' 2. Adjusts for autocorrelation in the differences
#' 3. Sets up control charts using the adjusted standard deviation
#' 4. Tests for normality of the differences using a Chi-squared test
#' 5. Checks for non-random patterns in the differences
#' 6. Determines if the data is consistent with URW based on the test results
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   data <- data.frame(
#'     mn = cumsum(rnorm(20, mean = 0, sd = 0.5)),
#'     time = 1:20
#'   )
#'   result <- analyze_urw_trait(data)
#' }
#'
#' @importFrom stats acf pchisq pnorm sd
#' @importFrom graphics hist
#' @export
analyze_urw_trait <- function(data = NULL, plot_file = "urw_plot.png", 
                              include_nre = TRUE, verbose = TRUE) {
  
  # Use provided data frame
  df <- data

  # Calculate differences between consecutive timepoints
  delta_x <- diff(df$mn, na.rm = TRUE)
  time_values_delta <- df$time[-1]  # Time values for differences exclude the first time point

  # Calculate the lag-1 autocorrelation coefficient (r) for the differences
  r <- NA
  if (length(delta_x) > 2) {
    autocorr_result <- suppressWarnings(acf(delta_x, lag.max = 1, plot = FALSE))
    if (!is.null(autocorr_result$acf) && length(autocorr_result$acf) > 1) {
      r <- autocorr_result$acf[2, 1, 1]
    }
  }

  # Calculate second differences (differences of differences)
  second_diffs <- diff(delta_x)
  avg_second_diffs <- mean(abs(second_diffs), na.rm = TRUE)

  # Calculate Adjusted Standard Deviation for Limits using the formula:
  # (Avg. of the difference between consecutive points) / (1.128/(sqrt(1-r^2)))
  if (is.na(r) || 1 - r^2 < .Machine$double.eps) {
    warning("Lag-1 autocorrelation close to 1 or NA, cannot reliably calculate adjusted standard deviation for limits.")
    adjusted_std_dev_for_limits <- avg_second_diffs / 1.128 # Fallback
  } else {
    adjusted_std_dev_for_limits <- avg_second_diffs / (1.128/sqrt(1 - r^2))
  }

  # Calculate Control Limits using the formula:
  # Center line = mean of differences
  # UCL = center_line + (3*avg_second_diffs)/(1.128/(sqrt(1-r^2)))
  center_line <- mean(delta_x, na.rm = TRUE)
  lcl_calc <- center_line - 3 * adjusted_std_dev_for_limits
  ucl_calc <- center_line + 3 * adjusted_std_dev_for_limits

  # Store all sigma levels for plotting
  sigma_levels_plot <- c(
    center_line - 3 * adjusted_std_dev_for_limits,
    center_line - 2 * adjusted_std_dev_for_limits,
    center_line - 1 * adjusted_std_dev_for_limits,
    center_line,
    center_line + 1 * adjusted_std_dev_for_limits,
    center_line + 2 * adjusted_std_dev_for_limits,
    center_line + 3 * adjusted_std_dev_for_limits
  )
  sigma_labels_plot <- c(
    paste0("-3s (LCL) (", signif(center_line - 3 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("-2s (", signif(center_line - 2 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("-1s (", signif(center_line - 1 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("Center (", signif(center_line, 3), ")"),
    paste0("+1s (", signif(center_line + 1 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("+2s (", signif(center_line + 2 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("+3s (UCL) (", signif(center_line + 3 * adjusted_std_dev_for_limits, 3), ")")
  )

  # Create control chart for differences
  cc_plot <- list(
    type = "i",
    data.name = "delta_x",
    series = delta_x,
    sizes = 1,
    center = center_line,
    std.dev = adjusted_std_dev_for_limits,
    limits = matrix(c(lcl_calc, ucl_calc), ncol = 2),
    violations = NULL
  )
  class(cc_plot) <- "qcc"

  # Calculate slope check
  avg_diff <- mean(delta_x, na.rm = TRUE)
  slope_check <- abs(avg_diff) < 0.03

  # Calculate normality test
  data_sd <- sd(delta_x, na.rm = TRUE)
  adjusted_sd_normality <- data_sd / sqrt(1 - r^2)
  
  # Bin data for histogram using the manual method
  data_range <- range(delta_x, na.rm = TRUE)
  n_bins <- ceiling(sqrt(length(delta_x))) + 1
  bin_width <- (data_range[2] - data_range[1]) / (n_bins - 1)
  breaks <- c(seq(data_range[1], data_range[2], by = bin_width), Inf)
  hist_res <- hist(delta_x, plot = FALSE, breaks = breaks)
  observed <- hist_res$counts
  breaks <- hist_res$breaks

  # Calculate expected counts using autocorrelation-corrected STDEV
  expected <- diff(pnorm(breaks, mean(delta_x, na.rm = TRUE), adjusted_sd_normality)) * sum(!is.na(delta_x))
  expected <- expected * sum(observed) / sum(expected)
  expected[expected == 0] <- 1e-6

  # Perform Chi-squared test
  chi_squared <- sum((observed - expected)^2 / expected)
  df <- n_bins - 3
  p_value <- pchisq(chi_squared, df = df, lower.tail = FALSE)
  norm_pass <- p_value >= 0.05

  # Detect patterns
  patterns <- detect_patterns(delta_x, cc_plot, rules = 1:8)

  # Create plot
  p <- visualize_results(delta_x, cc_plot, patterns, "urw", time_values = time_values_delta)
  print(p)

  # Summary
  nre_present <- length(unlist(patterns)) > 0
  urw_prediction <- !nre_present && norm_pass && slope_check

  if (verbose) {
    cat("\n==== URW Analysis Summary ====\n")
    cat("non-random events present:", ifelse(nre_present, "YES", "NO"), "\n")
    cat("Normality test (delta_x) p-value:", signif(p_value, 3), "-", ifelse(norm_pass, "PASS", "FAIL"), "\n")
    cat("Slope check (|avg_diff| < 0.03):", ifelse(slope_check, "PASS", "FAIL"), "\n")
    cat("Average difference between consecutive points:", signif(avg_diff, 3), "\n")
    cat("URW prediction:", ifelse(urw_prediction, "YES", "NO"), "\n")
    if (nre_present) {
      cat("Rules triggered:", paste(names(patterns), collapse = ", "), "\n")
    }
    cat("=================================\n")
  }

  return(list(
    nre_present = nre_present,
    normality_p = p_value,
    normality_pass = norm_pass,
    slope_check = slope_check,
    average_difference = avg_diff,
    urw_prediction = urw_prediction,
    rules_triggered = patterns,
    calculated_limits = c(lcl_calc, ucl_calc)
  ))

  cc_plot <- list(
    type = "i",
        data.name = "delta_x",
    series = delta_x,
    sizes = 1,
    center = center_line,
    std.dev = adjusted_sd_normality,
    limits = matrix(c(lcl_calc, ucl_calc), ncol = 2),
    violations = NULL
  )
  class(cc_plot) <- "qcc"

  # Detect patterns
  patterns <- detect_patterns(delta_x, cc_plot, rules = 1:8)

  # Create plot
  p <- visualize_results(delta_x, cc_plot, patterns, "urw", time_values = time_values_delta) # Pass delta_x and the modified cc_plot and time_values_delta
  print(p)

  # Summary
  nre_present <- length(unlist(patterns)) > 0
  urw_prediction <- !nre_present && norm_pass && slope_check

  if (verbose) {
    cat("\n==== URW Analysis Summary ====\n")
    cat("Non-random events present:", ifelse(nre_present, "YES", "NO"), "\n")
    cat("Normality test (delta_x) p-value:", signif(p_value, 3), "-", ifelse(norm_pass, "PASS", "FAIL"), "\n")
    cat("Slope check (|avg_diff| < 0.03):", ifelse(slope_check, "PASS", "FAIL"), "\n")
    cat("Average difference between consecutive points:", signif(avg_diff, 3), "\n")
    cat("URW prediction:", ifelse(urw_prediction, "YES", "NO"), "\n")
     if (nre_present) {
      cat("Rules triggered:", paste(names(patterns), collapse = ", "), "\n")
    }
    cat("=================================\n")
  }

  return(list(
    nre_present = nre_present,
    normality_p = p_value,
    normality_pass = norm_pass,
    slope_check = slope_check,
    average_difference = avg_diff,
    urw_prediction = urw_prediction,
    rules_triggered = patterns,
    calculated_limits = c(lcl_calc, ucl_calc)
  ))
}

#' Analyze Trait Data for General Random Walk (GRW) Pattern
#'
#' This function analyzes trait data to determine if it follows a general random walk (GRW) pattern,
#' which is characterized by directional changes in trait values over time with a significant slope.
#' It uses control charts and statistical tests to detect non-random evolution.
#'
#' @param data A data frame containing the time series data with columns:
#'   \itemize{
#'     \item mn: Mean trait values
#'     \item time: Time points
#'   }
#' @param plot_file Character string specifying the filename for saving the plot (default: "grw_plot.png")
#' @param include_nre Logical, whether to include non-random event detection in the analysis (default: TRUE)
#' @param verbose Logical, whether to print detailed analysis summary (default: TRUE)
#'
#' @return A list containing:
#'   \item{nre_present}{Logical indicating if non-random events were detected}
#'   \item{normality_test_p_value}{P-value from the normality test on differences (not used for GRW prediction)}
#'   \item{normality_pass}{Logical indicating if the differences pass the normality test (not used for GRW prediction)}
#'   \item{slope_check}{Logical indicating if the average difference between consecutive points is > 0.03}
#'   \item{average_difference}{Average difference between consecutive time points}
#'   \item{grw_prediction}{Logical indicating if the data is consistent with GRW}
#'   \item{rules_triggered}{List of control chart rules that were triggered}
#'
#' @details
#' The function performs the following analyses:
#' 1. Calculates first differences between consecutive time points
#' 2. Adjusts for autocorrelation in the differences
#' 3. Sets up control charts using the adjusted standard deviation
#' 4. Checks for non-random patterns in the differences
#' 5. Determines if the data is consistent with GRW based on the presence of a significant slope
#'    and absence of non-random patterns
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   data <- data.frame(
#'     mn = cumsum(rnorm(20, mean = 0.1, sd = 0.5)),
#'     time = 1:20
#'   )
#'   result <- analyze_grw_trait(data)
#' }
#'
#' @importFrom stats acf sd
#' @title Analyze Trait Data for General Random Walk (GRW) Pattern
#'
#' @description This function analyzes trait data to determine if it follows a general random walk (GRW) pattern,
#' which is characterized by directional changes in trait values over time with some random variation.
#'
#' @param data A data frame containing the time series data with columns 'mn' (mean), 'sd' (standard deviation),
#' 'n' (sample size), and 'time' (time points). If NULL, the function will prompt for a file input.
#' @param plot_file Character string specifying the filename for saving the plot. If NULL, no plot is saved.
#' @param include_nre Logical indicating whether to include non-random evolution (NRE) analysis.
#' @param verbose Logical indicating whether to print detailed output.
#'
#' @return A list containing the analysis results including:
#' \item{model}{The model type ("grw")}
#' \item{parameters}{List of model parameters}
#' \item{stats}{Summary statistics}
#' \item{patterns}{Detected patterns in the data}
#' \item{plot}{The generated ggplot object}
#' \item{normality_test}{Results of the normality test}
#' \item{slope_check}{Result of the slope check}
#' \item{conclusion}{The conclusion about the model fit}
#'
#' @examples
#' \dontrun{
#'   # Example with built-in data
#'   data <- data.frame(
#'     mn = cumsum(rnorm(20, 0.1, 0.5)),
#'     sd = rep(0.5, 20),
#'     n = rep(10, 20),
#'     time = 1:20
#'   )
#'   result <- analyze_grw_trait(data)
#'   print(result$conclusion)
#' }
#'
#' @export
analyze_grw_trait <- function(data = NULL, plot_file = "grw_plot.png", 
                               include_nre = TRUE, verbose = TRUE) {
   # Use provided data frame
  df <- data

  data_values <- df$mn # Trait values
  time_values <- df$time # Time values

  # Calculate differences between consecutive timepoints for slope check
  diffs <- diff(data_values, na.rm = TRUE)

  # Calculate successive differences (deltaX)
  delta_x <- diff(data_values, na.rm = TRUE)
  time_values_delta <- time_values[-1]  # Time values for differences

  # Calculate absolute successive differences (|deltaX|)
  abs_delta_x <- abs(delta_x)

  # Calculate mean and standard deviation of absolute successive differences
  mean_abs_delta_x <- mean(abs_delta_x, na.rm = TRUE)
  sd_abs_delta_x <- sd(abs_delta_x, na.rm = TRUE)

  # Calculate the lag-1 autocorrelation coefficient (r) for the differences (delta_x)
  r <- NA # Initialize r as NA
  if (length(delta_x) >= 2) { # Need at least 2 points in delta_x
    autocorr_result <- suppressWarnings(acf(delta_x, lag.max = 1, plot = FALSE))
    if (!is.null(autocorr_result$acf) && length(autocorr_result$acf) > 1) {
      r <- autocorr_result$acf[2, 1, 1] # Extract the lag-1 autocorrelation coefficient
    } else {
      warning("Could not calculate lag-1 autocorrelation for GRW analysis (delta_x).")
    }
  } else {
    warning("Insufficient data points for autocorrelation calculation on delta_x in GRW analysis.")
  }

  # Calculate second differences (differences of differences)
  second_diffs <- diff(delta_x)
  avg_second_diffs <- mean(abs(second_diffs), na.rm = TRUE)

  # Calculate Adjusted Standard Deviation for Limits using the formula:
  # (Avg. of the difference between consecutive points) / (1.128/(sqrt(1-r^2)))
  if (is.na(r) || 1 - r^2 < .Machine$double.eps) {
    warning("Lag-1 autocorrelation close to 1 or NA, cannot reliably calculate adjusted standard deviation for limits.")
    adjusted_std_dev_for_limits <- avg_second_diffs / 1.128 # Fallback
  } else {
    adjusted_std_dev_for_limits <- avg_second_diffs / (1.128/sqrt(1 - r^2))
  }

  # Calculate Control Limits using the formula:
  # Center line = mean of differences
  # UCL = center_line + (3*avg_second_diffs)/(1.128/(sqrt(1-r^2)))
  center_line <- mean(delta_x, na.rm = TRUE)
  lcl_calc <- center_line - 3 * adjusted_std_dev_for_limits
  ucl_calc <- center_line + 3 * adjusted_std_dev_for_limits

  # Store all sigma levels for plotting
  sigma_levels_plot <- c(
      center_line - 3 * adjusted_std_dev_for_limits,
      center_line - 2 * adjusted_std_dev_for_limits,
      center_line - 1 * adjusted_std_dev_for_limits,
      center_line,
      center_line + 1 * adjusted_std_dev_for_limits,
      center_line + 2 * adjusted_std_dev_for_limits,
      center_line + 3 * adjusted_std_dev_for_limits
  )
  sigma_labels_plot <- c(
    paste0("-3s (LCL) (", signif(center_line - 3 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("-2s (", signif(center_line - 2 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("-1s (", signif(center_line - 1 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("Center (", signif(center_line, 3), ")"),
    paste0("+1s (", signif(center_line + 1 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("+2s (", signif(center_line + 2 * adjusted_std_dev_for_limits, 3), ")"),
    paste0("+3s (UCL) (", signif(center_line + 3 * adjusted_std_dev_for_limits, 3), ")")
  )

  # 2. Build Individuals chart for differences - pass calculated limits to setup_control_chart
  cc_plot <- list(
        type = "i", # Individuals chart type
        data.name = "delta_x",
        series = delta_x, # Pass the differences
        sizes = 1, # Subgroup size is 1 for Individuals chart
        center = center_line, # Center line is mean of differences
        std.dev = adjusted_std_dev_for_limits, # Use adjusted std dev for limits
        limits = matrix(c(lcl_calc, ucl_calc), ncol = 2), # Use calculated limits
        violations = NULL # Not using violations from qcc object for plotting
  )
  class(cc_plot) <- "qcc"

  # 3. Detect NREs using Rules 1-4 (for Individuals chart)
  # Use delta_x for pattern detection and the manually created cc_plot object
  patterns <- detect_patterns(delta_x, cc_plot, rules = 1:4)

  # 4. Normality test on differences (delta_x) with autocorrelation adjustment
  # This is not required for GRW, so we can skip this step or report it as not applicable.
  # For now, we will just not include it in the GRW prediction criteria.
  norm_pass <- TRUE # Not a criterion for GRW
  norm_p_value <- NA # Not applicable

  # 5. Slope check on time vs. trait value
  # We use the original trait data and time values for this slope check
  slope_check <- FALSE
  avg_diff <- NA # Initialize average difference
  if (length(diffs) > 0) {
     avg_diff <- mean(diffs, na.rm = TRUE)
     slope_check <- abs(avg_diff) > 0.03 # GRW requires significant slope
  }

  # 6. Output plot and summary
   # Pass calculated sigma levels and labels to visualize_results for plotting
  # NOTE: visualize_results for GRW should plot delta_x, not the original data
  # Need to create a dummy cc object for visualize_results with the correct limits for delta_x
  cc_plot <- list(
        type = "i", # Individuals chart type
        data.name = "delta_x",
        series = delta_x, # Pass the differences
        sizes = 1, # Subgroup size is 1 for Individuals chart
        center = center_line, # Center line is mean of differences
        std.dev = adjusted_std_dev_for_limits, # Use adjusted std dev for limits
        limits = matrix(c(lcl_calc, ucl_calc), ncol = 2), # Use calculated limits
        violations = NULL # Not using violations from qcc object for plotting
  )
  class(cc_plot) <- "qcc"

  p <- visualize_results(delta_x, cc_plot, patterns, "grw", time_values = time_values_delta) # Pass delta_x and the modified cc_plot and time_values_delta
  print(p)

  # Summary
  nre_present <- length(unlist(patterns)) > 0
  grw_prediction <- !nre_present && slope_check # GRW doesn't require normality
  # If any of the required values are NA, set grw_prediction to FALSE
  if (is.na(nre_present) || is.null(slope_check)) grw_prediction <- FALSE

  if (verbose) {
    cat("\n==== GRW Analysis Summary ====\n")
    cat("Non-random events present:", ifelse(nre_present, "YES", "NO"), "\n")
    cat("Normality test (delta_x) p-value:", signif(norm_p_value, 3), "-", ifelse(norm_pass, "PASS", "FAIL"), "\n")
    cat("Slope check (time vs. mn):", ifelse(slope_check, "PASS", "FAIL"), "\n")
    cat("Average difference between consecutive points:", signif(avg_diff, 3), "\n")
    cat("GRW prediction:", ifelse(grw_prediction, "YES", "NO"), "\n")
     if (nre_present) {
      cat("Rules triggered:", paste(names(patterns), collapse = ", "), "\n")
    }
    cat("=================================\n")
  }

  list(
    nre_present = nre_present,
    normality_test_p_value = norm_p_value,
    normality_pass = norm_pass,
    slope_check = slope_check,
    average_difference = avg_diff, # Include average difference in output
    grw_prediction = grw_prediction,
    rules_triggered = patterns # Include detected patterns in return list
  )
}

# --- predict_evolutionary_model function (hierarchical model selection, clean table output with evaluation order) ---
#' Predict Evolutionary Model for a given dataset
#'
#' Hierarchically predicts the best fitting evolutionary model (Stasis, URW, GRW)
#'
#' @param data Data frame containing the time series data with required columns:
#'   \itemize{
#'     \item mn: Mean values
#'     \item sd: Standard deviations
#'     \item n: Sample sizes
#'     \item time: Time points
#'   }
#' @param evaluation_order Character vector specifying the order of model evaluation.
#'   Must be a permutation of c("Stasis", "URW", "GRW"). Default is c("Stasis", "URW", "GRW").
#' @param include_nre Logical, whether to include non-random events in the model criteria.
#'   If TRUE (default), models with non-random events are penalized.
#' @param verbose Logical, whether to print detailed analysis summaries (default: TRUE).
#' @param trait_name Optional character string specifying the trait name (used for plot titles and file naming).
#' @param start_time Optional numeric start time of the analyzed range (for file naming).
#' @param end_time Optional numeric end time of the analyzed range (for file naming).
#'
#' @return A list with the following components:
#'   \item{best_model}{Character string indicating the best fitting model ("Stasis", "URW", or "GRW")}
#'   \item{results}{List containing detailed results for each tested model}
#'   \item{parameters}{List of input parameters used for the analysis}
#'   \item{data}{The input data used for analysis}
#'
#' @details
#' The function performs the following steps:
#' 1. Validates input data and parameters
#' 2. Performs control chart analysis for each model in the specified evaluation order
#' 3. Applies model-specific criteria to determine the best fit
#' 4. Returns results including diagnostic plots and statistical summaries
#'
#' @examples
#' \dontrun{
#' # Basic usage with default settings
#' data <- data.frame(
#'   time = 1:20,
#'   mn = rnorm(20, mean = 10, sd = 0.5),
#'   sd = rep(0.2, 20),
#'   n = rep(10, 20)
#' )
#' result <- predict_evolutionary_model(data)
#'
#' # Custom evaluation order
#' result <- predict_evolutionary_model(data, evaluation_order = c("GRW", "URW", "Stasis"))
#' }
#'
#' @seealso \code{\link{analyze_entire_series}} for analyzing multiple time series segments
#' @export
predict_evolutionary_model <- function(data = NULL, evaluation_order = c("Stasis", "URW", "GRW"), include_nre = TRUE, verbose = TRUE, 
                                      trait_name = NULL, start_time = NULL, end_time = NULL) {
  # Validate evaluation order
  if (!all(evaluation_order %in% c("Stasis", "URW", "GRW"))) {
    stop("Invalid model in evaluation_order. Must be one or more of: Stasis, URW, GRW")
  }
  if (length(evaluation_order) > 3) {
    stop("evaluation_order cannot contain more than 3 models")
  }
  
  # Initialize variables using evaluation_order
  valid_models <- evaluation_order
  best_model <- NULL
  best_result <- NULL

  # Delete control chart files if they exist and NRE inclusion is FALSE
  if (!include_nre) {
    if (file.exists("XBAR_Chart.png")) file.remove("XBAR_Chart.png")
    if (file.exists("Individuals_Chart.png")) file.remove("Individuals_Chart.png")
  }

  # Initialize results list
  results <- list()
  stasis_plot_obj <- NULL
  urw_plot_obj <- NULL

  # Use provided data frame
  df <- data

  # Generate control charts if NRE inclusion is TRUE and verbose is TRUE
  if (include_nre && verbose) {
    # XBAR Chart (similar to stasis plot)
    data_values <- df$mn
    subgroup_sds <- df$sd
    time_values <- df$time # Get time values for plotting
    
    # Only proceed with visualization if we have enough points
    if(length(data_values) >= 2) {
      avg_mm <- mean(data_values, na.rm = TRUE)
      avg_vv <- mean(subgroup_sds, na.rm = TRUE)
      
      # Calculate autocorrelation only if we have enough points
      r <- NA
      if (length(data_values) > 2) {
        autocorr_result <- suppressWarnings(acf(data_values, lag.max = 1, plot = FALSE))
        if (!is.null(autocorr_result$acf) && length(autocorr_result$acf) > 1) {
          r <- autocorr_result$acf[2, 1, 1]
        }
      }
      
      # Use fallback if autocorrelation couldn't be calculated
      if (is.na(r) || 1 - r^2 < .Machine$double.eps) {
        adjusted_std_dev_for_limits <- avg_vv
      } else {
        adjusted_std_dev_for_limits <- avg_vv / sqrt(1 - r^2)
      }
      
      center_line <- avg_mm
      lcl_calc <- center_line - 3 * adjusted_std_dev_for_limits
      ucl_calc <- center_line + 3 * adjusted_std_dev_for_limits
      
      cc <- setup_control_chart(data_values, type = "xbar", subgroup_size = 1, calculated_limits = c(lcl_calc, ucl_calc))
      patterns <- detect_patterns(data_values, cc, rules = 1:8)
      
      # Only create and save plot if we have enough points
      stasis_plot_obj <- visualize_results(data_values, cc, patterns, "stasis", time_values = time_values) # Pass time_values here
      if(!is.null(stasis_plot_obj)) {
        # Construct filename based on provided parameters
        if (!is.null(trait_name) && !is.null(start_time) && !is.null(end_time)) {
          xbar_filename <- paste0("XBAR_Chart_", trait_name, "_", start_time, "_to_", end_time, ".png")
        } else if (verbose) { # Only save default if verbose is TRUE and no specific name provided
        # ggsave("stasis_plot.png", stasis_plot_obj, width = 12, height = 6) # Removed redundant save
           # ggsave("XBAR_Chart.png", stasis_plot_obj, width = 12, height = 6) # Removed default save from here
        }
        ggsave(xbar_filename, stasis_plot_obj, width = 12, height = 6); # Save with constructed filename
      }
    }
    
    # Individuals Chart (similar to URW plot)
    data_diff <- diff(df$mn)
    time_values_delta <- df$time[-1] # Get time values for differences
    
    # Only proceed with visualization if we have enough points
    if(length(data_diff) >= 2) {
      # Calculate second differences (differences of differences)
      second_diffs <- diff(data_diff)
      avg_second_diffs <- mean(abs(second_diffs), na.rm = TRUE)
      
      # Calculate the lag-1 autocorrelation coefficient (r)
      r <- NA
      if (length(data_diff) > 2) {
        autocorr_result <- suppressWarnings(acf(data_diff, lag.max = 1, plot = FALSE))
        if (!is.null(autocorr_result$acf) && length(autocorr_result$acf) > 1) {
          r <- autocorr_result$acf[2, 1, 1]
        }
      }
      
      # Calculate the Adjusted Standard Deviation for Limits
      if (is.na(r) || 1 - r^2 < .Machine$double.eps) {
        adjusted_std_dev_for_limits <- avg_second_diffs / 1.128
      } else {
        adjusted_std_dev_for_limits <- (avg_second_diffs / sqrt(1 - r^2)) / 1.128
      }
      
      center_line <- mean(data_diff, na.rm = TRUE);
      lcl_calc <- center_line - 3 * adjusted_std_dev_for_limits;
      ucl_calc <- center_line + 3 * adjusted_std_dev_for_limits;
      
      cc <- setup_control_chart(data_diff, type = "xbar", subgroup_size = 1, calculated_limits = c(lcl_calc, ucl_calc));
      patterns <- detect_patterns(data_diff, cc, rules = 1:8);
      
      # Only create and save plot if we have enough points
      urw_plot_obj <- visualize_results(data_diff, cc, patterns, "urw", time_values = time_values_delta); # Pass time_values_delta here
      if(!is.null(urw_plot_obj)) {
        # Construct filename based on provided parameters
         if (!is.null(trait_name) && !is.null(start_time) && !is.null(end_time)) {
          individuals_filename <- paste0("Individuals_Chart_", trait_name, "_", start_time, "_to_", end_time, ".png")
        } else if (verbose) { # Only save default if verbose is TRUE and no specific name provided
        # ggsave("urw_plot.png", urw_plot_obj, width = 12, height = 6) # Removed redundant save
           # ggsave("Individuals_Chart.png", urw_plot_obj, width = 12, height = 6); # Removed default save from here
        }
        ggsave(individuals_filename, urw_plot_obj, width = 12, height = 6); # Save with constructed filename
      }
    }
  }
  
  # Run analyses based on evaluation order
  for (model in valid_models) {
    if (model == "Stasis") {
      if (verbose) cat("\n=== Testing for Stasis ===\n")
      results$stasis <- analyze_stasis_trait(df, 
                                          plot_file = if(include_nre && verbose) "stasis_plot.png" else NULL,
                                          include_nre = include_nre, verbose = verbose)
    } else if (model == "URW") {
      if (verbose) cat("\n=== Testing for Unbiased Random Walk ===\n")
      results$urw <- analyze_urw_trait(df, 
                                     plot_file = if(include_nre && verbose) "urw_plot.png" else NULL,
                                     include_nre = include_nre, verbose = verbose)
    } else if (model == "GRW") {
      if (verbose) cat("\n=== Testing for General Random Walk ===\n")
      results$grw <- analyze_grw_trait(df, 
                                     plot_file = if(include_nre && verbose) "grw_plot.png" else NULL,
                                     include_nre = include_nre, verbose = verbose)
    }
  }
  
  # Create a table of results with evaluation order
  results_table <- data.frame(
    Evaluation_Order = seq_along(valid_models),
    Model = valid_models,
    Norm_Test_Value = sapply(valid_models, function(m) {
      if (m == "Stasis" && !is.null(results$stasis$normality_p)) results$stasis$normality_p
      else if (m == "URW" && !is.null(results$urw$normality_p)) results$urw$normality_p
      else if (m == "GRW") NA # GRW doesn't have normality test
      else NA # Default to NA if result is missing
    }),
    NRE_Count = sapply(valid_models, function(m) {
      if (m == "Stasis" && !is.null(results$stasis$rules_triggered)) length(unlist(results$stasis$rules_triggered))
      else if (m == "URW" && !is.null(results$urw$rules_triggered)) length(unlist(results$urw$rules_triggered))
      else if (m == "GRW" && !is.null(results$grw$rules_triggered)) length(unlist(results$grw$rules_triggered))
      else 0 # Default to 0 if result is missing
    }),
    Slope = sapply(valid_models, function(m) {
      if (m == "Stasis" && !is.null(results$stasis$avg_diff)) results$stasis$avg_diff
      else if (m == "URW" && !is.null(results$urw$average_difference)) results$urw$average_difference # Use average_difference from URW/GRW
      else if (m == "GRW" && !is.null(results$grw$average_difference)) results$grw$average_difference # Use average_difference from URW/GRW
      else NA # Default to NA if result is missing
    }),
    Pass_Fail = sapply(valid_models, function(m) {
      if (m == "Stasis" && !is.null(results$stasis)) ifelse(results$stasis$stasis, "PASS", "FAIL")
      else if (m == "URW" && !is.null(results$urw)) ifelse(results$urw$urw_prediction, "PASS", "FAIL") # Use urw_prediction
      else if (m == "GRW" && !is.null(results$grw)) ifelse(results$grw$grw_prediction, "PASS", "FAIL") # Use grw_prediction
      else "FAIL" # Default to FAIL if result is missing
    })
  )
  
  # Print a clear, prominent results table if verbose is TRUE
  if (verbose) {
    cat("\n")
    cat("===================================================\n")
    cat("               MODEL PREDICTION RESULTS             \n")
    cat("===================================================\n")
    cat("Parameters:\n")
    cat("- NRE Inclusion:", ifelse(include_nre, "Yes", "No"), "\n")
    cat("- Evaluation Order:", paste(valid_models, collapse = " -> "), "\n")
    cat("---------------------------------------------------\n")
    print(results_table, row.names = FALSE)
    cat("---------------------------------------------------\n")
    
    # Determine the best model based on evaluation order and NRE inclusion
    best_model <- "none"
    reason <- "No model passed all criteria"
    
    for (model in valid_models) {
      if (model == "Stasis") {
        # For Stasis, the 'stasis' boolean already incorporates the NRE check (if include_nre is TRUE)
        if (!is.null(results$stasis) && results$stasis$stasis) {
          best_model <- "stasis"
          reason <- "Data shows no non-random events, passes normality test, and has minimal slope"
          break
        }
      } else if (model == "URW") {
        # For URW, the 'urw_prediction' boolean already incorporates the NRE check (if include_nre is TRUE)
        if (!is.null(results$urw) && results$urw$urw_prediction) {
          best_model <- "urw"
          reason <- "Differences between consecutive timepoints show no non-random events, pass normality test, and have minimal slope"
          break
        }
      } else if (model == "GRW") {
        # For GRW, the 'grw_prediction' boolean already incorporates the NRE check (if include_nre is TRUE)
        if (!is.null(results$grw) && results$grw$grw_prediction) {
          best_model <- "grw"
          reason <- "Differences between consecutive timepoints show no non-random events and have significant slope"
          break
        }
      }
    }
    
    cat("Best fitting model:", toupper(best_model), "\n")
    cat("Reason:", reason, "\n")
    cat("===================================================\n\n")
  }
  
  # Determine the best model to return even if not verbose
   best_model_return <- "none"
   reason_return <- "No model passed all criteria"
   
   for (model in valid_models) {
      if (model == "Stasis") {
        # For Stasis, the 'stasis' boolean already incorporates the NRE check (if include_nre is TRUE)
        if (!is.null(results$stasis) && results$stasis$stasis) {
          best_model_return <- "stasis"
          reason_return <- "Data shows no non-random events, passes normality test, and has minimal slope"
          break
        }
      } else if (model == "URW") {
        # For URW, the 'urw_prediction' boolean already incorporates the NRE check (if include_nre is TRUE)
        if (!is.null(results$urw) && results$urw$urw_prediction) {
          best_model_return <- "urw"
          reason_return <- "Differences between consecutive timepoints show no non-random events, pass normality test, and have minimal slope"
          break
        }
      } else if (model == "GRW") {
        # For GRW, the 'grw_prediction' boolean already incorporates the NRE check (if include_nre is TRUE)
        if (!is.null(results$grw) && results$grw$grw_prediction) {
          best_model_return <- "grw"
          reason_return <- "Differences between consecutive timepoints show no non-random events and have significant slope"
          break
        }
      }
    }

  return(list(
    best_model = best_model_return,
    stasis_analysis = results$stasis,
    urw_analysis = results$urw,
    grw_analysis = results$grw,
    reason = reason_return
  ))
}

# --- Usage example (run this line in your R console after sourcing the script) ---
# analyze_urw_trait("/Users/neelviswanathan/CCEvoMP/Ptery L.csv")

# --- Helper function to find the end of a segment ---
#' Find the first time point where a simple model can be predicted
#'
#' @param df Data frame containing the time series data
#' @param start_idx Index of the starting time point
#' @param min_points Minimum number of points required for analysis
#' @param include_nre Logical, whether to include non-random events in the segmentation criteria
#'
#' @return Index of the first time point where a simple model could be predicted, or NULL if no model could be predicted
find_segment_end <- function(df, start_idx, min_points, include_nre) {
  # Need at least min_points for meaningful analysis from the start index
  if(start_idx + min_points - 1 > nrow(df)) {
    return(NULL)
  }
  
  # Start with the maximum possible segment
  end_idx <- nrow(df)
  segment_data <- df[start_idx:end_idx, ]
  
  # Try to predict model for the full remaining segment
  result <- suppressMessages(suppressWarnings(predict_evolutionary_model(
    data = segment_data,
    include_nre = include_nre,
    verbose = FALSE
  )))
  
  # If the full segment works, return it
  if(!is.null(result) && !is.null(result$best_model) && result$best_model %in% c("stasis", "urw", "grw")) {
    return(end_idx)
  }
  
  # If not, try progressively shorter segments
  while(end_idx > start_idx + min_points - 1) {
    # Try a segment that's 75% of the current length
    end_idx <- start_idx + floor((end_idx - start_idx + 1) * 0.75)
    segment_data <- df[start_idx:end_idx, ]
    
    result <- suppressMessages(suppressWarnings(predict_evolutionary_model(
      data = segment_data,
      include_nre = include_nre,
      verbose = FALSE
    )))
    
    if(!is.null(result) && !is.null(result$best_model) && result$best_model %in% c("stasis", "urw", "grw")) {
      # Found a valid segment, now try to extend it
      while(end_idx < nrow(df)) {
        test_end <- end_idx + 1
        test_data <- df[start_idx:test_end, ]
        test_result <- suppressMessages(suppressWarnings(predict_evolutionary_model(
          data = test_data,
          evaluation_order = c("Stasis", "URW", "GRW"),
          include_nre = include_nre,
          verbose = FALSE
        )))
        
        if(!is.null(test_result) && !is.null(test_result$best_model) && 
           test_result$best_model %in% c("stasis", "urw", "grw")) {
          end_idx <- test_end
        } else {
          break
        }
      }
      return(end_idx)
    }
  }
  
  return(NULL)
}

# --- Helper function to find the largest possible interval ---
#' Find the largest possible interval where any simple model can be predicted
#'
#' @param df Data frame containing the time series data
#' @param min_points Minimum number of points required for analysis
#' @param include_nre Logical, whether to include non-random events in the segmentation criteria
#'
#' @return A list containing start_idx and end_idx of the largest valid interval, or NULL if no valid interval found
find_largest_valid_interval <- function(df, min_points = 15, include_nre = TRUE) {
  # Need at least min_points for meaningful analysis
  if(nrow(df) < min_points) {
    return(NULL)
  }
  
  largest_interval <- NULL
  max_length <- 0
  
  # Try all possible start points
  for(start_idx in 1:(nrow(df) - min_points + 1)) {
    # For each start point, try all possible end points
    for(end_idx in (start_idx + min_points - 1):nrow(df)) {
      # Create a temporary CSV file for this interval
      temp_file <- tempfile(fileext = ".csv")
      interval_data <- df[start_idx:end_idx, ]
      write.csv(interval_data, temp_file, row.names = FALSE)
      
      # Try to predict model for this interval
      result <- suppressMessages(suppressWarnings(predict_evolutionary_model(data = df_subset, evaluation_order = c("Stasis", "URW", "GRW"), 
        temp_file,
        include_nre = include_nre
      )))
      
      # Clean up temporary file
      unlink(temp_file)
      
      # If we found a simple model and this interval is larger than our current largest
      if(!is.null(result$best_model) && result$best_model %in% c("stasis", "urw", "grw")) {
        interval_length <- end_idx - start_idx + 1
        if(interval_length > max_length) {
          max_length <- interval_length
          largest_interval <- list(
            start_idx = start_idx,
            end_idx = end_idx,
            model = result$best_model
          )
        }
      }
    }
  }
  
  return(largest_interval)
}

#' Create a beautiful output for model predictions
#'
#' @param subgroups List of subgroups from analyze_entire_series
#' @return A formatted string with the prediction results
create_prediction_output <- function(subgroups) {
  # Create a header
  output <- "\n================================ EVOLUTIONARY MODEL PREDICTION ================================\n"
  output <- paste0(output, "|                EVOLUTIONARY MODEL PREDICTION                |\n")
  output <- paste0(output, "==============================================================================================\n")
  
  # If we have only one subgroup with a simple model
  if(length(subgroups) == 1 && subgroups[[1]]$model %in% c("stasis", "urw", "grw")) {
    model <- toupper(subgroups[[1]]$model)
    # Bold the model prediction
    output <- paste0(output, "|  SIMPLE MODEL DETECTED: **", model, "**\n")
    output <- paste0(output, "|  Time Range: ", subgroups[[1]]$start_time, " to ", subgroups[[1]]$end_time, " years\n")
    output <- paste0(output, "|  Direction: ", ifelse(model == "GRW", "Directional", ifelse(model == "URW", "Random", "Stable")), "\n")
  } else {
    # Complex model with transitions
    models <- sapply(subgroups, function(s) toupper(s$model))
    transition_str <- paste(models, collapse = " -> ")
    output <- paste0(output, "|  MODEL TRANSITION: ", transition_str, "\n")
    output <- paste0(output, "|  Transitions:                                            |\n")
    
    for(i in seq_along(subgroups)) {
      subgroup <- subgroups[[i]]
      model <- toupper(subgroup$model)
      if(model %in% c("STASIS", "URW", "GRW")) {
        output <- paste0(output, "|    ", i, ". ", model, " (", subgroup$start_time, " to ", subgroup$end_time, " years)\n")
      } else {
        output <- paste0(output, "|    ", i, ". Complex Segment (", subgroup$start_time, " to ", subgroup$end_time, " years)\n")
      }
    }
  }
  
  # Add footer
  output <- paste0(output, "==============================================================================================\n")
  
  return(output)
}

#' Internal function to visualize subgroups and transitions on an X-Bar Chart
#' This function is not meant to be called directly by users.
#' It is used internally by analyze_entire_series.
#'
#' @param df Data frame containing the time series data (with columns mn, time)
#' @param subgroups List of subgroups from analyze_entire_series
#' @param file_path The original file path to name the output plot
#' @return A ggplot object
visualize_subgroups <- function(df, subgroups, file_path) {
  # Extract base filename for plot naming
  base_filename <- tools::file_path_sans_ext(basename(file_path))
  plot_file <- paste0("Subgroup_Plot_", base_filename, ".png")
  
  # Create the base plot of mean trait values over time
  p <- ggplot(df, aes(x = time, y = mn)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_point(color = "blue", size = 2) # Keep base points, endpoints will be layered on top
  
  # Add vertical lines at transition points
  # Transition points are the end times of subgroups, except the last one
  transition_times <- sapply(subgroups[1:(length(subgroups)-1)], function(s) s$end_time)
  
  if (length(transition_times) > 0) {
    p <- p + geom_vline(xintercept = transition_times, linetype = "dashed", color = "red", linewidth = 1)
  }
  
  # Identify endpoint data points for simple models
  endpoint_data <- data.frame()
  for (subgroup in subgroups) {
    if (subgroup$model %in% c("stasis", "urw", "grw")) {
      # Find the data point corresponding to the end_time
      endpoint_row <- df[df$time == subgroup$end_time, ]
      if (nrow(endpoint_row) > 0) {
        endpoint_row$model_type <- toupper(subgroup$model) # Store model type
        endpoint_data <- rbind(endpoint_data, endpoint_row)
      }
    }
  }
  
  # Add a layer for endpoint points with shapes mapped to model type
  if (nrow(endpoint_data) > 0) {
    p <- p + geom_point(data = endpoint_data, aes(x = time, y = mn, shape = model_type), size = 5, fill = "black", color = "black") + # Fill with black for filled shapes
             ggplot2::scale_shape_manual(values = c("STASIS" = 15, "URW" = 16, "GRW" = 17)) # Updated shapes
  }
  
  # Add subgroup labels and time ranges below the plot
  label_data <- data.frame(
    start_time = sapply(subgroups, function(s) s$start_time),
    end_time = sapply(subgroups, function(s) s$end_time),
    model = sapply(subgroups, function(s) toupper(s$model))
  )
  
  # Calculate duration and proportional font size for labels
  total_time_span <- max(df$time, na.rm = TRUE) - min(df$time, na.rm = TRUE)
  base_font_size <- 4 # Base font size for geom_text
  font_size_multiplier <- 2 # How much font size can vary
  min_duration_ratio <- 0.1 # Minimum segment duration ratio to get base_font_size
  
  label_data <- label_data %>%
    mutate(
      duration = end_time - start_time,
      # Calculate proportional font size: smaller for shorter durations
      # Scale duration from min_duration_ratio to 1, then multiply by base_font_size and multiplier
      proportional_size = pmax(min_duration_ratio, duration / total_time_span), # Avoid too small durations
      calculated_size = base_font_size + (proportional_size * font_size_multiplier),
      
      # Initial text horizontal position (midpoint of the segment)
      initial_text_x = start_time + (end_time - start_time) / 2
    ) %>%
    rowwise() %>%
    mutate(
      # Adjust text_x to avoid overlap with vertical lines
      adjusted_text_x = {
        x_pos <- initial_text_x
        # Define a threshold for proximity, relative to the plot's x-range
        x_range <- max(df$time, na.rm = TRUE) - min(df$time, na.rm = TRUE)
        proximity_threshold <- x_range * 0.02 # 2% of x-range as threshold
        
        # Check proximity to transition lines
        for (t_time in transition_times) {
          # If label midpoint is close to a transition time
          if (abs(x_pos - t_time) < proximity_threshold) {
            # Determine direction of nudge: away from the transition line
            if (x_pos < t_time) { # Label is to the left of the line, nudge left
              x_pos <- t_pos - proximity_threshold # Nudge to the left of the line
            } else { # Label is to the right of the line, nudge right
              x_pos <- t_time + proximity_threshold # Nudge to the right of the line
            }
          }
        }
        # Also ensure text_x stays within segment bounds (optional, but good practice)
        # x_pos <- max(start_time, min(end_time, x_pos))
        x_pos # Return the adjusted position
      }
    ) %>% 
    ungroup() # Return to normal data frame
  
  # Calculate y position for labels - dynamically adjust based on segment count
  min_y <- min(df$mn, na.rm = TRUE)
  # Stagger labels if there are more than one
  label_data <- label_data %>%
    mutate(
      label = paste0(start_time, " to ", end_time, " yrs - ", model),
      # Use adjusted_text_x for horizontal position
      text_x = adjusted_text_x,
      # Stagger y positions for labels - further decrease for more subgroups
      text_y = min_y - (max(df$mn, na.rm = TRUE) - min_y) * 0.2 - (row_number() - 1) * (max(df$mn, na.rm = TRUE) - min_y) * 0.07 # Adjusted multiplier
    )
  
  # Add text labels with calculated size
  p <- p + geom_text(data = label_data, aes(x = text_x, y = text_y, label = label, size = calculated_size), 
                       vjust = 1, color = "darkgreen", fontface = "bold", show.legend = FALSE) + # vjust=1 places top of text at y position, bold font
       scale_size_identity() # Use the calculated size directly
  
  # Add overall plot labels and theme
  p <- p + theme_minimal() +
    labs(
      title = paste0("Evolutionary Model Subgroups for ", basename(file_path)),
      x = "Time (yrs)",
      y = "Mean Trait Value",
      shape = "Model Type" # Legend title for shapes
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12, face = "bold"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      
      # Position the shape legend
      legend.position = c(0.9, 0.9), # Adjust position as needed (x, y)
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      legend.key = element_rect(fill = "white"), # Background for legend keys
      
      # Make axes bold arrows with tick marks
      axis.line.x = element_line(arrow = arrow(length = unit(0.3, "cm"), type = "closed"), color = "black", linewidth = 1.2),
      axis.line.y = element_line(arrow = arrow(length = unit(0.3, "cm"), type = "closed"), color = "black", linewidth = 1.2),
      axis.ticks = element_line(color = "black", linewidth = 1.2), # Major ticks
      axis.ticks.length = unit(0.15, "cm"), # Length for major ticks
      
      # Add settings for minor ticks
      axis.ticks.x.minor = element_line(color = "black", linewidth = 0.8), # Minor x-axis ticks (thicker)
      axis.ticks.y.minor = element_line(color = "black", linewidth = 0.8), # Minor y-axis ticks (thicker)
      axis.ticks.length.minor = unit(0.1, "cm") # Length for minor ticks (slightly longer)
    )
  
  # Modify x-axis breaks and labels
  min_time <- min(df$time, na.rm = TRUE)
  max_time <- max(df$time, na.rm = TRUE)
  
  # Custom label function to label the minimum time as "Oldest"
  custom_x_labels <- function(breaks) {
    labels <- character(length(breaks))
    min_time_val <- min(df$time, na.rm = TRUE) # Ensure min_time is calculated within the function scope
    
    for (i in seq_along(breaks)) {
      # Check for NA and perform comparison safely
      if (!is.na(breaks[i]) && abs(breaks[i] - min_time_val) < .Machine$double.eps^0.5) { # Use tolerance for floating point comparison
        labels[i] <- "Oldest"
      } else if (!is.na(breaks[i])) { # Only format non-NA breaks
        labels[i] <- as.character(breaks[i])
      } else {
        labels[i] <- "" # Handle NA breaks with an empty string
      }
    }
    return(labels)
  }
  
  # Calculate y-axis range for minor breaks
  min_mn <- min(df$mn, na.rm = TRUE)
  max_mn <- max(df$mn, na.rm = TRUE)
  
  # Calculate minor breaks
  x_minor_breaks <- seq(floor(min_time / 1000) * 1000, ceiling(max_time / 1000) * 1000, by = 1000)
  y_minor_breaks <- seq(floor(min_mn / 0.1) * 0.1, ceiling(max_mn / 0.1) * 0.1, by = 0.1)
  
  p <- p + 
    scale_x_continuous(
      labels = custom_x_labels,
      limits = c(min_time, max_time),
      expand = c(0, 0),
      minor_breaks = x_minor_breaks
    ) +
    scale_y_continuous(
      minor_breaks = y_minor_breaks
    )
  
  # Save the plot
  ggsave(plot_file, p, width = 10, height = 6, dpi = 300)
  
  return(p)
}

# The analyze_entire_series function is now defined in analyze_entire_series.R
# This ensures we use the most up-to-date version with all features

# --- Usage example ---
# results <- analyze_entire_series("path/to/data.csv")

# --- Function to analyze specific time range ---
#' Analyze a Specific Time Range
#'
#' Analyzes a subset of the time series data based on a specified time range.
#'
#' @param file_path The path to the input CSV file.
#' @param start_time The starting time point for the analysis.
#' @param end_time The ending time point for the analysis.
#' @param include_nre A logical value indicating whether to include non-random effects.
#' @param trait_name A string specifying the name of the trait column in the data.
#' @return A list containing the results of the analysis for the specified time range.
#' @export
analyze_time_range <- function(file_path, start_time, end_time, include_nre = TRUE, trait_name = "trait") { # Add trait_name parameter
  # Read and prepare data
  df <- read.csv(file_path)
  colnames(df) <- tolower(colnames(df))
  df <- df %>% rename(mn = mm, sd = vv, n = nn, time = tt)
  df <- df %>% arrange(time)
  
  # Subset to the specified time range
  df_subset <- df %>% filter(time >= start_time & time <= end_time)
  
  if(nrow(df_subset) == 0) {
    stop("No data points found in the specified time range")
  }
  
  # Analyze the subset - pass trait_name, start_time, and end_time
  result <- predict_evolutionary_model(df_subset, evaluation_order = c("Stasis", "URW", "GRW"), include_nre = include_nre, verbose = TRUE, 
                                      trait_name = trait_name, start_time = start_time, end_time = end_time)
  
  return(result)
}

#' Analyze Time Series Data from Oldest to Youngest
#'
#' This function analyzes a time series dataset from the oldest to the youngest time point,
#' generating control charts and model predictions. It's particularly useful for evolutionary
#' time series data where the direction of time is from older to younger samples.
#'
#' @param file_path Character string specifying the path to the CSV file containing the time series data.
#'        The file should contain columns for time and measurement data.
#' @param include_nre Logical indicating whether to include the non-random evolution (NRE) model
#'        in the analysis. Default is TRUE.
#' @param trait_name Character string specifying the name of the trait being analyzed.
#'        This is used in generating output filenames. Default is "trait".
#'
#' @return A list containing the results from predict_evolutionary_model for the entire time series.
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Reads and prepares the time series data
#'   \item If include_nre is TRUE, generates and saves control charts:
#'     \itemize{
#'       \item X-Bar chart (similar to stasis plot)
#'       \item Individuals chart (similar to URW plot)
#'     }
#'   \item Analyzes the data using predict_evolutionary_model
#'   \item Returns the analysis results
#' }
#'
#' @examples
#' \dontrun{
#'   # Analyze a time series file
#'   results <- analyze_oldest_to_youngest("path/to/your/data.csv")
#'   
#'   # Analyze with custom trait name
#'   results <- analyze_oldest_to_youngest("data.csv", trait_name = "body_mass")
#' }
#'
#' @importFrom dplyr %>% arrange rename filter
#' @importFrom stats acf
#' @importFrom ggplot2 ggsave
#' @importFrom utils read.csv
#' @export
analyze_oldest_to_youngest <- function(file_path, include_nre = TRUE, trait_name = "trait") {
  # Add .csv extension if not present
  if (!grepl("\\.csv$", file_path)) {
    file_path <- paste0(file_path, ".csv")
  }

  # Read and prepare data
  df <- read.csv(file_path)
  colnames(df) <- tolower(colnames(df))

  # Check if we need to rename columns
  if (all(c("mm", "vv", "nn", "tt") %in% colnames(df))) {
    df <- df %>% rename(mn = mm, sd = vv, n = nn, time = tt)
  } else if (!all(c("mn", "sd", "n", "time") %in% colnames(df))) {
    stop("Data must contain columns: mn (or mm), sd (or vv), n (or nn), and time (or tt)")
  }

  df <- df %>% arrange(time)

  # Use the entire time range
  start_time <- min(df$time)
  end_time <- max(df$time)

  # Subset to the entire time range
  df_subset <- df %>% filter(time >= start_time & time <= end_time)

  if(nrow(df_subset) == 0) {
    stop("No data points found in the specified time range")
  }
  
  # --- Generate plots (copied from predict_evolutionary_model) ---
  if (include_nre) { # Only generate plots if including NREs
    # XBAR Chart (similar to stasis plot)
    data_values <- df_subset$mn
    subgroup_sds <- df_subset$sd
    time_values <- df_subset$time # Get time values for plotting

    # Only proceed with visualization if we have enough points
    if(length(data_values) >= 2) {
      avg_mm <- mean(data_values, na.rm = TRUE)
      avg_vv <- mean(subgroup_sds, na.rm = TRUE)
  
      # Calculate autocorrelation only if we have enough points
      r <- NA
      if (length(data_values) > 2) {
        autocorr_result <- suppressWarnings(acf(data_values, lag.max = 1, plot = FALSE))
        if (!is.null(autocorr_result$acf) && length(autocorr_result$acf) > 1) {
          r <- autocorr_result$acf[2, 1, 1]
        }
      }

      # Use fallback if autocorrelation couldn't be calculated
      if (is.na(r) || 1 - r^2 < .Machine$double.eps) {
        adjusted_std_dev_for_limits <- avg_vv
      } else {
        adjusted_std_dev_for_limits <- avg_vv / sqrt(1 - r^2)
      }

      center_line <- avg_mm
      lcl_calc <- center_line - 3 * adjusted_std_dev_for_limits
      ucl_calc <- center_line + 3 * adjusted_std_dev_for_limits

      cc <- setup_control_chart(data_values, type = "xbar", subgroup_size = 1, calculated_limits = c(lcl_calc, ucl_calc));
      patterns <- detect_patterns(data_values, cc, rules = 1:8);

      # Only create and save plot if we have enough points
      stasis_plot_obj <- visualize_results(data_values, cc, patterns, "stasis", time_values = time_values) # Pass time_values here
      if(!is.null(stasis_plot_obj)) {
         xbar_filename <- paste0("XBAR_Chart_", trait_name, ".png") # Use trait name only
         ggsave(xbar_filename, stasis_plot_obj, width = 12, height = 6); # Save with constructed filename
      }
    }

    # Individuals Chart (similar to URW plot)
    data_diff <- diff(df_subset$mn)
    time_values_delta <- df_subset$time[-1] # Get time values for differences

    # Only proceed with visualization if we have enough points
    if(length(data_diff) >= 2) {
      # Calculate second differences (differences of differences)
      second_diffs <- diff(data_diff)
      avg_second_diffs <- mean(abs(second_diffs), na.rm = TRUE)
  
      # Calculate the lag-1 autocorrelation coefficient (r)
      r <- NA
      if (length(data_diff) > 2) {
        autocorr_result <- suppressWarnings(acf(data_diff, lag.max = 1, plot = FALSE))
        if (!is.null(autocorr_result$acf) && length(autocorr_result$acf) > 1) {
          r <- autocorr_result$acf[2, 1, 1]
        }
      }

      # Calculate the Adjusted Standard Deviation for Limits
      if (is.na(r) || 1 - r^2 < .Machine$double.eps) {
        adjusted_std_dev_for_limits <- avg_second_diffs / 1.128
      } else {
        adjusted_std_dev_for_limits <- (avg_second_diffs / sqrt(1 - r^2)) / 1.128
      }

      center_line <- mean(data_diff, na.rm = TRUE);
      lcl_calc <- center_line - 3 * adjusted_std_dev_for_limits;
      ucl_calc <- center_line + 3 * adjusted_std_dev_for_limits;

      cc <- setup_control_chart(data_diff, type = "xbar", subgroup_size = 1, calculated_limits = c(lcl_calc, ucl_calc));
      patterns <- detect_patterns(data_diff, cc, rules = 1:8);

      # Only create and save plot if we have enough points
      urw_plot_obj <- visualize_results(data_diff, cc, patterns, "urw", time_values = time_values_delta); # Pass time_values_delta here
      if(!is.null(urw_plot_obj)) {
         individuals_filename <- paste0("Individuals_Chart_", trait_name, ".png") # Use trait name only
         ggsave(individuals_filename, urw_plot_obj, width = 12, height = 6); # Save with constructed filename
      }
    }
  }

  # Analyze the subset using predict_evolutionary_model for results only
  # verbose is FALSE so it doesn't generate plots again
  result <- predict_evolutionary_model(data = df_subset, 
                                      include_nre = include_nre, 
                                      verbose = FALSE) 
  
  return(result)
}
