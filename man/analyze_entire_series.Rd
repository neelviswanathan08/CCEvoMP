\name{analyze_entire_series}
\alias{analyze_entire_series}
\title{Analyze Entire Time Series for Evolutionary Patterns}
\description{
  This function analyzes a time series dataset to identify segments that follow different
  evolutionary models (Stasis, Unbiased Random Walk - URW, or General Random Walk - GRW).
  It automatically segments the time series and classifies each segment based on statistical
  criteria using control chart methodology.
}
\usage{
analyze_entire_series(file_path, include_nre = TRUE, min_points = 7,
  evaluation_order = c("Stasis", "URW", "GRW"))
}
\arguments{
  \item{file_path}{Character string specifying the path to the CSV file containing the time series data.
    The file should contain columns for time, mean values, standard deviations, and sample sizes.}
  \item{include_nre}{Logical. If TRUE (default), includes non-random events in the analysis.}
  \item{min_points}{Integer specifying the minimum number of points required for analysis (default: 7).
    Segments with fewer points than this will not be analyzed.}
  \item{evaluation_order}{Character vector specifying the order in which to evaluate the evolutionary models.
    Default is c("Stasis", "URW", "GRW"). The first model that fits the data will be selected.}

}
\value{
  A list containing the following components:
  \item{subgroups}{A list of data frames, each representing a segment of the time series with its
    identified evolutionary model}
  \item{best_model}{The best fitting model for each segment}
  \item{segment_boundaries}{A data frame showing the start and end points of each segment}
  \item{diagnostics}{Diagnostic information about the analysis}
  \item{model_summary}{A summary of the models used for each segment}
}
\details{
  The function works by:
  1. Reading and preprocessing the time series data
  2. Identifying segments that follow different evolutionary patterns
  3. Applying control chart rules to detect significant changes in the time series
  4. Classifying each segment using the specified evolutionary models
  5. Returning the segmented time series with model classifications

  The function automatically handles column name variations (e.g., 'mm' or 'mn' for means,
  'vv' or 'sd' for standard deviations, 'nn' or 'n' for sample sizes, 'tt' or 'time' for time).
}

\examples{
\dontrun{
  # Analyze a time series file
  results <- analyze_entire_series(
    "path/to/your/data.csv",
    include_nre = TRUE,
    min_points = 7,
    evaluation_order = c("Stasis", "URW", "GRW")
  )
  
  # Print summary of results
  print(results$model_summary)
  
  # Access individual segments
  first_segment <- results$subgroups[[1]]
  cat(sprintf("First segment: %.1f to %.1f (model: %s)\n",
              first_segment$start_time,
              first_segment$end_time,
              first_segment$model))
}
}
