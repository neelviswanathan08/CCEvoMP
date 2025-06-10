\name{analyze_time_range}
\alias{analyze_time_range}
\title{Analyze a Specific Time Range in Trait Data}
\description{
  This function analyzes a specified time range within a time series of trait data,
  identifying the best-fitting evolutionary model (Stasis, URW, or GRW) for the given period.
  It's particularly useful for focusing on specific intervals of interest within a longer time series.
}
\usage{
analyze_time_range(
  file_path,
  start_time,
  end_time,
  include_nre = TRUE,
  trait_name = "trait"
)
}
\arguments{
  \item{file_path}{Path to the CSV file containing the time series data. The file should contain
                 columns for time points and trait measurements.}
  \item{start_time}{Numeric value specifying the starting time point for analysis.}
  \item{end_time}{Numeric value specifying the ending time point for analysis.}
  \item{include_nre}{Logical indicating whether to include non-random events in the analysis.
                    Default is TRUE.}
  \item{trait_name}{Character string specifying the name of the trait being analyzed.
                   Used for labeling plots and results. Default is "trait".}
}
\value{
  A list containing the analysis results, including:
  \item{best_model}{The best fitting evolutionary model for the specified time range}
  \item{stasis_analysis}{Results of the stasis model analysis}
  \item{urw_analysis}{Results of the unbiased random walk model analysis}
  \item{grw_analysis}{Results of the general random walk model analysis}
  \item{data_subset}{The subset of data used for the analysis}
}
\details{
  The function reads the input CSV file, subsets the data to the specified time range,
  and applies the same evolutionary model prediction methodology used in other functions
  in the package. It automatically handles column name variations (e.g., 'mm' or 'mn' for
  mean values, 'vv' or 'sd' for standard deviations).
}
\examples{\dontrun{
# Basic usage with example data
# Replace with your actual file path and time range
result <- analyze_time_range(
  file_path = "path/to/your/data.csv",
  start_time = 1000,
  end_time = 3000,
  include_nre = TRUE,
  trait_name = "Your Trait Name"
)

# View the best fitting model
print(result$best_model)
}
}

\seealso{
  \code{\link{analyze_entire_series}}, \code{\link{analyze_oldest_to_youngest}}
}
