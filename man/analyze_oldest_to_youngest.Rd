\name{analyze_oldest_to_youngest}
\alias{analyze_oldest_to_youngest}
\title{Analyze Time Series Data from Oldest to Youngest}
\description{
  This function analyzes a time series of trait data from the oldest to the youngest time points,
  identifying segments that follow different evolutionary models (Stasis, URW, or GRW). It's
  particularly useful for detecting patterns of evolutionary change over time.
}
\usage{
analyze_oldest_to_youngest(file_path, include_nre = TRUE, trait_name = "trait")
}
\arguments{
  \item{file_path}{Path to the CSV file containing the time series data. The file should contain
                 columns for time points and trait measurements.}
  \item{include_nre}{Logical indicating whether to include non-random events in the analysis.
                    Default is TRUE.}
  \item{trait_name}{Character string specifying the name of the trait being analyzed.
                   Used for labeling plots and results. Default is "trait".}
}
\value{
  A list containing the analysis results, including:
  \item{best_model}{The best fitting evolutionary model for the time series}
  \item{stasis_analysis}{Results of the stasis model analysis}
  \item{urw_analysis}{Results of the unbiased random walk model analysis}
  \item{grw_analysis}{Results of the general random walk model analysis}
  \item{plots}{List of generated plots (if include_nre is TRUE)}
}
\details{
  The function reads the input CSV file, processes the time series data, and applies control chart
  methodology to identify segments that follow different evolutionary models. It automatically
  handles column name variations (e.g., 'mm' or 'mn' for mean values, 'vv' or 'sd' for standard
  deviations).
}
\examples{
\dontrun{
# Basic usage with default parameters
# Replace with your actual file path
result <- analyze_oldest_to_youngest("path/to/your/data.csv")

# With custom parameters
result <- analyze_oldest_to_youngest(
  file_path = "path/to/your/data.csv",
  include_nre = TRUE,
  trait_name = "Your Trait Name"
)
}
}
\seealso{
  \code{\link{analyze_entire_series}}, \code{\link{analyze_time_range}}
}
