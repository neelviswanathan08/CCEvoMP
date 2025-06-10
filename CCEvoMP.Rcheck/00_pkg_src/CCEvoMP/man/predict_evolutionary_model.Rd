\name{predict_evolutionary_model}
\alias{predict_evolutionary_model}
\title{Predict Evolutionary Model for Time Series Data}
\description{
  This function analyzes time series data to determine the best-fitting evolutionary model
  among Stasis, Unbiased Random Walk (URW), and General Random Walk (GRW). It performs
  statistical tests and control chart analysis to identify patterns and non-random events
  in the data.
}
\usage{
predict_evolutionary_model(
  data = NULL,
  evaluation_order = c("Stasis", "URW", "GRW"),
  include_nre = TRUE,
  verbose = TRUE,
  trait_name = NULL,
  start_time = NULL,
  end_time = NULL
)
}
\arguments{
  \item{data}{A data frame containing the time series data with required columns:
             \code{mn} (mean values), \code{sd} (standard deviations),
             \code{n} (sample sizes), and \code{time} (time points).}
  \item{evaluation_order}{Character vector specifying the order in which to evaluate
                         the models. Must be a subset of c("Stasis", "URW", "GRW").
                         Default is c("Stasis", "URW", "GRW").}
  \item{include_nre}{Logical indicating whether to include non-random events (NRE)
                    in the analysis. Default is TRUE.}
  \item{verbose}{Logical indicating whether to print detailed progress messages
                and results. Default is TRUE.}
  \item{trait_name}{Character string specifying the name of the trait being analyzed.
                   Used for plot titles and file naming.}
  \item{start_time}{Optional numeric value specifying the start time for analysis.}
  \item{end_time}{Optional numeric value specifying the end time for analysis.}
}
\value{
  A list containing the following components:
  \item{best_model}{Character string indicating the best fitting model ("stasis", "urw", or "grw")}
  \item{stasis_analysis}{Results of the stasis model analysis, including:
    \itemize{
      \item{stasis: Logical indicating if stasis model is supported}
      \item{normality_p: p-value from normality test}
      \item{rules_triggered: List of triggered control chart rules}
      \item{avg_diff: Average difference between consecutive points}
    }
  }
  \item{urw_analysis}{Results of the unbiased random walk analysis}
  \item{grw_analysis}{Results of the general random walk analysis}
  \item{data}{The input data used for analysis}
  \item{trait_name}{Name of the analyzed trait}
}
\details{
  The function performs the following steps:
  \enumerate{
    \item Validates input data and parameters
    \item Optionally generates control charts if include_nre is TRUE
    \item Evaluates each model in the specified evaluation order
    \item Performs model-specific statistical tests
    \item Returns results including diagnostic information and plots
  }
  
  For each model, the function checks:
  \itemize{
    \item Stasis: Normality of residuals and absence of significant trends
    \item URW: Normality of first differences and absence of non-random patterns
    \item GRW: Significance of directional trend in the data
  }
}
\examples{\dontrun{
# Example with sample data
# Create a small sample dataset
sample_data <- data.frame(
  time = 1:10,
  mn = c(10.1, 10.2, 10.0, 10.3, 10.1, 10.2, 10.3, 10.1, 10.2, 10.1),
  sd = rep(0.2, 10),
  n = rep(5, 10)
)

# Run analysis with default settings
result <- predict_evolutionary_model(
  data = sample_data,
  trait_name = "Sample Trait"
)

# View the best model
print(paste("Best fitting model:", result$best_model))

# Run analysis with custom evaluation order
result_custom <- predict_evolutionary_model(
  data = sample_data,
  evaluation_order = c("URW", "Stasis"),
  include_nre = FALSE,
  verbose = FALSE
)
}

# A simpler example that can run without \dontrun
if (requireNamespace("stats", quietly = TRUE)) {
  sample_data <- data.frame(
    time = 1:5,
    mn = c(10.1, 10.2, 10.1, 10.3, 10.2),
    sd = rep(0.1, 5),
    n = rep(5, 5)
  )
  # Just show the structure without running the full analysis
  str(sample_data)
}
}
\seealso{
  \code{\link{analyze_entire_series}} for analyzing multiple time series segments,
  \code{\link{analyze_time_range}} for analyzing a specific time range,
  \code{\link{analyze_oldest_to_youngest}} for analyzing from oldest to youngest time points
}
