pkgname <- "CCEvoMP"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "CCEvoMP-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('CCEvoMP')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("analyze_entire_series")
### * analyze_entire_series

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyze_entire_series
### Title: Analyze Entire Time Series for Evolutionary Patterns
### Aliases: analyze_entire_series

### ** Examples

## Not run: 
##D   # Analyze a time series file
##D   results <- analyze_entire_series(
##D     "path/to/your/data.csv",
##D     include_nre = TRUE,
##D     min_points = 7,
##D     evaluation_order = c("Stasis", "URW", "GRW")
##D   )
##D   
##D   # Print summary of results
##D   print(results$model_summary)
##D   
##D   # Access individual segments
##D   first_segment <- results$subgroups[[1]]
##D   cat(sprintf("First segment: ##D 
##D               first_segment$start_time,
##D               first_segment$end_time,
##D               first_segment$model))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyze_entire_series", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyze_grw_trait")
### * analyze_grw_trait

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyze_grw_trait
### Title: Analyze Trait Data for General Random Walk (GRW) Pattern
### Aliases: analyze_grw_trait

### ** Examples

## Not run: 
##D   # Example usage:
##D   data <- data.frame(
##D     mn = cumsum(rnorm(20, mean = 0.1, sd = 0.5)),
##D     time = 1:20
##D   )
##D   result <- analyze_grw_trait(data)
## End(Not run)

## Not run: 
##D   # Example with built-in data
##D   data <- data.frame(
##D     mn = cumsum(rnorm(20, 0.1, 0.5)),
##D     sd = rep(0.5, 20),
##D     n = rep(10, 20),
##D     time = 1:20
##D   )
##D   result <- analyze_grw_trait(data)
##D   print(result$conclusion)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyze_grw_trait", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyze_oldest_to_youngest")
### * analyze_oldest_to_youngest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyze_oldest_to_youngest
### Title: Analyze Time Series Data from Oldest to Youngest
### Aliases: analyze_oldest_to_youngest

### ** Examples

## Not run: 
##D # Basic usage with default parameters
##D # Replace with your actual file path
##D result <- analyze_oldest_to_youngest("path/to/your/data.csv")
##D 
##D # With custom parameters
##D result <- analyze_oldest_to_youngest(
##D   file_path = "path/to/your/data.csv",
##D   include_nre = TRUE,
##D   trait_name = "Your Trait Name"
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyze_oldest_to_youngest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyze_stasis_trait")
### * analyze_stasis_trait

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyze_stasis_trait
### Title: Analyze Trait Data for Stasis Pattern
### Aliases: analyze_stasis_trait

### ** Examples

## Not run: 
##D   # Example usage:
##D   data <- data.frame(
##D     mn = rnorm(20, mean = 10, sd = 0.5),
##D     sd = rep(0.2, 20),
##D     time = 1:20
##D   )
##D   result <- analyze_stasis_trait(data)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyze_stasis_trait", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyze_time_range")
### * analyze_time_range

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyze_time_range
### Title: Analyze a Specific Time Range in Trait Data
### Aliases: analyze_time_range

### ** Examples
## Not run: 
##D # Basic usage with example data
##D # Replace with your actual file path and time range
##D result <- analyze_time_range(
##D   file_path = "path/to/your/data.csv",
##D   start_time = 1000,
##D   end_time = 3000,
##D   include_nre = TRUE,
##D   trait_name = "Your Trait Name"
##D )
##D 
##D # View the best fitting model
##D print(result$best_model)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyze_time_range", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyze_urw_trait")
### * analyze_urw_trait

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyze_urw_trait
### Title: Analyze Trait Data for Unbiased Random Walk (URW) Pattern
### Aliases: analyze_urw_trait

### ** Examples

## Not run: 
##D   # Example usage:
##D   data <- data.frame(
##D     mn = cumsum(rnorm(20, mean = 0, sd = 0.5)),
##D     time = 1:20
##D   )
##D   result <- analyze_urw_trait(data)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyze_urw_trait", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("predict_evolutionary_model")
### * predict_evolutionary_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: predict_evolutionary_model
### Title: Predict Evolutionary Model for Time Series Data
### Aliases: predict_evolutionary_model

### ** Examples
## Not run: 
##D # Example with sample data
##D # Create a small sample dataset
##D sample_data <- data.frame(
##D   time = 1:10,
##D   mn = c(10.1, 10.2, 10.0, 10.3, 10.1, 10.2, 10.3, 10.1, 10.2, 10.1),
##D   sd = rep(0.2, 10),
##D   n = rep(5, 10)
##D )
##D 
##D # Run analysis with default settings
##D result <- predict_evolutionary_model(
##D   data = sample_data,
##D   trait_name = "Sample Trait"
##D )
##D 
##D # View the best model
##D print(paste("Best fitting model:", result$best_model))
##D 
##D # Run analysis with custom evaluation order
##D result_custom <- predict_evolutionary_model(
##D   data = sample_data,
##D   evaluation_order = c("URW", "Stasis"),
##D   include_nre = FALSE,
##D   verbose = FALSE
##D )
## End(Not run)

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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("predict_evolutionary_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("setup_control_chart")
### * setup_control_chart

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: setup_control_chart
### Title: Setup Control Chart for Evolutionary Model
### Aliases: setup_control_chart
### Keywords: internal

### ** Examples

data <- rnorm(100, mean = 10, sd = 1)
cc <- setup_control_chart(data, type = "xbar", subgroup_size = 5)
data <- rnorm(100, mean = 10, sd = 1)
cc <- setup_control_chart(data, type = "xbar", subgroup_size = 5)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("setup_control_chart", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("subset_time_series")
### * subset_time_series

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: subset_time_series
### Title: Subset time series data to a specific time range
### Aliases: subset_time_series

### ** Examples

## Not run: 
##D # Create a sample dataset
##D sample_data <- data.frame(
##D   time = 1:100,
##D   mn = cumsum(rnorm(100)),
##D   sd = runif(100, 0.1, 0.5),
##D   n = 10
##D )
##D 
##D # Write to a temporary file
##D temp_file <- tempfile(fileext = ".csv")
##D write.csv(sample_data, temp_file, row.names = FALSE)
##D 
##D # Use the function
##D subset_data <- subset_time_series(temp_file, 20, 80)
##D 
##D # Clean up
##D unlink(temp_file)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("subset_time_series", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("visualize_results")
### * visualize_results

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: visualize_results
### Title: Visualize Control Chart Results with Detected Patterns
### Aliases: visualize_results

### ** Examples

## Not run: 
##D data <- rnorm(100, mean = 10, sd = 1)
##D time <- 1:100
##D cc <- setup_control_chart(data, type = "xbar", subgroup_size = 5)
##D patterns <- detect_patterns(data, cc, rules = 1:4)
##D plot <- visualize_results(data, cc, patterns, time_values = time, analysis_type = "stasis")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("visualize_results", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
