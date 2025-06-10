# CCEvoMP 0.1.4: Control Chart Evolutionary Model Predictor

[![Github](https://img.shields.io/badge/Github-CCEvoMP-blue.svg)](https://github.com/neelviswanathan08/CCEvoMP)

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

[![CRAN](https://img.shields.io/badge/CRAN-0.1.3-green.svg)](https://CRAN.R-project.org/package=CCEvoMP)


> **CCEvoMP** is an R package for analyzing evolutionary patterns in time series data using control chart methodology. It implements a hierarchical model selection framework for identifying the best-fitting evolutionary model (Stasis, Unbiased Random Walk, or General Random Walk) for evolutionary biology time series data.

## 🚀 Quick Start

```r
# Install development version
devtools::install_github("neelviswanathan08/CCEvoMP")

# Load package and example data
library(CCEvoMP)
data_path <- system.file("examples", "LSeriesDorsalSpines.csv", package = "CCEvoMP")

# Run analysis
results <- analyze_entire_series(
  file = data_path,
  trait_name = "Dorsal Spines",
  min_points = 7
)

# View results
print(results$summary)
plot(results$plots$individual_chart)
```

## 📚 Table of Contents

- [Installation](#-installation)
- [Core Features](#-core-features)
- [Basic Usage](#-basic-usage)
- [Model Selection](#-model-selection)
- [Troubleshooting](#-troubleshooting)
- [Contributing](#-contributing)
- [License](#-license)
- [Citation](#-citation)
- [Acknowledgments](#-acknowledgments)

## 📦 Installation

### From CRAN (Coming Soon)
```r
install.packages("CCEvoMP")
```

### Development Version
```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install from GitHub
devtools::install_github("neelviswanathan08/CCEvoMP")
```

## 🔍 Core Features

### 📊 Model Selection
- **Stasis**: Variation around a stable mean
- **URW**: Random walk with no directional trend
- **GRW**: Random walk with directional trend

### 📈 Control Chart Analysis
- Individual and moving range charts
- X-bar and R charts
- Automated pattern detection

### 📋 Data Requirements
```
- time: Chronological time points
- mn:   Mean trait values
- sd:   Standard deviations (optional)
- n:    Sample sizes (optional)
```

## 🛠️ Basic Usage

### 1. Load Package and Data
```r
library(CCEvoMP)
data_path <- system.file("examples", "LSeriesDorsalSpines.csv", package = "CCEvoMP")
```

### 2. Run Analysis
```r
results <- analyze_entire_series(
  file = data_path,
  trait_name = "Your Trait",
  min_points = 7
)
```

### 3. View Results
```r
# Summary statistics
print(results$summary)

# Diagnostic plots
plot(results$plots$individual_chart)
plot(results$plots$xbar_chart)

# Save results
write.csv(results$summary, "analysis_results.csv")
ggsave("control_chart.png", results$plots$individual_chart)
```

```R
# Load example data
file_path <- system.file("examples", "LSeriesDorsalSpines.csv", package = "CCEvoMP")
stickleback_data <- read.csv(file_path)
head(stickleback_data)
```

## Core Features

### 1. Hierarchical Model Selection
CCEvoMP implements a flexible framework for comparing three evolutionary models:
- **Stasis**: Stasis normally distributed variation around a phenotypic optima. 
- **Unbiased Random Walk (URW)**: An unbiased random walk is a normally distributed variation, where an increase or decrease in the difference between consecutive points is ~0.
- **General Random Walk (GRW)**: A general random walk is where the average difference between consecutive points differs from 0.

### 2. Control Chart Analysis
Implements statistical process control methods to identify:
- Shifts in trait means
- Changes in trait variance
- Presence of trends or cycles
- Statistical aberrations

### 3. Comprehensive Visualization
Generates diagnostic plots including:
- Individual value charts
- Moving range charts
- Model fit diagnostics
- Residual analysis

## Data Requirements

### Input Format
Your data should be a data frame or CSV file with the following columns:
- `time` or `tt`: Time points (numeric)
- `mn` or `mm`: Mean trait values (numeric)
- `sd` or `vv`: Standard deviation of trait values (numeric)
- `n` or `nn`: Sample size (integer)

### Minimum Requirements
- At least 7 time points for basic analysis
- Chronologically ordered time points
- No missing values in required columns

## Step-by-Step Tutorial with LSeriesDorsalSpines.csv (from 'inst' folder of CCEvoMP package)

This tutorial will walk you through using CCEvoMP with the included `LSeriesDorsalSpines.csv` dataset, which contains measurements of stickleback dorsal spines across different time points.

### Step 1: Load the Package and Data

First, let's load the package and examine the example dataset:

```r
# Load package and data
library(CCEvoMP)
file_path <- system.file("examples", "LSeriesDorsalSpines.csv", package = "CCEvoMP")
data <- read.csv(file_path)
head(data)  # View first few rows
```

### Basic Analysis
```r
# Analyze entire time series
results <- analyze_entire_series(
  file = file_path,
  trait_name = "Dorsal Spines",
  min_points = 7
)

# View results summary
print(results$summary)
```

### View Model Details
```r
# View model comparisons
print(results$models$Stasis$summary)
print(results$models$URW$summary)
print(results$models$GRW$summary)

# View diagnostic plots
plot(results$plots$individual_chart)
plot(results$plots$xbar_chart)
```

### Analyze Time Range
```r
time_results <- analyze_time_range(
  file = file_path,
  start_time = 4500,
  end_time = 20000,
  trait_name = "Dorsal Spines",
  min_points = 7
)
print(time_results$summary)
```

### Analyze Chronological Segments
```r
seg_results <- analyze_oldest_to_youngest(
  file = file_path,
  trait_name = "Dorsal Spines",
  min_points = 10
)
print(seg_results$segments)
```

### Advanced Usage

#### Custom Data Preparation
```r
# Prepare custom data frame
data_processed <- data.frame(
  time = data$tt,  # Time points
  mn = data$mm,    # Mean trait values
  sd = data$vv,    # Standard deviations
  n = data$nn      # Sample sizes
)

# Run custom analysis
result <- predict_evolutionary_model(
  data = data_processed,
  trait_name = "My Analysis"
)
print(result$best_model)
```

#### Save Results
```r
# Save analysis results
output_dir <- "results"
dir.create(output_dir, showWarnings = FALSE)

# Save summary and plots
write.csv(result$summary, file.path(output_dir, "model_summary.csv"))
ggsave(file.path(output_dir, "individual_chart.png"), result$plots$individual_chart)
ggsave(file.path(output_dir, "xbar_chart.png"), result$plots$xbar_chart)
```

This covers the main functionality of CCEvoMP. Use these examples as a starting point for your own analyses.

## 🔍 Model Selection

### 🎯 Hierarchical Approach
The package evaluates models in this order (customizable):
1. **Stasis** → **URW** → **GRW**

### 📊 Control Chart Rules
| Model  | Charts Used       | Key Metrics                  |
|--------|------------------|-----------------------------|
| Stasis | I-MR             | Points within control limits|
| URW    | X-Bar R          | No significant trends       |
| GRW    | X-Bar R          | Significant trends          |


### 📈 Key Tests
- Chi-squared goodness-of-fit with null hypothesis alpha value of 0.05 (Normality Test)
- Control chart rules (Western Electric rules)
- Slope of the linear regression line (URW and GRW)

## 🎨 Visualization

### Standard Plots
```r
# Time series with model fit
plot_evolutionary_series(results)

# Control chart analysis
plot_control_chart(results)

# Model diagnostics
plot_model_diagnostics(results)
```

### Customization Example
```r
library(ggplot2)

plot_evolutionary_series(results) +
  ggtitle("My Analysis") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
```

## 🚨 Troubleshooting

### Common Issues & Solutions
| Issue | Solution |
|-------|----------|
| Data format errors | Check column names and types |
| Model fitting fails | Increase `min_points` |
| Poor model fits | Check for outliers |
| Plot issues | Update `ggplot2` |

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing`)
5. Open a Pull Request

## 📜 License

MIT © [Neel Viswanathan](https://github.com/neelviswanathan08)

## 📝 Citation

```bibtex
@software{viswanathan2025ccevomp,
  author = {Viswanathan, Neel and Kirshner, Brian},
  title = {CCEvoMP: Control Chart Evolutionary Model Predictor},
  year = {2025},
  publisher = {GitHub},
  journal = {GitHub repository},
  howpublished = {https://github.com/neelviswanathan08/CCEvoMP}
}
```

## 🙏 Academic Contributions
- **Michael A. Bell**  
  *University of California, Berkeley*  
  For providing the seminal stickleback dataset and pioneering work in evolutionary biology

- **Jack Tseng**  
  *University of California, Berkeley*  
  For methodological insights and valuable feedback on the control chart approach

- **Gene Hunt**  
  *Smithsonian Institution*  
  For foundational discussions on evolutionary model selection frameworks and applications of control charts in paleontology

- **Brian Kirshner**  
  *Co-Developer*  
  For developing the original control chart and statistical methodology for evolutionary analysis 

---

*Development of this package was assisted by Claude 3.7 Sonnet (Anthropic, 2024), an AI language model, which contributed to the implementation of control chart methodology, statistical aberration detection, and visualization functions.*

---

📫 **Questions or feedback?**  
## 📧 Contact

For questions, bug reports, or feature requests, please:

1. Check the [documentation](https://github.com/neelviswanathan08/CCEvoMP#readme)
2. Search [existing issues](https://github.com/neelviswanathan08/CCEvoMP/issues)
3. Open a [new issue](https://github.com/neelviswanathan08/CCEvoMP/issues/new/choose)
4. Email: [neelviswanathan08@gmail.com](mailto:neelviswanathan08@gmail.com)

## 🤝 Contributing

We welcome contributions! Please see our [GitHub Issues](https://github.com/neelviswanathan08/CCEvoMP/issues) page to report bugs, request features, or discuss potential improvements.

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 📚 References

1. Hunt, G. (2006). Fitting and comparing models of phyletic evolution: random walks and beyond. *Paleobiology*, 32(4), 578-601. https://doi.org/10.1666/0094-8373(2006)32[578:FACOMPE]2.0.CO;2
2. Hunt, G., Bell, M. A., & Travis, M. P. (2008). Evolution toward a new adaptive optimum: Phenotypic evolution in a fossil stickleback lineage. *Evolution*, 62(3), 700-710. https://doi.org/10.1111/j.1558-5646.2007.00310.x
3. Sheets, H. D., & Mitchell, C. E. (2001). Why the null matters: Statistical tests, random walks and evolution. *Genetica*, 112-113, 105-125. https://doi.org/10.1023/A:1013308402471
4. Voje, K. L. (2016). Tempo does not correlate with mode in the fossil record. *Evolution*, 70(12), 2678-2689. https://doi.org/10.1111/evo.13090
5. Hunt, G., & Rabosky, D. L. (2014). Phenotypic evolution in fossil species: Pattern and process. *Annual Review of Earth and Planetary Sciences*, 42, 421-441. https://doi.org/10.1146/annurev-earth-040809-152524

## 🛠️ Development Notes

This package was developed with the assistance of AI tools for code generation and documentation. The author maintains full responsibility for the package's content, having reviewed and modified all generated code to ensure accuracy, efficiency, and adherence to best practices. The final implementation represents the author's original work and intellectual contribution.

*Note: A manuscript describing the CCEvoMP package and its methodology is currently in preparation and is expected to be submitted for publication by the end of 2025.*

---

<p align="center">
  Authored and Maintained by Neel Viswanathan | 
  <a href="https://github.com/neelviswanathan08/CCEvoMP">GitHub</a> | 
  <a href="https://CRAN.R-project.org/package=CCEvoMP">CRAN</a>
</p>
