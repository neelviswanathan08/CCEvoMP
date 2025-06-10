# Contributing to CCEvoMP

Thank you for considering contributing to CCEvoMP! We welcome all contributions, including bug reports, feature requests, documentation improvements, and code contributions.

## How to Contribute

### Reporting Issues
- Please check the [existing issues](https://github.com/neelviswanathan08/CCEvoMP/issues) before creating a new one
- Include a clear title and description with as much relevant information as possible
- If reporting a bug, please include a minimal reproducible example

### Code Contributions
1. Fork the repository
2. Create a new branch for your feature or bugfix
3. Make your changes with clear, concise commit messages
4. Add tests for any new functionality
5. Ensure all tests pass
6. Submit a pull request with a clear description of your changes

### Code Style
- Follow the [tidyverse style guide](https://style.tidyverse.org/)
- Use roxygen2 for documentation
- Write tests using testthat

## Development Setup

1. Install development dependencies:
   ```r
   install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown"))
   ```

2. Install the package in development mode:
   ```r
   devtools::install_github("neelviswanathan08/CCEvoMP")
   ```

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](https://www.contributor-covenant.org/version/2/0/code_of_conduct/). By participating in this project you agree to abide by its terms.

## License

By contributing, you agree that your contributions will be licensed under the [MIT License](LICENSE).
