#' CCEvoMP: A package for Control Chart Evolutionary Model Prediction.
#'
#' @description
#' The CCEvoMP package provides tools for analyzing time series data in
#' evolutionary contexts, with a focus on detecting punctuated equilibrium
#' and stasis patterns.
#'
#' @details
#' Version: 0.1.2
#'
#' @keywords internal
"_PACKAGE"
#'
#' @import methods
#' @import stats
#' @import utils
#' @import grDevices
#' @import graphics
#' @import grid
#' @import ggplot2
#' @import qcc

# Import specific functions to avoid conflicts
#' @importFrom dplyr select mutate rename
#' @importFrom stats filter lag
#' @importFrom magrittr %>% %<>%
#' @importFrom forecast auto.arima
#' @importFrom nortest ad.test
#' @importFrom tseries jarque.bera.test
NULL
