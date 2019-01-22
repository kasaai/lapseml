#' Weighted RMSE
#'
#' Calculates weighted RMSE
#'
#' @param data The data frame with predicted and actual values.
#' @param truth Column name for actual values.
#' @param estimate Column name for predicted values.
#' @param weights Column name for weights.
#'
#' @export
weighted_rmse <- function(data, truth, estimate, weights) {
  weighted_mse <- sum(data[[weights]] * (data[[truth]] - data[[estimate]]) ^ 2) /
    sum(data[[weights]])
  sqrt(weighted_mse)
}

