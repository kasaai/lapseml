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
#' Plot Actual vs. Predicted
#'
#' @param df Data frame of predictions and actuals by quantile. Should be the
#'   return value of `compute_prediction_quantiles()`.
#' @param orientation this specify the orientation of the graph, Portrait or landscape.
#'   Default value is Portrait
#' @export
plot_actual_vs_expected <- function(df, orientation = "portrait") {
  if(orientation == "portrait"){
  df %>%
    tidyr::gather("key", "value", -"decile") %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$decile, y = .data$value, fill = .data$key)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::ggtitle("Average Actual vs. Predicted Lapse Rates") +
    ggplot2::xlab("Actual Lapse Rate Decile") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  } else if(orientation == "landscape") {
    df %>%
      tidyr::gather("key", "value", -"decile") %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$decile, y = .data$value, fill = .data$key)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::ggtitle("Average Actual vs. Predicted Lapse Rates") +
      ggplot2::xlab("Actual Lapse Rate Decile") +
      ggplot2::coord_flip()
  } else{
    stop("Orientation only takes the value landscape or portrait")
  }
}

#' Compute Average Predictions by Quantile
#'
#' Compute the average predicted and actual values by decile
#'
#' @param predictions Data frame of predictions.
#' @param predicted_col Column name of predicted values.
#' @param actual_col Column name of actual values.
#'
#' @importFrom dplyr .data
#' @export
compute_prediction_quantiles <- function(predictions, predicted_col, actual_col) {
  predictions %>%
    dplyr::select(dplyr::one_of(c(predicted_col, actual_col))) %>%
    dplyr::mutate(decile = cut(!!rlang::sym(predicted_col), unique(stats::quantile(
      !!rlang::sym(predicted_col), probs = seq(0, 1, 0.1)
    )), include.lowest = TRUE)) %>%
    dplyr::group_by(.data$decile) %>%
    dplyr::summarize(mean_predicted = mean(!!rlang::sym(predicted_col)),
                     mean_actual = mean(!!rlang::sym(actual_col)))
}
