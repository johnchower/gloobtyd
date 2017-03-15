#' Calculate sensitivity + specificity statistic for a given result set, at a
#' given prediction probability threshold.
#'
#' @param threshold A number between 0 and 1. Any user whose estimated return
#' probability exceeds this threshold is predicted to return next week.
#' @param comparison_data A data.frame of the form (user_id,
#' return_probability, showed_up). return_probability is the users' estimated
#' return probability, and showed_up is an indicator of whether they actually
#' showed up (1 = they showed up)
#' @return A number between -1  and 1 representing sensitivity + specificity
#' - 1. Higher numbers represent accurate forecasts
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
calculate_sss <- function(threshold = .5
                          , comparison_data){
  comparison_data_is_homogeneous <- 
    sum(comparison_data$showed_up) == 0 |
    sum(comparison_data$showed_up) == nrow(comparison_data)
  if (comparison_data_is_homogeneous) {
    stop("Sensitivity and specificity are undefined when all users did the same thing.")
  }
  comparison_data %>%
    mutate(prediction = as.numeric(return_probability > threshold) ) %>%
    group_by(prediction, showed_up) %>%
    summarise(user_count = n()) %>%
    ungroup %>%
    summarise(
      sensitivity =
        sum(user_count * (showed_up == 1 & prediction == 1)) /
        sum(user_count * (showed_up == 1))
      , one_minus_specificity =
        sum(user_count * (showed_up == 0 & prediction == 1)) /
        sum(user_count * (showed_up == 0))
    ) %>% {
      .$sensitivity - .$one_minus_specificity
    }
}
