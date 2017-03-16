#' Calculate the comparison data frame for a given session duration data set,
#' for a given set of parameters, a session duration data set, and a run date.
#'
#' @param params A numeric vector of parameters for the log likelihood
#' function. (alpha, beta, gamma, delta)
#' @param sessDurData A data.frame of the form (user_id,
#' active_week_start_date) that gives the weeks in which each user was active.
#' @param cutoffDate A numeric date id in the form yyyymmdd. All data after this
#' date will be dropped. Must fall on a Monday
#' @return comparison_data A data.frame of the form (user_id,
#' return_probability, showed_up). return_probability is the users' estimated
#' return probability, and showed_up is an indicator of whether they actually
#' showed up (1 = they showed up)
calculateComparisonData <- function(sessDurData
                                    , params
                                    , cutoffDate){
  cutoffYear <- substr(cutoffDate, 1, 4)
  cutoffMonth <- substr(cutoffDate, 5, 6)
  cutoffDay <- substr(cutoffDate, 7, 8)
  cutoffDate0 <- as.Date(paste(cutoffYear, cutoffMonth, cutoffDay, sep = "-"))
  if (weekdays(cutoffDate0) != "Monday"){
    stop("cutoffDate must fall on a Monday.")
  }
  recencyFrequency <- calculateRecencyFrequency(sessDurData, cutoffDate0)
  userReturnProbability <- recencyFrequency %>%
    group_by(user_id) %>%
    do({
      recency_frequency_stats <- c(.$x, .$n, .$m)
      data.frame(
        return_probability =
          estimateReturnProbability(params = params
                                    , data = recency_frequency_stats)
      )
    }) %>%
    select(user_id, return_probability)
  userShowedUp <- sessDurData %>%
    filter(active_week_start_date == cutoffDate0) %>%
    mutate(showed_up = 1) %>%
    select(user_id, showed_up)
  userReturnProbability %>%
    left_join(userShowedUp, by = "user_id") %>%
    mutate(showed_up = ifelse(is.na(showed_up)
                              , 0
                              , 1) )
}

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
