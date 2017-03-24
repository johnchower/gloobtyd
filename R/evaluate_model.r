#' Calculate the comparison data frame for a given session duration data set,
#' for a given set of parameters, a session duration data set, and a run date.
#'
#' @param params A numeric vector of parameters for the log likelihood
#' function. (alpha, beta, gamma, delta)
#' @param sessDurData A data.frame of the form (user_id,
#' active_week_start_date) that gives the weeks in which each user was active.
#' @param cutoffDate A numeric date id in the form yyyymmdd. All data after this
#' date will be dropped. Must fall on a Monday
#' @param problem The classification problem to solve:
#' "churn" : Predict whether users are "alive" or "dead" at the cutoff date
#' (default)
#' "wau" : Predict whether users will use Gloo next week
#' @return comparison_data A data.frame of the form (user_id,
#' return_probability, showed_up). return_probability is the users' estimated
#' return probability, and showed_up is an indicator of whether they actually
#' showed up (1 = they showed up)
#' @importFrom dplyr group_by
#' @importFrom dplyr do
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
calculateComparisonData <- function(sessDurData
                                    , params
                                    , cutoffDate
                                    , problem = "churn"){
  cutoffYear <- substr(cutoffDate, 1, 4)
  cutoffMonth <- substr(cutoffDate, 5, 6)
  cutoffDay <- substr(cutoffDate, 7, 8)
  cutoffDate0 <- as.Date(paste(cutoffYear, cutoffMonth, cutoffDay, sep = "-"))
  if (weekdays(cutoffDate0) != "Monday"){
    stop("cutoffDate must fall on a Monday.")
  }
  recencyFrequency <- calculateRecencyFrequency(sessDurData, cutoffDate0)
  if (problem == "churn"){
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
      filter(active_week_start_date >= cutoffDate0) %>%
      group_by(user_id) %>%
      summarise(showed_up = 1) %>%
      select(user_id, showed_up)
  } else if (problem == "wau") {
    userReturnProbability <- recencyFrequency %>%
      group_by(user_id) %>%
      do({
        recency_frequency_stats <- c(.$x, .$n, .$m)
        data.frame(
          return_probability =
            estimateExpectedTransactions(n_star = 1
                                         , params = params
                                         , data = recency_frequency_stats)
        )
      }) %>%
      select(user_id, return_probability)
    userShowedUp <- sessDurData %>%
      filter(active_week_start_date == cutoffDate0) %>%
      group_by(user_id) %>%
      summarise(showed_up = 1) %>%
      select(user_id, showed_up)
  }
  userReturnProbability %>%
    left_join(userShowedUp, by = "user_id") %>%
    mutate(showed_up = ifelse(is.na(showed_up)
                              , 0
                              , 1) )
}

#' Calculate confusion matrix for a given set of comparison data
#'
#' @param threshold A number between 0 and 1. Any user whose estimated return
#' probability exceeds this threshold is predicted to return next week.
#' @param comparisonData A data.frame of the form (user_id,
#' return_probability, showed_up). return_probability is the users' estimated
#' return probability, and showed_up is an indicator of whether they actually
#' showed up (1 = they showed up)
#' @return A data frame of the form (prediction, showed_up, user_count) that
#' gives the number of users that fall into each category
calculateConfusionMatrix <- function(threshold = .5
                          , comparisonData){
  comparisonData %>%
    mutate(prediction = as.numeric(return_probability > threshold) ) %>%
    group_by(prediction, showed_up) %>%
    summarise(user_count = n()) %>%
    ungroup
}

#' Calculate ROC curve for a given set of comparison data
#'
#' @param threshold A number between 0 and 1. Any user whose estimated return
#' probability exceeds this threshold is predicted to return next week.
#' @param comparisonData A data.frame of the form (user_id,
#' return_probability, showed_up). return_probability is the users' estimated
#' return probability, and showed_up is an indicator of whether they actually
#' showed up (1 = they showed up)
#' @return A length-2 numeric vector of the form (1 - specificity, sensitivity)
#' giving the coordinates in ROC space corresponding to the given prediction
#' threshold.
calculateROC <- function(threshold = .5
                          , comparisonData){
  comparisonData_is_homogeneous <- 
    sum(comparisonData$showed_up) == 0 |
    sum(comparisonData$showed_up) == nrow(comparisonData)
  if (comparisonData_is_homogeneous) {
    stop("Sensitivity and specificity are undefined when all outcomes equal")
  }
  calculateConfusionMatrix(threshold, comparisonData) %>%
    summarise(
      sensitivity =
        sum(user_count * (showed_up == 1 & prediction == 1)) /
        sum(user_count * (showed_up == 1))
      , one_minus_specificity =
        sum(user_count * (showed_up == 0 & prediction == 1)) /
        sum(user_count * (showed_up == 0))
    ) %>% {
      c(.$one_minus_specificity, .$sensitivity)
    }
}

#' Calculate sensitivity + specificity statistic for a given set of comparison
#' data, at a given prediction probability threshold.
#'
#' @param threshold A number between 0 and 1. Any user whose estimated return
#' probability exceeds this threshold is predicted to return next week.
#' @param comparisonData A data.frame of the form (user_id,
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
                          , comparisonData){
  calculateROC(threshold, comparisonData) %>% {
    .[2] - .[1]
  }
}
