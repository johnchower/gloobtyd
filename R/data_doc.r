#' A string containing the session duration data query.
#' 
#' @format A length-one character vector.
"query_session_duration_data"

#' A string containing the session duration data query, with placeholders to
#' substitue userGroup and runDate queries.
#' 
#' @format A length-one character vector.
"query_session_duration_data_sub"

#' A string containing the user ids query.
#' 
#' @format A length-one character vector.
"query_get_user_ids"

#' A random sample of 10 user ids.
#' 
#' @format numeric
"user_id_sample_mini"

#' A random sample of 100 user ids.
#' 
#' @format numeric
"user_id_sample_small"

#' A data.frame containing the recency/frequency statistics for
#' user_id_sample_mini
#' 
#' @format data.frame
"recency_frequency_mini_test"

#' A data frame containing the session duration data that produces the NA error
#' caused by calculateRecencyFrequencyData, with rundate = "2016-10-05".
#'
#' @format data.frame
"test_data_cRF_NA"

#' Data frame for testing the output of the calculate_sps function
#'
#' @format data.frame
"comparison_test_data_perfect_prediction"

#' Data frame for testing the output of the calculate_sps function
#'
#' @format data.frame
"comparison_test_data_terrible_prediction"

#' Data frame for testing the output of the calculate_sps function
#'
#' @format data.frame
"comparison_test_data_random_prediction"

#' Data frame for testing the output of the calculate_sps function
#'
#' @format data.frame
"comparison_test_data_homogeneous"
