cat("\nRunning test_C_evaluate_model.r\n")
library(dplyr)

set.seed(0)
test_params <- c(rlnorm(1) 
                 , rlnorm(1) 
                 , rlnorm(1) 
                 , rlnorm(1)) 
test_run_date <- 20160829
test_run_date_as_date <- as.Date("2016-08-29")
test_min_date <- as.Date("2016-01-04")
test_sess_dur_data_user_id <- 1:100
test_sess_dur_data <- data.frame()
for (user in test_sess_dur_data_user_id){
  date_seq <- seq.Date(from = test_min_date
                       , to = test_run_date_as_date
                       , by = 7)
  N <- sample(1:length(date_seq), size = 1)
  date_sample <- sample(x = date_seq
                        , size = N
                        , replace = F)
  new_user_id <- rep(user, times = N)
  new_data <- data.frame(user_id = new_user_id
                         , active_week_start_date = date_sample
                         , stringsAsFactors = F)
  test_sess_dur_data <- rbind(test_sess_dur_data, new_data)
}

test_that("calculateComparisonData throws errors properly.", {
  expect_error(calculateComparisonData(sessDurData = test_sess_dur_data
                                       , params = test_params
                                       , cutoffDate = 20160901)
               , regexp = "cutoffDate must fall on a Monday.")
})

test_that("calculateComparisonData returns results in correct form.", {
  object_to_test <- calculateComparisonData(sessDurData = test_sess_dur_data
                                            , params = test_params
                                            , cutoffDate = test_run_date)
  expect_is(object = object_to_test
            , class = "data.frame")
  expect_gt(object = nrow(object_to_test)
            , expected = 0)
  colnames_to_test <- colnames(object_to_test)
  expected_colnames <- c("user_id", "return_probability", "showed_up")
  expect_equal(object = colnames_to_test[order(colnames_to_test)]
               , expected = expected_colnames[order(expected_colnames)])
  expect_is(object_to_test$user_id
            , "integer")
  user_ids_before <- unique(test_sess_dur_data$user_id)
  user_ids_after <- unique(object_to_test$user_id)
  expect_equal(object = user_ids_after[order(user_ids_after)]
               , expected = user_ids_before[order(user_ids_before)] )
  expect_is(object_to_test$return_probability
            , "numeric")
  expect_is(object_to_test$showed_up
            , "numeric")
  expect_equal(sum(object_to_test$return_probability < 0)
               , 0)
  expect_equal(sum(object_to_test$return_probability > 1)
               , 0)
  values_showed_up_to_test <- unique(object_to_test$showed_up) 
  values_showed_up_expected <- c(0, 1)
  expect_equal(values_showed_up_to_test[order(values_showed_up_to_test)]
               , values_showed_up_expected)
})

test_that("calculate_sss throws errors correctly.", {
  expect_error(
    calculate_sss(comparison_data = comparison_test_data_homogeneous)
    , regexp = 
      "Sensitivity and specificity are undefined when all users did the same thing."
  )
})

test_that("calculate_sss returns correct results.", {
  object_to_test <- 
    calculate_sss(comparison_data = comparison_test_data_terrible_prediction)
  expect_equal(object = object_to_test
               , expected = -1)
  object_to_test <- 
    calculate_sss(comparison_data = comparison_test_data_random_prediction)
  expect_equal(object = object_to_test
               , expected = 0)
  object_to_test <- 
    calculate_sss(comparison_data = comparison_test_data_perfect_prediction)
  expect_equal(object = object_to_test
               , expected = 1)
  lt_neg1 <- c()
  gt_1 <- c()
  test_data_size <- 5
  for (i in 1:100){
    test_threshold <- runif(1)
    comparison_test_data <- data.frame(user_id = 1:test_data_size
                                 , return_probability = runif(test_data_size)
                                 , showed_up = sample(x = 0:1
                                                      , size = test_data_size
                                                      , replace = T) )
    comparison_test_data_is_homogeneous <- 
      sum(comparison_test_data$showed_up) == 0 |
      sum(comparison_test_data$showed_up) == nrow(comparison_test_data)
    if (comparison_test_data_is_homogeneous) {
      comparison_test_data <- 
        mutate(comparison_test_data
               , showed_up = c(1, rep(0, times = nrow(comparison_test_data) - 1)) )
    }
    test_result <- calculate_sss(threshold = test_threshold
                                 , comparison_data = comparison_test_data)
    new_gt_1 <- (test_result > 1)
    new_lt_neg1 <- (test_result < -1)
    gt_1 <- c(gt_1, new_gt_1)
    lt_neg1 <- c(lt_neg1, new_lt_neg1)
  }
  expect_equal(object = sum(lt_neg1)
               , expected = 0)
  expect_equal(object = sum(gt_1)
               , expected = 0)
})

