cat("\nRunning test_C_evaluate_model.r\n")
library(dplyr)

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
  gt_neg1 <- c()
  lt_1 <- c()
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
    new_lt_1 <- (test_result > 1)
    new_gt_neg1 <- (test_result < -1)
    lt_1 <- c(lt_1, new_lt_1)
    gt_neg1 <- c(gt_neg1, new_gt_neg1)
    # print(comparison_test_data)
    # print(new_lt_1)
    # print(new_gt_neg1)
    if (is.na(new_lt_1) | is.na(new_gt_neg1)) break
  }
  expect_equal(object = sum(gt_neg1)
               , expected = 0)
  expect_equal(object = sum(lt_1)
               , expected = 0)
})

test_that("calculate_sss throws errors correctly.", {
  expect_error(
    calculate_sss(comparison_data = comparison_test_data_homogeneous)
    , regexp = 
      "Sensitivity and specificity are undefined when all users did the same thing."
  )
})
