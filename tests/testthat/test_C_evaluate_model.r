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
})
