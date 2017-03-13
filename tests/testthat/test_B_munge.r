library(dplyr)
csv_loc <- "~/Projects/gloobtyd/inst/exdata/sess_dur_data_full.csv"
if (is.null(csv_loc)){
  glootility::connect_to_redshift()
}

user_group_test <- user_id_sample_mini
run_date_test <- 20170308
effective_run_date_test <- as.Date("2017-03-06")
sess_dur_data_mini <-
  fetchSessDurData(
    userGroup = user_group_test
    , runDate = run_date_test
    , csvLoc = csv_loc)

test_that("fetchSessDurData returns correct results.", {
  object_to_test  <- sess_dur_data_mini
  expect_is(object = object_to_test
          , class = "data.frame")
  expect_gt(object = nrow(object_to_test)
            , expected = 0)
  expected_colnames <- c("user_id"
                         , "active_week_start_date")
  actual_colnames <- colnames(object_to_test)
  expect_equal(object = actual_colnames[order(actual_colnames)]
               , expected = expected_colnames[order(expected_colnames)])
  expect_equal(object = is(object_to_test$user_id)[1]
               , "numeric")
  expect_equal(object = is(object_to_test$active_week_start_date)[1]
               , "Date")
  expect_equal(object =
                 sum(object_to_test$active_week_start_date
                       >= effective_run_date_test)
               , expected = 0)
})

recency_frequency_mini <-
  calculateRecencyFrequency(sessDurData = sess_dur_data_mini)

test_that("calculateRecencyFrequency returns correct results.", {
  object_to_test  <- recency_frequency_mini
  expected_object <- recency_frequency_mini_test
  expect_is(object = object_to_test
          , class = "data.frame")
  expect_gt(object = nrow(object_to_test)
            , expected = 0)
  expected_colnames <- colnames(expected_object)
  actual_colnames <- colnames(object_to_test)
  expect_equal(object = actual_colnames[order(actual_colnames)]
               , expected = expected_colnames[order(expected_colnames)])
})

test_that("calculateRecencyFrequency never violates constraint x <= m <= n.", {
  object_to_test <- calculateRecencyFrequency(
      sessDurData = fetchSessDurData(csvLoc = csv_loc)
      , runDate = effective_run_date_test
  ) %>%
  slice(sample(1:nrow(.), size = 1000))

  expect_is(object = object_to_test
          , class = "data.frame")
  expect_gt(object = nrow(object_to_test)
            , expected = 0)
  constraints_violated <- object_to_test %>%
    mutate(x_greater_than_m = (x > m)
           , m_greater_than_n = (m > n))
  m_greater_than_n <- sum(constraints_violated$m_greater_than_n)
  x_greater_than_m <- sum(constraints_violated$x_greater_than_m)
  expect_equal(object = x_greater_than_m
               , expected = 0)
  expect_equal(object = m_greater_than_n
               , expected = 0)
})

test_that("calculateRecencyFrequency returns no NA or NULL values.", {
  w <- as.Date("2015-10-05")
  object_to_test <- calculateRecencyFrequency(sessDurData = test_data_cRF_NA
                                              , runDate = w)
  x_na <- sum(is.na(object_to_test$x))
  n_na <- sum(is.na(object_to_test$n))
  m_na <- sum(is.na(object_to_test$m))
  expect_equal(object = x_na
               , expected = 0)
  expect_equal(object = n_na
               , expected = 0)
  expect_equal(object = m_na
               , expected = 0)
})

if (is.null(csv_loc)){
  RPostgreSQL::dbDisconnect(conn = redshift_connection$con)
}
