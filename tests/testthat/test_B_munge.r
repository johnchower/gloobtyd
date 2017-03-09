glootility::connect_to_redshift()

user_group_test <- user_id_sample_mini
run_date_test <- 20170308
sess_dur_data_mini <- fetchSessDurData(userGroup = user_group_test
                                      , runDate = run_date_test)

test_that("fetchSessDurData returns correct results.", {
  object_to_test  <- sess_dur_data_mini  
  expect_is(object = object_to_test
          , class = "data.frame")
  expect_gt(object = nrow(object_to_test)
            , expected = 0)
})

recency_frequency_mini <- 
  calculateRecencyFrequency(sessDurData = sess_dur_data_mini)

test_that("calculateRecencyFrequency returns correct results.", {
  object_to_test  <- recency_frequency_mini
  expect_is(object = object_to_test
          , class = "data.frame")
  expect_gt(object = nrow(object_to_test)
            , expected = 0)
})

RPostgreSQL::dbDisconnect(conn = redshift_connection$con)
