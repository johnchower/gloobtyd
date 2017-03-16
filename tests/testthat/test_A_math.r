cat("\nRunning test_A_math.r\n")
library(numDeriv)

test_that("Log likelihood function catches errors.", {
  test_n <- 20
  test_m <- 10
  test_x <- 5
  testdata <- c(test_x, test_n, test_m)
  testparams <- c(0, -1, 1, .5)
  expect_error(object = loglikelihood0(data = testdata, params = testparams)
               , regexp = "params must all be greater than 0")
  testparams <- rep(.5, times = 4)
  testdata <- c(.5, 10, 4)
  expect_error(object = loglikelihood0(data = testdata, params = testparams)
               , regexp = "x, n, and m must all be nonnegative integers")
  testdata <- c(5,  10, 15)
  expect_error(object = loglikelihood0(data = testdata, params = testparams)
               , regexp = "x, n, and m must satisfy x <= m <= n")
})

test_that("Derivative of log-likelihood function was computed properly", {
  error <- 0
  num_iterations <- 100
  for (j in 1:num_iterations){
    test_n <- rpois(n <- 1, lambda = 20)
    test_m <- sample(x = 1:test_n, size = 1)
    test_x <- sample(x = 0:(test_m - 1), size = 1)
    testdata <- c(test_x, test_n, test_m)
    testparams <- c(abs(rnorm(1))
                  , abs(rnorm(1))
                  , abs(rnorm(1))
                  , abs(rnorm(1))
                  )
    expected_deriv <- numDeriv::grad(func = function(x){
                             loglikelihood0(params = x
                                           , data = testdata)
                           }
                           , x = testparams
                           )
    actual_deriv <- loglikelihood_d0(params = testparams
                                    , data = testdata)
    iter_error <- sum( (actual_deriv - expected_deriv) ^ 2)
    error <- error + iter_error
  }
  expect_equal(object = error
               , expected = 0)
})

test_that("Regularization function has correct properties", {
  testparams <- rep(1, times = 4)
  testthat::expect_equal(regularization(testparams)
                         , 0)
  actual_deriv <- numDeriv::grad(func = function(x){
                           regularization(params = c(x[1]
                                                        , x[2]
                                                        , x[3]
                                                        , x[4]))
                         }
                         , x = c(testparams[1]
                                 , testparams[2]
                                 , testparams[3]
                                 , testparams[4])
                        )
  expected_deriv <- rep(0, times = 4)
  expect_equal(object = actual_deriv
               , expected = expected_deriv)
})

test_that("Derivative of regularization function was computed properly", {
  error <- 0
  num_iterations <- 100
  for (i in 1:num_iterations){
    testparams <- c(abs(rnorm(1))
                  , abs(rnorm(1))
                  , abs(rnorm(1))
                  , abs(rnorm(1))
                  )
    expected_deriv <- numDeriv::grad(func = function(x){
                             regularization(params = c(x[1]
                                                          , x[2]
                                                          , x[3]
                                                          , x[4]))
                           }
                           , x = c(testparams[1]
                                   , testparams[2]
                                   , testparams[3]
                                   , testparams[4])
                          )
    actual_deriv <- regularization_d(testparams)
    iter_error <- sum( (actual_deriv - expected_deriv) ^ 2)
    error <- error + iter_error
  }
  expect_equal(object = error
               , expected = 0)
})

test_that("estimateReturnProbability returns answer.", {
  testparams <- rep(1, times = 4)
  testdata <- c(0, 1000, 0)
  testtolerance <- 10 ^ (-5)
  expect_equal(object = estimateReturnProbability(data = testdata
                                                  , params = testparams)
               , expected = 0
               , tolerance = testtolerance)
  testdata <- c(100000, 100000, 100000)
  expect_equal(object = estimateReturnProbability(data = testdata
                                                  , params = testparams)
               , expected = 1
               , tolerance = testtolerance)
})
