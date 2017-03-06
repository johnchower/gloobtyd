library(numDeriv)

test_that("Regularization function has correct properties", {
  params = list(alpha = .5
                , beta = .5
                , gamma = .5
                , delta = .5)
  testthat::expect_equal(regularization(params)
                         , 0)
})

test_that("Derivative of regularization function was computed properly", {
  error <- 0
  num_iterations <- 100
  for(i in 1:num_iterations){
    testparams = c(runif(n = 1)
                  , runif(n = 1)
                  , runif(n = 1)
                  , runif(n = 1))
    expected_deriv <- grad(func = function(x){
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
    iter_error <- sum((actual_deriv - expected_deriv)^2)
    error <- error + iter_error
  }
  expect_equal(object = error
               , expected = 0)
})
