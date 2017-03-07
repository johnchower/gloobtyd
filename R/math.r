#' Log-Likelihood function for BG/BB model (single user)
#'
#' @param data An integer vector consisting of the recency/frequency statistics
#' for a single user, in the order (x, n, m)
#' @param params A numeric vector of parameters for the log likelihood
#' function. (alpha, beta, gamma, delta)
loglikelihood0 <- function(data
                               , params){
  Alpha_param <- params[1]
  Beta_param <- params[2]
  Gamma_param <- params[3]
  Delta_param <- params[4]
  x <- data[1]
  n <- data[2]
  m <- data[3]
  if (sum(params <= 0) > 0){
    stop("params must all be greater than 0")
  }
  if (any(x < 0) | any(x - floor(x) > 0)){
    stop("x, n, and m must all be nonnegative integers")
  }
  if (x > m | m > n | x > n){
    stop("x, n, and m must satisfy x <= m <= n")
  }
  out <- beta(Alpha_param + x, Beta_param + n - x) *
          beta(Gamma_param, Delta_param + n) / (
          beta(Alpha_param, Beta_param) * beta(Gamma_param, Delta_param) )
  if (x < n){
    for (i in 0:(n - m - 1)){
      out <- out +
         beta(Gamma_param + 1, Delta_param + m + i) *
         beta(Alpha_param + x, Beta_param + m - x + i) / (
            beta(Alpha_param, Beta_param) * beta(Gamma_param, Delta_param)
         )
    }
  }
  out
}

#' Log-Likelihood function for BG/BB model
#'
#' @param data An integer matrix where each row consists of the
#' recency/frequency statistics for a single user, in the order (x, n, m)
#' @param params A numeric vector of parameters for the log likelihood
#' function. (alpha, beta, gamma, delta)
loglikelihood <- function(data
                               , params){
  sum(
    apply(data, 1, function(row) loglikelihood(row, params))
  )
}

#' Derivative of log-Likelihood function for BG/BB model (single user)
#'
#' @param data An integer vector consisting of the recency/frequency statistics
#' for a single user, in the order (x, n, m)
#' @param params A numeric vector of parameters for the log likelihood
#' function. (alpha, beta, gamma, delta)
#' @importFrom numDeriv grad
loglikelihood_d0 <- function(data
                               , params){
  numDeriv::grad(func = function(x){
                 loglikelihood0(params = x
                                , data = data)
                 }
                 , x = params
                 )
}

#' Derivative of log-Likelihood function for BG/BB model
#'
#' @param data An integer matrix where each row consists of the
#' recency/frequency statistics for a single user, in the order (x, n, m)
#' @param params A numeric vector of parameters for the log likelihood
#' function. (alpha, beta, gamma, delta)
loglikelihood_d <- function(data
                               , params){
  sum(
    apply(data, 1, function(row) loglikelihood_d0(row, params))
  )
}

#' Regularization function for BG/BB model
#'
#' @param params A vector of parameters for the log likelihood
#' function. c(alpha, beta, gamma, delta)
#' @param lambda The regularization factor
regularization <- function(params
                                , lambda = 0){
  lambda *
    sum(
      sapply(params
             , FUN = function(x) (log(x)) ^ 2)
    )
}

#' Derivative of regularization function for BG/BB model
#'
#' @param params A numeric vector of parameters for the log likelihood
#' function. (alpha, beta, gamma, delta)
#' @param lambda The regularization factor
regularization_d <- function(params
                                  , lambda = 0){
  lambda * (
    sum(
      sapply(params
             , FUN = function(x) 2 * log(x) / x)
    )
  )
}
