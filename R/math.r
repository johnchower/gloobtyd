#' Log-Likelihood function for BG/BB model (single user)
#'
#' @param data An integer vector consisting of the recency/frequency statistics
#' for a single user, in the order (x, n, m)
#' @param params A named list of parameters for the log likelihood
#' function. (alpha, beta, gamma, delta)
loglikelihood0 <- function(data
                               , params){
  3
}

#' Log-Likelihood function for BG/BB model
#'
#' @param data An integer matrix where each row consists of the
#' recency/frequency statistics for a single user, in the order (x, n, m)
#' @param params A named list of parameters for the log likelihood
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
#' @param params A named list of parameters for the log likelihood
#' function. (alpha, beta, gamma, delta)
loglikelihood_d0 <- function(data
                               , params){
  2
}

#' Derivative of log-Likelihood function for BG/BB model
#'
#' @param data An integer matrix where each row consists of the
#' recency/frequency statistics for a single user, in the order (x, n, m)
#' @param params A named list of parameters for the log likelihood
#' function. (alpha, beta, gamma, delta)
loglikelihood_d <- function(data
                               , params){
  sum(
    apply(data, 1, function(row) loglikelihood_d0(row, params))
  )
}

#' Smoother function to map interval [0,1] to [0, infinity]
#'
#' @param x A number in interval [0,1] (inclusive)
smoother <- function(x){
  scaling_factor <- 2/pi
  scaled_x <- scaling_factor*x
  1/cos(scaled_x) - 1
}

#' Derivative of smoother function
#'
#' @param x A number in interval [0,1] (inclusive)
smoother_d <- function(x){
  scaling_factor <- 2/pi
  scaled_x <- scaling_factor*x
  scaling_factor*sin(scaled_x)/(cos(scaled_x))^2
}

#' Regularization function for BG/BB model
#'
#' @param params A vector of parameters for the log likelihood
#' function. c(alpha, beta, gamma, delta)
#' @param lambda The regularization factor
regularization <- function(params
                                , lambda = 0){
  lambda*smoother(
    sum(
      sapply(params
             , FUN = function(x) (x-.5)^2)
    )
  )
}

#' Derivative of regularization function for BG/BB model
#'
#' @param params A named list of parameters for the log likelihood
#' function. (alpha, beta, gamma, delta)
#' @param lambda The regularization factor
regularization_d <- function(params
                                  , lambda = 0){
  part1 <- lambda*smoother_d(    
    sum(
      sapply(params
             , FUN = function(x) (x-.5)^2)
    )
  )
                    
  part2 <- c(params[1]*2
             , params[2]*2
             , params[3]*2
             , params[4]*2)

  part1*part2
}
