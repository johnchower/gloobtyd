#' Function to train model
#'
#' @param data An integer matrix where each row consists of the
#' recency/frequency statistics for a single user, in the order (x, n, m)
#' @param paramsInit A length-4 numeric vector of initial parameters for 
#' the log likelihood function. (alpha, beta, gamma, delta)
#' @param lambda Regularization parameter to pass to cost and cost_d
#' @param ... Additional arguments to pass to stats::optim
#' @importFrom stats optim
trainModel <- function(data
                       , paramsInit = c(1, 1, 1, 1)
                       , lambda = 0
                       , ...){
  optim(par = paramsInit
        , fn = cost
        , gr = cost_d
        , lambda = lambda
        , data = data
        , ...)
}
