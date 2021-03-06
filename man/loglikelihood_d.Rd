% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/math.r
\name{loglikelihood_d}
\alias{loglikelihood_d}
\title{Derivative of log-Likelihood function for BG/BB model}
\usage{
loglikelihood_d(params, data)
}
\arguments{
\item{params}{A numeric vector of parameters for the log likelihood
function. (alpha, beta, gamma, delta)}

\item{data}{An integer matrix where each row consists of the
recency/frequency statistics for a single user, in the order (x, n, m)}
}
\description{
Derivative of log-Likelihood function for BG/BB model
}
