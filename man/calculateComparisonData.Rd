% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_model.r
\name{calculateComparisonData}
\alias{calculateComparisonData}
\title{Calculate the comparison data frame for a given session duration data set,
for a given set of parameters, a session duration data set, and a run date.}
\usage{
calculateComparisonData(sessDurData, params, cutoffDate, problem = "churn")
}
\arguments{
\item{sessDurData}{A data.frame of the form (user_id,
active_week_start_date) that gives the weeks in which each user was active.}

\item{params}{A numeric vector of parameters for the log likelihood
function. (alpha, beta, gamma, delta)}

\item{cutoffDate}{A numeric date id in the form yyyymmdd. All data after this
date will be dropped. Must fall on a Monday}

\item{problem}{The classification problem to solve:
"churn" : Predict whether users are "alive" or "dead" at the cutoff date
(default)
"wau" : Predict whether users will use Gloo next week}
}
\value{
comparison_data A data.frame of the form (user_id,
return_probability, showed_up). return_probability is the users' estimated
return probability, and showed_up is an indicator of whether they actually
showed up (1 = they showed up)
}
\description{
Calculate the comparison data frame for a given session duration data set,
for a given set of parameters, a session duration data set, and a run date.
}
