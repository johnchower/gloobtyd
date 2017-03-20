library(pROC)
library(dplyr)
library(ggplot2)
devtools::load_all()

proj_root <- rprojroot::find_root(rprojroot::has_dirname("gloobtyd"))
path_to_data <- "inst/exdata/sess_dur_data_full.csv"
full_path_to_data <- paste(proj_root, path_to_data, sep = "/")

# Load sess_dur_data

sess_dur_data <- fetchSessDurData(csvLoc = full_path_to_data)

# Split users according to the week of their first session
user_id_first_week <- sess_dur_data %>%
  group_by(user_id) %>%
  summarise(first_week = min(active_week_start_date)) %>%
  ungroup %>%
  select(user_id, first_week)

# Within each week, split users into training, test and validation sets
# Training = 0, test = 1, validation = 2
set.seed(0)
user_id_set <- user_id_first_week %>%
  group_by(first_week) %>%
  mutate(set = sample(0:2, size = n()
                      , replace = T
                      , prob = c(.6, .2, .2))
  ) %>%
  ungroup %>%
  select(user_id, set)

# Fix a date, date_training_cutoff, to split the data into training and test
# dates.
date_training_cutoff <- as.Date("2016-08-29")
date_training_cutoff_id <- gsub(pattern = "-"
                                , replacement = ""
                                , x = date_training_cutoff) %>%
  as.numeric

# Prepare data for training
recency_frequency_data <- sess_dur_data %>%
  select(user_id, active_week_start_date) %>%
  calculateRecencyFrequency(runDate = date_training_cutoff)

training_matrix <- recency_frequency_data %>%
  left_join(user_id_set, by = "user_id") %>%
  filter(set == 0) %>%
  select(x, n, m) %>%
  as.matrix

sess_dur_data_validation <- sess_dur_data %>%
  left_join(user_id_set, by = "user_id") %>%
  filter(set == 1)

sess_dur_data_test <- sess_dur_data %>%
  left_join(user_id_set, by = "user_id") %>%
  filter(set == 2)

lambdas_to_try <- c(.0025*1:20)
tolerance <- 10 ^ (-5)
# lambda_trained_params <- data.frame()
for (lamb_da in lambdas_to_try){
  trainStart <- Sys.time()
  new_row <- optim(par = c(1, 1, 1, 1)
                   , fn = cost
                   , lambda = lamb_da
                   , lower = rep(tolerance, times = 4)
                   , method = "L-BFGS-B"
                   , data = training_matrix) %>%
    {.$par} %>% {
      data.frame(week_beginning = date_training_cutoff
                 , lambda = lamb_da
                 , alpha = .[1]
                 , beta = .[2]
                 , gamma = .[3]
                 , delta = .[4]
                 , stringsAsFactors = F)
    }
  trainEnd <- Sys.time()
  print(trainEnd - trainStart)
  lambda_trained_params <- rbind(lambda_trained_params, new_row)
}

# lambda_auc <- data.frame()
for (lamb_da in lambdas_to_try){
  params_current <- lambda_trained_params %>%
    filter(lambda == lamb_da) %>% {
      c(.$alpha, .$beta, .$gamma, .$delta)
    }
  comparison_data <- calculateComparisonData(sessDurData = sess_dur_data_validation
                          , params = params_current
                          , cutoffDate = date_training_cutoff_id)
  roc_obj <- pROC::roc(comparison_data$showed_up, comparison_data$return_probability)
  lambda_auc_new <- data.frame(lambda = lamb_da
                               , AUC = auc(roc_obj))
  lambda_auc <- rbind(lambda_auc, lambda_auc_new)
}
lambda_auc %>%
  filter(lambda > 0, lambda <= 0.1) %>%
  ggplot(aes(x = lambda, y = AUC)) +
  geom_point()

best_lambda <- .05
params_current <- lambda_trained_params %>%
  filter(lambda == best_lambda) %>% {
    c(.$alpha, .$beta, .$gamma, .$delta)
  }
comparison_data <- calculateComparisonData(sessDurData = sess_dur_data_test
                        , params = params_current
                        , cutoffDate = date_training_cutoff_id)
roc_test <- pROC::roc(comparison_data$showed_up, comparison_data$return_probability)
plot(roc_test)
sss_fcn <- function(threshold) calculate_sss(threshold, comparison_data)
best_sss <- optimize(f = sss_fcn, interval = c(0,1), maximum = T)
optimal_threshold <- best_sss$maximum
calculateROC(threshold = optimal_threshold, comparison_data)
