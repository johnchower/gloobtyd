#' Fetch session duration data for a given group of users
#'
#' @param userGroup A numeric vector of user_ids. Only user_ids belonging to this
#' set will be included in the analysis. 
#' @param runDate A numeric date id in the form yyyymmdd. All data after this
#' date will be dropped.
#' @param con A database connection object to fetch the results from.
#' @return data.frame of the form (user_id, active_week_start_date)
#' @importFrom magrittr %>%
#' @importFrom RPostgreSQL dbGetQuery
fetchSessDurData <- function(userGroup = NULL
                             , runDate = as.numeric(
                                          gsub(pattern = "-"
                                               , replacement = ""
                                               , x = Sys.Date()))
                             , con = redshift_connection$con){
  runYear <- substr(runDate, 1, 4)
  runMonth <- substr(runDate, 5, 6)
  runDay <- substr(runDate, 7, 8)
  runDate0 <- as.Date(paste(runYear, runMonth, runDay, sep = "-"))
  weekBefore <- seq.Date(from = runDate0 - 6
                          , to =  runDate0
                          , by = 1)
  runDate <- weekBefore[which(weekdays(weekBefore) == "Monday")] %>%
    as.character %>% {
    gsub(pattern = "-"
         , replacement = ""
         , x = .)
    }
  if (length(userGroup) == 1){
    stop("'userGroup' must be either NULL or a group of at least 2 users")
  } else if (is.null(userGroup)){
    userGroupQuery <-
      paste0("SELECT DISTINCT ud.id "
             , "FROM user_dimensions ud "
             , "LEFT JOIN user_platform_action_facts upaf "
             , "on upaf.user_id=ud.id "
             , "WHERE ud.email IS NOT NULL "
             , "AND upaf.platform_action=\'Account Created\' ")
  } else {
    usersChar <- paste(userGroup, collapse = ",")
    userGroupQuery <-
      paste0(
        "SELECT DISTINCT id FROM user_dimensions WHERE id IN ("
        , usersChar
        , ")"
      )
  }
  runDateQuery <- paste0("SELECT id as date_id FROM date_dim where id="
                         , runDate)
  fetchQuery <- query_session_duration_data_sub %>% {
    gsub(pattern = "xyz_userGroupQuery_xyz"
         , replacement = userGroupQuery
         , x = .)
  } %>% {
    gsub(pattern = "xyz_runDateQuery_xyz"
         , replacement = runDateQuery
         , x = .)
  }
  RPostgreSQL::dbGetQuery(conn = con
                          , statement = fetchQuery)
}

#' Calculate recency and frequency statistics from session duration data
#'
#' @param sessDurData A data.frame of the form (user_id,
#' active_week_start_date) that gives the weeks in which each user was active.
#' @param runDate A numeric date id in the form yyyymmdd. All data after this
#' date will be dropped.
#' @return A data.frame of the form (user_id, x, n, m). Here, x represents the
#' number of repeat transactions made by the user, n represents the number of
#' repeat transaction opportunities that the user had, and m represents the
#' index of the most recent transaction. (M is also called t_x in the
#' literature.)
calculateRecencyFrequency <- function(sessDurData
                                     , runDate = Sys.Date()){
  runDate0 <- runDate
  weekBefore <- seq.Date(from = runDate0 - 6
                          , to =  runDate0
                          , by = 1)
  runDate <- weekBefore[which(weekdays(weekBefore) == "Monday")]
  minRunDate <- min(sessDurData$active_week_start_date)
  dateSeq <- seq.Date(from = minRunDate
                       , to = runDate
                       , by = 7)
  dateSeq <- data.frame(active_week_start_date = dateSeq
                        , seq_number = (length(dateSeq) - 1):0)
  recency_frequency_mini_test <- sessDurData %>%
    left_join(dateSeq, by = "active_week_start_date") %>%
    arrange(user_id, active_week_start_date) %>%
    group_by(user_id) %>%
    mutate(reverse_seq_number = max(seq_number) - seq_number) %>%
    filter(active_week_start_date < runDate) %>%
    summarise(x = length(unique(active_week_start_date)) - 1
              , n = max(seq_number) - 1
              , m = max(reverse_seq_number))
}
