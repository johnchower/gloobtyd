#' Fetch session duration data for a given group of users
#'
#' @param userGroup A numeric vector of user_ids. Only user_ids belonging to this
#' set will be included in the analysis. 
#' @param runDate A numeric date id in the form yyyymmdd. All data after this
#' date will be dropped.
#' @param con A database connection object to fetch the results from.
#' @importFrom magrittr %>%
#' @importFrom RPostgreSQL dbGetQuery
fetchSessDurData <- function(userGroup
                             , runDate
                             , con = redshift_connection$con){
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
#' @return A data.frame of the form (user_id, x, n, m). Here, x represents the
#' number of repeat transactions made by the user, n represents the number of
#' repeat transaction opportunities that the user had, and m represents the
#' index of the most recent transaction. (M is also called t_x in the
#' literature.)
calculateRecencyFrequency <- function(sessDurData){
 333
}
