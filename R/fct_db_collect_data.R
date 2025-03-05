

#' Slice database rows
#'
#' Selects rows in a database with the maximum value of one or more variables
#' within every group, and collects the resulting table in memory. Mainly
#' designed to be used with date-time variables, thus selecting on latest time,
#' but can be used with any variable for row selection.
#'
#' Needs `dbplyr` version 2.4.0 or later, otherwise the `.data` pronoun will not
#' work within `dplyr::slice_max()` when used on a SQL database connection (see
#' [this issue](https://github.com/tidyverse/dbplyr/issues/1294)).
#'
#' In the application, we slice for two date-time variables to
#' collect active review data. The first date-time variable is the `timestamp`
#' added after reviewing a data point in the application. This allows
#' us to keep an audit trail, by keeping data of all review actions in the
#' database, but only selecting the latest review data each timestamp allows us
#' to add an audit trail of the review actions in the main database and allow
#' the user to update their review, while only showing the main data in the
#' application.
#'
#' @param data Can either be a data frame, or a character string with the file
#'   path to the database.
#' @param db_table Character string. Name of the table to collect. Will only be
#'   used if `data` is a character string to a database.
#' @param slice_vars Character vector. Names of the variables that will be used
#'   to slice the data frame. Note that the order matters: Slicing will occur
#'   for each variable in this vector successively,
#' @param group_vars Character vector. Variable names of the variables to
#'   perform the grouping on.
#' @param warn_duplicates Logical. Whether to warn if duplicate rows are
#'   detected after performing the grouping.
#'
#' @return A data frame with the same or less amount of rows as the data frame
#'   in the database.
#'
#' @export
#'
#' 
db_slice_rows <- function(
    data,
    db_table = "all_review_data",
    slice_vars = c("timestamp", "edit_date_time"),
    group_vars = c("subject_id", "event_name", "item_group",
                   "form_repeat", "item_name"),
    warn_duplicates = TRUE
){
  stopifnot(inherits(data, c("character", "data.frame", "tbl_sql")))   
  stopifnot(is.character(db_table))
  stopifnot(is.character(slice_vars))
  stopifnot(is.character(group_vars))
  
  if(is.character(data)){
    con <- get_db_connection(data)
    df <- dplyr::tbl(con, db_table)  
  } else(
    df <- data
  )
  for(i in seq_along(slice_vars)){
    # adds all timevars that come after the one used for slicing to the grouping var:
    groups <- c(group_vars, slice_vars[-c(1:i)])
    df <- dplyr::slice_max(df, .data[[ slice_vars[i] ]], by = dplyr::all_of(groups))
  }
  if(inherits(df, "tbl_sql")){
    df <- dplyr::collect(df) 
  }
  if(warn_duplicates){
    # check for duplicate rows and give a warning if so:
    duplicate_rows <- dplyr::summarize(
      df, 
      n_rows = dplyr::n(), 
      .by = dplyr::all_of(c(group_vars, slice_vars))
    ) |> 
      dplyr::filter(n_rows > 1)
    if(nrow(duplicate_rows) != 0 ){
      warning("Duplicate rows detected: \n", 
              "\nAre all grouping variables declared correctly?", 
              "\nThe last row of all duplicates will be selected.")
    }
  }
  dplyr::slice_tail(df, n = 1, by = dplyr::all_of(c(group_vars, slice_vars)))
}

db_get_table <- function(db_path, db_table = "all_review_data") {
  stopifnot(is.character(db_path))
  stopifnot(is.character(db_table))
  
  con <- get_db_connection(db_path)
  sql <- "SELECT * FROM ?db_table"
  query <- DBI::sqlInterpolate(con, sql, db_table = db_table[1])
  DBI::dbGetQuery(con, query)
}


#' Collect query data
#'
#' Collects query data from a remote database.
#'
#' @param db_path A character string with the file path to the database.
#' @param time_var Character string with the name of the timestamp variable.
#' @param query_id Character string with the name of the query id variable
#' @param query_follow_up_number Character string. Follow-up number of the
#'   query. Will be incremented for follow-up message on the initial query.
#' @param resolved_var Character string with the name of the variable that shows
#'   whether the query is resolved ('Yes') or not ('No').
#' @param resolved_date_var Character string. Date-time when the query was
#'   marked as resolved.
#' @inheritParams db_slice_rows
#'
#' @return A data frame collected from a remote database.
#' @export
#' 
collect_query_data <- function(
    db_path,
    db_table = "query_data",
    time_var = "timestamp", 
    query_id = "query_id",
    query_follow_up_number = "n",
    resolved_var = "resolved",
    resolved_date_var = "resolved_date"
){
  db_slice_rows(db_path, db_table = db_table, slice_vars = time_var, 
                group_vars = c(query_id, query_follow_up_number)) |> 
    dplyr::group_by(.data[[query_id]]) |> 
    dplyr::mutate(
      "{resolved_var}" := ifelse(any(.data[[resolved_var]] == "Yes"), "Yes", .data[[resolved_var]]),
      "{resolved_date_var}" := ifelse(.data[[resolved_date_var]] == "", NA_character_, .data[[resolved_date_var]]) |> 
        as.character() # otherwise empty missing vectors will be converted to factors
      ) |> 
    tidyr::fill(dplyr::all_of(resolved_date_var), .direction =  "updown") |> 
    dplyr::ungroup() 
}
