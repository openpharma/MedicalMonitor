#' Connect to database
#' 
#' Small helper function to connect to database. This way, the connection 
#' arguments are in one place, which makes it easier to change the database 
#' driver in the entire app if needed. 
#'
#' @param path Character vector. Path to the database.
#' @param drv Database driver to use. Defaults to `RSQLite::SQLite()`. 
#' @param envir Environment to make the connection available in. 
#'
#' @return A database connection. 
#' @export
#'
#' @examples get_db_connection(tempfile(fileext = ".sqlite"))
get_db_connection <- function(
    path = db_path,
    drv = RSQLite::SQLite(),
    envir = parent.frame()
){
  withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), path), 
    .local_envir =  envir
  )
}

#' Connect temporarily to a database.
#'
#' @param db_path Path to the database.
#' @param code Code to execute with the temporary connection.
#' @param drv The DB driver to use. Standard the SQLite driver.
#'
#' @return Nothing will be returned by default.
#' @export
#'
#' @examples
#'  library(DBI)
#'  db_temp_connect(tempfile(), DBI::dbWriteTable(con, "test_table", mtcars))
#'  
db_temp_connect <- function(db_path, code, drv = RSQLite::SQLite()){
  withr::with_db_connection(
    con = list(con = DBI::dbConnect(drv, db_path)), 
    code = code 
  )
}

#' Create app database
#'
#' Creates application database. To create a database with all data flagged as
#' 'new', use the default settings of `reviewed`, `reviewer`, and `status`.
#'
#' @param data A data frame with review data (Usually created with
#'   [get_review_data()]).
#' @param db_path A character vector with the path to the database to be
#'   created.
#' @param reviewed Character vector. Sets the reviewed tag in the review
#'   database.
#' @param reviewer Character vector. Sets the reviewer in the review database.
#' @param status Character vector. Sets the status in the review database.
#'   Defaults to `new`.
#'
#' @return A database will be created. Nothing else will be returned.
#' @export
#'
#' @seealso [get_review_data()]
#' 
db_create <- function(
    data, 
    db_path,
    reviewed = "No",
    reviewer = "",
    status = "new"
){
  stopifnot(!file.exists(db_path))
  stopifnot(reviewed %in% c("Yes", "No", ""))
  stopifnot(is.data.frame(data) || is.character(data))
  db_directory <- dirname(db_path)
  if(tools::file_ext(db_path) == "sqlite" && !dir.exists(db_directory)) {
    cat("Directory to store user database does not exist. ", 
        "Creating new directory named '", db_directory, "'.\n", sep = "")
    dir_created <- dir.create(db_directory)
    if(!dir_created) stop("Could not create directory for user database")
  }
  data_synch_time <- attr(data, "synch_time") %||% ""
  
  df <- data |> 
    dplyr::mutate(
      reviewed = reviewed, 
      comment = "", 
      reviewer = reviewer, 
      timestamp = time_stamp(),
      status = status
    )
  
  new_data <- list(
    "all_review_data" = df,
    "query_data"      = query_data_skeleton,
    "db_synch_time"   = data.frame(synch_time = data_synch_time)
  )
  con <- get_db_connection(db_path)
  for(i in names(new_data)){
    cat("\nCreating new table: ", i,  "\n")
    DBI::dbWriteTable(con, i, new_data[[i]])
  }
  cat("Finished writing to database\n\n")
}

#' Update app database
#'
#' Compares the latest edit date-times in the review database and in the data
#' frame. If the provided data frame is newer, the database will be updated.
#'
#' @param data An updated data frame with review data.
#' @param db_path Character vector. Path to the database.
#' @param common_vars A character vector containing the common key variables.
#' @param edit_time_var A character vector with the column name of the edit-time
#'   variable.
#'
#' @return Nothing will be returned.
#' @export
#' 
db_update <- function(
    data, 
    db_path,
    common_vars = c("subject_id", "event_name", "item_group", 
                    "form_repeat", "item_name"), 
    edit_time_var = "edit_date_time"
){
  stopifnot(file.exists(db_path))
  con <- get_db_connection(db_path)
  data_synch_time <- attr(data, "synch_time") %||% ""
  
  db_synch_time <- tryCatch({
    DBI::dbGetQuery(con, "SELECT synch_time FROM db_synch_time") |> 
    unlist(use.names = FALSE)}, error = \(e){""})
  if(!identical(data_synch_time, "") && identical(data_synch_time, db_synch_time)){
    return("Database up to date. No update needed") 
  }
  if(!identical(data_synch_time, "") && db_synch_time > data_synch_time){
    return({
      warning("DB synch time is more recent than data synch time. ", 
              "Aborting synchronization.")
      })
  }
  # Continue in the case data_synch_time is missing and if data_synch_time is 
  # more recent than db_synch_time
  review_data <- DBI::dbGetQuery(con, "SELECT * FROM all_review_data")
  cat("Start adding new rows to database\n")
  updated_review_data <- update_review_data(
    review_df = review_data,
    latest_review_data = data,
    common_vars = common_vars,
    edit_time_var = edit_time_var,
    update_time = data_synch_time
  )
  cat("writing updated review data to database...\n")
  DBI::dbWriteTable(con, "all_review_data", updated_review_data, append = TRUE)
  DBI::dbWriteTable(
    con, 
    "db_synch_time", 
    data.frame("synch_time" = data_synch_time), 
    overwrite = TRUE
  )
  cat("Finished updating review data\n")
}


#' Save review in database
#'
#' Helper function to save review in database. All old data will not be changed.
#' New rows with the new/updated review data will be added to the applicable
#' database tables.
#'
#' @param rv_row A data frame containing the row of the data that needs to be
#'   checked.
#' @param db_path Character vector. Path to the database.
#' @param tables Character vector. Names of the tables within the database to
#'   save the review in.
#' @param common_vars A character vector containing the common key variables.
#' @param review_by A character vector, containing the key variables to perform
#'   the review on. For example, the review can be performed on form level
#'   (writing the same review to all items in a form), or on item level, with a
#'   different review per item.
#'
#' @return Review information will be written in the database. No local objects
#'   will be returned.
#' @export
#' 
db_save_review <- function(
    rv_row,
    db_path,
    tables = c("all_review_data"),
    common_vars = c("subject_id", "event_name", "item_group", 
                    "form_repeat", "item_name"),
    review_by = c("subject_id", "item_group")
){
  stopifnot(is.data.frame(rv_row))
  if(nrow(rv_row) != 1){
    warning("multiple rows detected to save in database. Only the first row will be selected.")
    rv_row <- rv_row[1, ]
  }
  
  cols_to_change <- c("reviewed", "comment", "reviewer", "timestamp", "status")
  db_con <- get_db_connection(db_path)
  new_review_state <- rv_row$reviewed
  cat("copy row ids into database\n ")
  dplyr::copy_to(db_con, rv_row[review_by], "row_ids")
  new_review_rows <-  dplyr::tbl(db_con, "all_review_data") |> 
    dplyr::inner_join(dplyr::tbl(db_con, "row_ids"), by = review_by) |> 
    # Filter below prevents unnecessarily overwriting the review status in forms   
    # with mixed reviewed status (due to an edit by the investigators). 
    dplyr::filter(reviewed != new_review_state) |> 
    dplyr::collect()
  if(nrow(new_review_rows) == 0){return(
    warning("Review state unaltered. No review will be saved.")
  )}
  new_review_rows <- new_review_rows |> 
    db_slice_rows(slice_vars = c("timestamp", "edit_date_time"), group_vars = common_vars) |> 
    dplyr::select(-dplyr::all_of(cols_to_change)) |> 
    # If there are multiple edits, make sure to only select the latest editdatetime for all items:
    # dplyr::slice_max(edit_date_time, by = dplyr::all_of(common_vars)) |> 
    dplyr::bind_cols(rv_row[cols_to_change]) # bind_cols does not work in a db connection.
  cat("write updated review data to database\n")
  lapply(tables, \(x){DBI::dbWriteTable(db_con, x, new_review_rows, append = TRUE)}) |> 
    invisible()
  cat("finished writing to the tables:", tables, "\n")
}

#' Append database table
#' 
#' Saves a query to a table in the user database.
#'
#' @param data A data frame.
#' @param db_path Character string with the file path to the database. 
#' @param db_table Character vector with the name of the destination table that 
#' needs to be appended.
#'
#' @return A table in a database will be appended. No values will be returned. 
#' @export 
#'
#' @examples 
#' db_save(mtcars, ":memory:", "mtcars_db")
#' 
db_save <- function(data, db_path, db_table = "query_data"){
  stopifnot(is.data.frame(data), is.character(db_table))
  db_con <- get_db_connection(db_path)
  
  cat("saving data frame to database table '", db_table, "'\n")
  DBI::dbWriteTable(db_con, db_table, data, append = TRUE)
  cat("data saved\n")  
}


#'Retrieve query from database
#'
#'Small helper function to retrieve a query from the database. if no follow-up
#'number is provided, all messages will be collected.
#'
#'@param db_path Character vector. Needs to be a valid path to a database.
#'@param query_id Character string with the query identifier to extract from the
#'  database.
#'@param n (optional) numerical or character string, with the query follow-up
#'  number to extract
#'@param db_table Character vector with the name of the table to read from.
#'
#'@return A data frame
#'@export
#'@inheritParams db_slice_rows
#'
#' @examples
#'local({
#' temp_path <- withr::local_tempfile(fileext = ".sqlite")
#' con <- get_db_connection(temp_path)
#'
#' new_query <- dplyr::tibble(
#'  query_id = "ID124234",
#'  subject_id = "ID1",
#'  n = 1,
#'  timestamp = "2024-02-05 01:01:01",
#'  other_info = "testinfo"
#' )
#' DBI::dbWriteTable(con, "query_data", new_query)
#' db_get_query(temp_path, query_id = "ID124234", n = 1)
#' })
#' 
db_get_query <- function(
    db_path, 
    query_id, 
    n = NULL,
    db_table = "query_data",
    slice_vars = "timestamp",
    group_vars = c("query_id", "n")
){
  stopifnot(file.exists(db_path))
  stopifnot(is.character(query_id))
  stopifnot(is.character(db_table))
  stopifnot(is.null(n) | is.numeric(n) | is.character(n))
  filter_n <- ifelse(is.null(n), "", " AND n=?n")
  sql <- paste0(
    "SELECT * FROM ?db_table WHERE query_id = ?query_id", 
    filter_n, ";"
  )
  db_temp_connect(db_path, {
    sql_args <- list(
      conn = con, 
      sql = sql, 
      db_table = db_table[1], 
      query_id = query_id[1]
    )
    sql_args$n <- n[1] #So that this function argument will be conditional.
    query <- do.call(DBI::sqlInterpolate, sql_args)
    DBI::dbGetQuery(con, query) |> 
      db_slice_rows(slice_vars = slice_vars, group_vars = group_vars) |> 
      dplyr::as_tibble()
  })
}

#' Retrieve review
#'
#' Small helper function to retrieve the (latest) review data from the database
#' with the given subject id (`subject`) and `form`.
#'
#' @param db_path Character vector. Needs to be a valid path to a database.
#' @param subject Character vector with the subject identifier to select from
#'   the database.
#' @param form Character vector with the form identifier to select from the
#'   database.
#'
#' @inheritParams db_slice_rows
#' @return A data frame.
#' @export
#'
#' @examples
#'
#' local({
#'   temp_path <- withr::local_tempfile(fileext = ".sqlite")
#'   con <- get_db_connection(temp_path)
#'   review_data <- data.frame(
#'   subject_id = "Test_name",
#'    event_name = "Visit 1",
#'    item_group = "Test_group",
#'    form_repeat = 1,
#'    item_name = "Test_item",
#'    edit_date_time = "2023-11-05 01:26:00",
#'    timestamp = "2024-02-05 01:01:01"
#'   ) |>
#'    dplyr::as_tibble()
#'   DBI::dbWriteTable(con, "all_review_data", review_data)
#'   db_get_review(temp_path, subject = "Test_name", form = "Test_group")
#' })
#' 
db_get_review <- function(
    db_path, 
    subject = review_row$subject_id, 
    form = review_row$item_group,
    db_table = "all_review_data",
    slice_vars = c("timestamp", "edit_date_time"),
    group_vars = c("subject_id", "event_name", "item_group",
                   "form_repeat", "item_name")
){
  stopifnot(file.exists(db_path))
  stopifnot(is.character(subject))
  stopifnot(is.character(form))
  db_temp_connect(db_path, {
    sql <- "SELECT * FROM ?db_table WHERE subject_id = ?id AND item_group = ?group;"
    query <- DBI::sqlInterpolate(con, sql, db_table = db_table[1], 
                                 id = subject[1], group = form[1])
    DBI::dbGetQuery(con, query) |> 
      db_slice_rows(slice_vars = slice_vars, group_vars = group_vars) |> 
      dplyr::as_tibble()
  })
}
