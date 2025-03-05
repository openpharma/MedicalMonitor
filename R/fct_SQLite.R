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
  
  new_pk_data <- list(
    "all_review_data" = df,
    "query_data"      = query_data_skeleton
  )
  idx_pk_cols <- list(
    all_review_data = idx_cols
  )
  other_data <- list(
    "db_synch_time"   = data.frame(synch_time = data_synch_time),
    "db_version" = data.frame(version = db_version)
  )
  con <- get_db_connection(db_path)
  db_add_tables(con, new_pk_data, idx_pk_cols, other_data)
  cat("Finished writing to database\n\n")
}

#' Add new tables to DB
#'
#' @param con A DBI Connection to the SQLite DB
#' @param pk_data A named list of data frames to add a primary key field to DB
#'   table. Names will correspond to the DB table names.
#' @param unique_cols A named list of the fields defining unique records for a
#'   table. Names will correspond to the table to apply the index constraint.
#' @param other_data A named list of other data frames to add to the DB. Names
#'   will correspond to the DB table names.
#'
#' @keywords internal
db_add_tables <- function(con, pk_data, unique_cols, other_data) {
  for(i in names(pk_data)){
    cat("\nCreating new table: ", i,  "\n")
    db_add_primary_key(con, i, pk_data[[i]], unique_cols[[i]])
  }
  for(i in names(other_data)){
    cat("\nCreating new table: ", i,  "\n")
    DBI::dbWriteTable(con, i, other_data[[i]])
  }
  cat("\nCreating log table: all_review_data_log\n")
  db_add_log(con)
}

#' Add primary key field
#' 
#' @param con A DBI Connection to the SQLite DB
#' @param name The table name
#' @param value A data.frame to add to the table
#' @param keys A character vector specifying which columns define a unique row
#'   for the table. If `NULL`, no unique index will be created.
#'   
#' @keywords internal
db_add_primary_key <- function(con, name, value, keys = NULL) {
  fields <- c(id = "INTEGER PRIMARY KEY AUTOINCREMENT", DBI::dbDataType(con, value))
  DBI::dbCreateTable(con, name, fields)
  if (!is.null(keys)) {
    all_keys <- paste(keys, collapse = ", ")
    rs <- DBI::dbSendStatement(
      con, 
      sprintf("CREATE UNIQUE INDEX idx_%1$s ON %1$s (%2$s)", name, all_keys)
      )
    DBI::dbClearResult(rs)
  }
  DBI::dbAppendTable(con, name, value)
}

#' Add Logging Table
#'
#' Both creates the logging table and the trigger to update it for
#' all_review_data.
#'
#' @param con A DBI Connection to the SQLite DB
#' @param keys A character vector specifying which columns should not be updated
#'   in a table. Defaults to 'id' and the package-defined index columns
#'   (`idx_cols`).
#'
#' @keywords internal
db_add_log <- function(con, keys = c("id", idx_cols)) {
  stopifnot(is.character(keys))
  all_keys <- paste(keys, collapse = ", ")
  stopifnot("'keys' parameter cannot be empty" = nchar(all_keys) > 0)
  
  DBI::dbCreateTable(
    con, 
    "all_review_data_log",
    c(
      id = "INTEGER PRIMARY KEY AUTOINCREMENT", 
      review_id = "INTEGER NOT NULL",
      edit_date_time = "CHAR", 
      reviewed = "CHAR", 
      comment = "CHAR", 
      reviewer = "CHAR", 
      timestamp = "CHAR", 
      status = "CHAR",
      dml_type = "CHAR NOT NULL", 
      dml_timestamp = "DATETIME DEFAULT CURRENT_TIMESTAMP"
      )
  )
  # This will trigger before any UPDATEs happen on all_review_data. Instead of
  # allowing 'id' to be updated, it will throw an error.
  rs <- DBI::dbSendStatement(con, paste(
    "CREATE TRIGGER all_review_data_id_update_trigger",
    sprintf("BEFORE UPDATE OF %s ON all_review_data", all_keys),
    "BEGIN",
    sprintf("SELECT RAISE(FAIL, 'Fields %s are read only');", all_keys),
    "END"
  ))
  DBI::dbClearResult(rs)
  rs <- DBI::dbSendStatement(con, paste(
    "CREATE TRIGGER all_review_data_update_log_trigger",
    "AFTER UPDATE ON all_review_data FOR EACH ROW",
    "BEGIN",
      "INSERT INTO all_review_data_log (",
        "review_id, edit_date_time, reviewed, comment, reviewer, timestamp, status, dml_type",
      ")",
      "VALUES(",
        "NEW.id,",
        "OLD.edit_date_time,",
        "OLD.reviewed,",
        "OLD.comment,",
        "OLD.reviewer,",
        "OLD.timestamp,",
        "OLD.status,",
        "'UPDATE'",
      ");",
    "END"
  ))
  DBI::dbClearResult(rs)
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
  db_upsert(con, updated_review_data, common_vars)
  DBI::dbWriteTable(
    con, 
    "db_synch_time", 
    data.frame("synch_time" = data_synch_time), 
    overwrite = TRUE
  )
  cat("Finished updating review data\n")
}

#' UPSERT to all_review_data
#' 
#' Performs an UPSERT on all_review_data. New records will be appended to the
#' table. Changed/updated records will be applied to the table based on the
#' index column constraint.
#' 
#' @param con A DBI Connection to the SQLite DB
#' @param data A data frame containing the data to UPSERT into all_review_data
#' @param idx_cols A character vector specifying which columns define a
#'   unique index for a row
#'   
#' @return invisibly returns TRUE. Is run for it's side effects on the DB.
#' 
#' @keywords internal
db_upsert <- function(con, data, idx_cols) {
  if ("id" %in% names(data))
    data$id <- NULL
  cols_to_update <- names(data)[!names(data) %in% idx_cols]
  cols_to_insert <- names(data) |> 
    paste(collapse = ", ")
  constraint_cols <- paste(idx_cols, collapse = ", ")
  dplyr::copy_to(con, data, "row_updates")
  rs <- DBI::dbSendStatement(con, paste(
    "INSERT INTO",
    "all_review_data",
    sprintf("(%s)", cols_to_insert),
    sprintf("SELECT %s FROM row_updates WHERE true", cols_to_insert),
    "ON CONFLICT",
    sprintf("(%s)", constraint_cols),
    "DO UPDATE SET",
    sprintf("%1$s = excluded.%1$s", cols_to_update) |> paste(collapse = ", ")
  ))
  DBI::dbClearResult(rs)
}


#' Save review in database
#'
#' Helper function to save review in database. All old data will not be changed.
#' New rows with the new/updated review data will be added to the applicable
#' database tables.
#'
#' @param rv_records A data frame containing the rows of data that needs to be
#'   checked.
#' @param db_path Character vector. Path to the database.
#' @param table Character vector. Names of the table within the database to
#'   save the review in.
#'
#' @return Review information will be written in the database. No local objects
#'   will be returned.
#' @export
#' 
db_save_review <- function(
    rv_records,
    db_path,
    table = "all_review_data"
){
  stopifnot(is.data.frame(rv_records))
  stopifnot(is.character(table) && length(table) == 1)
  if (any(duplicated(rv_records[["id"]]))) {
    warning("duplicate records detected to save in database. Only the first will be selected.")
    rv_records <- rv_records[!duplicated(rv_records[["id"]]),]
  }

  cols_to_change <- c("reviewed", "comment", "reviewer", "timestamp", "status")
  db_con <- get_db_connection(db_path)
  if(nrow(rv_records) == 0){return(
    warning("Review state unaltered. No review will be saved.")
  )}
  
  cat("write updated review data to database\n")
  dplyr::copy_to(db_con, rv_records, "row_updates")
  rs <- DBI::dbSendStatement(db_con, paste(
    "UPDATE",
    table,
    "SET",
    sprintf("%1$s = row_updates.%1$s", cols_to_change) |> paste(collapse = ", "),
    "FROM",
    "row_updates",
    "WHERE",
    sprintf("%s.id = row_updates.id", table),
    "AND",
    sprintf("%s.reviewed <> row_updates.reviewed", table)
  ))
  DBI::dbClearResult(rs)
  cat("finished writing to the table:", table, "\n")
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
#' @param ... Named arguments specifying which records to retrieve, see
#'   examples. Note that `...` will be processed with `data.frame()` and thus
#'   the arguments within `...` should be convertible to a data frame. This is
#'   chosen so that filters of length one can be used with other filters since
#'   they will be recycled (for example, when selecting multiple events of one
#'   subject). 
#' @param db_table Character string. Name of the table to collect. Will only be
#'   used if `data` is a character string to a database.
#'
#' @return A data frame.
#' @keywords internal
#' 
db_get_review <- function(
    db_path, 
    ...,
    db_table = "all_review_data"
){
  stopifnot(file.exists(db_path))
  fields <- ...names()
  if (is.null(fields)) {
    if (...length() > 0) {
      warning("Unnamed arguments passed in `...`. Returning full data table.")
    } else {
      warning("No arguments passed in `...`. Returning full data table.")
    }
    conditionals <- "true"
  } else {
    conditionals <- paste0(fields, " = $", fields, collapse = " AND ")
    parameters <- data.frame(...)
  }
  
  db_temp_connect(db_path, {
    sql <- paste("SELECT * FROM ?db_table WHERE", conditionals)
    query <- DBI::sqlInterpolate(con, sql, db_table = db_table[1])
    rs <- DBI::dbSendQuery(con, query)
    if (!is.null(fields)) DBI::dbBind(rs, params = parameters)
    df <- DBI::dbFetch(rs) |> 
      dplyr::as_tibble()
    DBI::dbClearResult(rs)
    df
  })
}

db_get_version <- function(db_path) {
  stopifnot(file.exists(db_path))
  con <- get_db_connection(db_path)
  tryCatch({
    DBI::dbGetQuery(con, "SELECT version FROM db_version") |> 
      unlist(use.names = FALSE)
  },
  error = \(e) {""}
  )
}

update_db_version <- function(db_path, version = "1.1") {
  stopifnot(file.exists(db_path))
  version <- match.arg(version)
  temp_path <- withr::local_tempfile(fileext = ".sqlite")
  file.copy(db_path, temp_path)
  con <- get_db_connection(temp_path)
  
  current_version <- tryCatch({
    DBI::dbGetQuery(con, "SELECT version FROM db_version") |> 
      unlist(use.names = FALSE)}, error = \(e){""})
  if(identical(current_version, db_version)) return("Database up to date. No update needed")
  
  review_skeleton <- DBI::dbGetQuery(con, "SELECT * FROM all_review_data LIMIT 0")
  rs <- DBI::dbSendQuery(con, "ALTER TABLE all_review_data RENAME TO all_review_data_old")
  DBI::dbClearResult(rs)
  rs <- DBI::dbSendQuery(con, "ALTER TABLE query_data RENAME TO query_data_old")
  DBI::dbClearResult(rs)
  
  new_pk_data <- list(
    "all_review_data" = review_skeleton,
    "query_data"      = query_data_skeleton
  )
  idx_pk_cols <- list(
    all_review_data = idx_cols
  )
  other_data <- list(
    "db_version" = data.frame(version = db_version)
  )
  db_add_tables(con, new_pk_data, idx_pk_cols, other_data)
  
  query_cols <- paste(names(query_data_skeleton), collapse = ", ")
  cat("\nInserting old query records into new table.\n")
  rs <- DBI::dbSendStatement(con, sprintf("INSERT INTO query_data (%1$s) SELECT %1$s FROM query_data_old", query_cols))
  DBI::dbClearResult(rs)
  
  stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) FROM query_data") == 
              DBI::dbGetQuery(con, "SELECT COUNT(*) FROM query_data_old"))
  
  rs <- DBI::dbSendStatement(con, "DROP TABLE query_data_old")
  DBI::dbClearResult(rs)
  
  cat("\nInserting old review records into new tables.\n")
  cols_to_update <- names(review_skeleton)[!names(review_skeleton) %in% idx_pk_cols$all_review_data]
  cols_to_insert <- names(review_skeleton) |> 
    paste(collapse = ", ")
  upsert_statement <- paste(
    "INSERT INTO",
    "all_review_data",
    sprintf("(%s)", cols_to_insert),
    sprintf("SELECT %s FROM all_review_data_old WHERE true", cols_to_insert),
    "ON CONFLICT",
    sprintf("(%s)", paste(idx_pk_cols$all_review_data, collapse = ", ")),
    "DO UPDATE SET",
    sprintf("%1$s = excluded.%1$s", cols_to_update) |> paste(collapse = ", ")
  )
  rs <- DBI::dbSendStatement(con, upsert_statement)
  DBI::dbClearResult(rs)
  
  stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) FROM all_review_data") +
              DBI::dbGetQuery(con, "SELECT COUNT(*) FROM all_review_data_log") == 
              DBI::dbGetQuery(con, "SELECT COUNT(*) FROM all_review_data_old"))
  
  rs <- DBI::dbSendStatement(con, "DROP TABLE all_review_data_old")
  DBI::dbClearResult(rs)
  
  file.copy(temp_path, db_path, overwrite = TRUE)
  cat("Finished updating to new database standard\n\n")
}
