#' Get review data
#'
#' Create a data frame containing all data to be reviewed. Selects the *latest*
#' edit date-time for every event/item.
#'
#' @param data A data frame, commonly raw data merged with meta data.
#' @param common_vars A character vector with the common variable column names.
#'   Assumes that these variables can identify a unique data point that needs to
#'   be reviewed.
#' @param date_var A character vector with the name of the date column.
#' @param edit_time_var A character vector with the name of the edit date-time
#'   column.
#'
#' @return A data frame with review data.
#' @export
#' 
get_review_data <- function(
    data,
    common_vars  = c("subject_id", "event_name", "item_group", 
                     "form_repeat", "item_name"),
    date_var = "event_date",
    edit_time_var = "edit_date_time"
){
  stopifnot(inherits(data, c("Pool", "data.frame")), 
            is.character(common_vars), 
            is.character(date_var), is.character(edit_time_var))
  all_review_data <- data |> 
    # inner_join since some variables in metadata are undefined. Examples are "Other" units for some items.
    # This will be tested at a different stage of loading the application, so no need to test it here.
    # update: dont merge with metadata here. instead apply this function after merging 
    # raw data with meta, and creating all other vars. This way, every var that will be created, gets a review tag
    dplyr::select(dplyr::all_of(c(common_vars, date_var, edit_time_var))) |> 
    # slice_max below does not group by date_var since event_date for adverse events is weird in our EDC 
    # (event_date is the same as the latest edit_date_time)
    dplyr::slice_max(.data[[edit_time_var]], by = dplyr::all_of(common_vars), with_ties = FALSE) |> 
    dplyr::mutate(dplyr::across(!dplyr::where(is.numeric), as.character)) |> 
    dplyr::filter(!item_group == "General")
  ## TODO: test here for distinct rows? any duplicated rows? should not be the case ideally. 
  # If form_repeat not used, then Adverse events can have duplicate rows.
  all_review_data
}

#' Update review data
#'
#'
#' @param review_df Data frame containing old review data that need to be
#'   updated.
#' @param latest_review_data Data frame containing latest review data.
#' @param common_vars A character vector containing the common key variables.
#' @param edit_time_var A character vector with the column name of the edit-time
#'   variable.
#' @param update_time Time stamp given to rows with new rows that are added.
#'   Defaults to [time_stamp()].
#'
#' @return A data frame containing only the rows with updated review data.
#' @export
#' 
update_review_data <- function(
    review_df,
    latest_review_data,
    common_vars = c("subject_id", "event_name", "item_group", 
                    "form_repeat", "item_name"),
    edit_time_var = "edit_date_time",
    update_time = time_stamp()
){
  stopifnot(is.data.frame(latest_review_data), nrow(latest_review_data) > 0 )
  stopifnot(is.data.frame(review_df), nrow(review_df) > 0 )
  stopifnot(is.character(common_vars))
  stopifnot(is.character(edit_time_var))
  
  deleted_data <- dplyr::anti_join(review_df, latest_review_data, by = common_vars)
  n_deleted <- nrow(deleted_data)
  if(n_deleted != 0){
    warning(n_deleted, " items were not found in the updated dataset")
    cat("Missing items in the dataset:\n")
    print(deleted_data)
  }
  
  # If data is reviewed and later in an update deleted, then it is okay if the 
  # deletion does not show up in the updated data set, and the deleted data point shows 
  # up as reviewed. One reason why this is okay is because missing data is checked 
  # by the data managers. 
  # a more practical reason is that missing data does not show up in the application.
  
  # when do we have the same edit_date_time but different timestamps? 
  # For example:
  # - we have a new data point. It is not yet reviewed (status new or updated).
  # - the data point will be reviewed. a new row with the same item and edit date-time 
  #   is added, with new review status and new timestamp
  
  df <- latest_review_data |> 
    # including event_date in join by should work, even for Adverse events, since 
    # if event_date changes, the edit date-time also changes, and thus all rows 
    # will be taken into consideration.
    dplyr::full_join(review_df, by = names(latest_review_data)) |> 
    add_missing_columns(c("timestamp", "reviewed", "comment", "status"))
  
  df <- df |> 
    tidyr::replace_na(list(timestamp = update_time, reviewed = "No", comment = "")) |> 
    # now add status labels to the new rows:
    dplyr::mutate(
      status = ifelse(
        # All old status labels ("old", "updated" or "new") should be retained here:
        !is.na(status) & !status == "", status,
        # All new rows do not have a status label yet. They should either get
        # the label "new" or "updated", based on whether a review of the same
        # data point did already take place in the past:
        ifelse(any(reviewed == "Yes"), "updated", "new")
      ),
      .by = dplyr::all_of(c(common_vars))
    )
  updated_data <- dplyr::anti_join(df, review_df, 
                                   by = c(common_vars, edit_time_var))
  
  if(nrow(updated_data) == 0){
    warning("No new data in the updated dataset. Returning empty data frame.")
    }
  if(nrow(updated_data) != nrow(dplyr::distinct(updated_data))){
    warning("Unexpected duplicated rows detected. Verify status data frame")
  }
  updated_data
}

#' Summarize review data
#'
#' Creates a summary data frame based on review data. Creates one row per
#' subject id and form. All events (or visits) of a specific subject id and form
#' will be collapsed into one column separated with a comma, using
#' [collapse_column_vals()] internally. Used to summarize data in the server,
#' which is then passed to the modules [mod_header_widgets_server()],
#' [mod_navigate_review_server()], and [mod_report_server()].
#'
#' @param data Data frame with review data.
#' @param common_vars A character vector containing the common key variables.
#' @param event_var A character string containing the name of the column with
#'   the event names.
#' @param date_time_vars A character vector with the names of the date-time
#'   variables. Can be multiple variable names.
#' @param status_var A character vector with the name of the status column.
#' @param collapse_exclude A character vector with the values to be excluded
#'   from the column collapse.
#'
#' @return A data frame. Required columns are: subject_id, item_group,
#'   event_name, edit_date_time, status, reviewed.
#' @export
#' 
summarize_review_data <- function(
    data, 
    common_vars = c("subject_id", "item_group"),
    event_var = "event_name",
    date_time_vars = "edit_date_time",
    status_var = "status",
    collapse_exclude = "Any visit" 
){
  stopifnot(inherits(data, c("tbl_sql", "data.frame")))
  stopifnot(is.character(common_vars))
  if(dplyr::pull(dplyr::tally(data), n) != 0){
    data <- data |> 
      dplyr::select(dplyr::all_of(c(common_vars, event_var, date_time_vars, status_var))) |> 
      collapse_column_vals(column = event_var, exclude = collapse_exclude, 
                           group_by = common_vars) |> 
      dplyr::mutate(
        "{status_var}"    := dplyr::case_when(
          all(.data[[status_var]] == "old")                  ~ "old", 
          all(.data[[status_var]] %in% c("new", "old"))      ~ "new", 
          all(.data[[status_var]] %in% c("updated", "old"))  ~ "updated", 
          all(.data[[status_var]] %in% c("old",  "updated", "new"))  ~ "new/updated",
          TRUE ~ NA_character_
        ),
        .by = dplyr::all_of(common_vars)
      ) |> 
      dplyr::mutate(
        reviewed = ifelse(status == "old", "Yes", "No")
      ) |> 
      dplyr::mutate(
        dplyr::across(dplyr::all_of(date_time_vars), ~max(., na.rm = TRUE)),
        .by = dplyr::all_of(common_vars)
        ) |>
      dplyr::distinct()
  }
  data 
} 

#' Collapse column values
#'
#' Collapses all the unique values of a character variable in a (long-format)
#' data frame with a given separator and within a given group. It is also
#' possible to define values that need to be excluded from the collapse; any
#' rows with these excluded values will be left as is.
#'
#' @param data A data frame in long format.
#' @param column Character string. Column containing the strings to collapse.
#' @param exclude Character vector. Values that should not be collapsed.
#' @param group_by  Character vector. Collapsing will occur grouped by these
#'   variables.
#' @param separator Character string to collapse the column values with.
#'
#' @return A data frame in which the `column` is changed.
#' @export
#'
#' @examples
#'  library(dplyr)
#'  df <- data.frame(
#'   ID = sample(1:5, 15, replace = TRUE),
#'   name = sample(c("collapsed", "excluded", "names"), 15, replace = TRUE)
#'   ) |>
#'   mutate(
#'     site = sample(c("S1", "S2", "S3"), 1),
#'     .by = ID
#'  ) |>
#'  arrange(ID, factor(name))
#'  collapse_column_vals(df, column =  "name", exclude = "excluded",
#'   group_by = c("ID", "site"))
#'   
collapse_column_vals <- function(
    data,
    column = "event_name",
    exclude = "",
    group_by = c("subject_id", "item_group"),
    separator = ", "
){
  stopifnot(inherits(data, c("tbl_sql", "data.frame")))
  stopifnot({is.character(column); length(column) == 1})
  stopifnot(is.character(group_by))
  stopifnot({is.character(separator); length(separator) == 1})
  # TODO: note that the below is just a hotfix. Better to make it work with SQL 
  # in the future instead of collecting the entire database. 
  if(inherits(data, "tbl_sql")) data <- dplyr::collect(data)
  if(dplyr::pull(dplyr::tally(data), n) == 0) return(data)
  data |> 
    dplyr::mutate(
       "{column}" := ifelse(
         .data[[column]] %in% exclude, 
         .data[[column]], 
         paste0(unique(.data[[column]][!.data[[column]] %in% exclude]), 
                collapse = separator)
         ),
      .by = dplyr::all_of(group_by)
    ) 
}
