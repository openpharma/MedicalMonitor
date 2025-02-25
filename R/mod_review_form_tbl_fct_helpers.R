
#' Get form table
#'
#' @param form_data A data frame with the study data of the respective form.
#' @param form_review_data A data frame with he review data of the form.
#' @param form A character string with the name of the form.
#' @param form_items Named character vector with all form_items to display.
#' @param active_subject A character string with the active subject id.
#' @param is_reviewed A logical, indicating whether all items of the entire form
#'   for the active subject id are reviewed.
#' @param is_SAE A logical, indicating whether the form is a SAE form. If TRUE,
#'   will make some adjustments to the columns to display.
#' @param id_cols Columns that identify a unique row in the data.
#'
#' @keywords internal
#'
get_form_table <- function(
    form_data,
    form_review_data,
    form,
    form_items,
    active_subject,
    is_reviewed = NULL,
    is_SAE = NULL,
    id_cols = idx_cols
){
  stopifnot(is.data.frame(form_data), is.data.frame(form_review_data))
  stopifnot(is.character(form), is.character(form_items))
  stopifnot(is.logical(is_reviewed %||% FALSE), is.logical(is_SAE %||% FALSE))
  is_SAE <- isTRUE(is_SAE)
  stopifnot(is.character(active_subject %||% ""))
  if(is.null(active_subject)){
    warning("No active subject selected")
  }
  required_cols <- c(id_cols, "edit_date_time", "event_date", "item_value")
  missing_cols <- required_cols[!required_cols %in% names(form_data)]
  if(length(missing_cols) != 0){
    stop("the following columns are missing: ", paste0(missing_cols, collapse = ", "))
  }
  df <- dplyr::left_join(
    form_data,
    form_review_data |> 
      dplyr::select(-dplyr::all_of(c("edit_date_time", "event_date"))), 
    by = id_cols
  ) |> 
    dplyr::mutate(
      not_reviewed_but_missing = (reviewed == "No" & is.na(item_value)), 
      item_value = dplyr::case_when(
        is.na(reviewed) ~ htmltools::htmlEscape(item_value),
        (reviewed == "No" & !is.na(item_value)) ~
        paste0("<b>", htmltools::htmlEscape(item_value), "*</b>"), 
        .default = htmltools::htmlEscape(item_value)
      )
    ) |> 
    create_table(expected_columns = names(form_items)) |> 
    dplyr::mutate(
      o_reviewed = Map(
        \(x, y, z) append(x, list(
          row_id = y, 
          disabled = z,
          updated = is_reviewed)
        ), 
        o_reviewed, 
        dplyr::row_number(),
        if (is.null(active_subject)) FALSE else subject_id != active_subject
      )
    )
  if(form == "Adverse events") {
    df <- adjust_ae_form_table(df, is_SAE = is_SAE)
  }
  df
}

#' Adjust (Serious) Adverse Event form tables
#'
#' Needed because AE and SAE items are closely related and are currently
#' adjusted after pivoting (see [create_table.adverse_events()]). This function
#' should be phased out in the future.
#'
#' @param data 
#' @inheritParams source
#' 
#' @noRd
#' 
adjust_ae_form_table <- function(
    data,
    is_SAE
){
  stopifnot(is.data.frame(data), is.logical(is_SAE))
  if (is_SAE){
    with(data, data[grepl("Yes", `Serious Adverse Event`),]) |>
      dplyr::select(dplyr::any_of(
        c("o_reviewed", "subject_id","form_repeat", "Name", "AESI",  "SAE Start date",
          "SAE End date", "CTCAE severity", "Treatment related",
          "Treatment action", "Other action", "SAE Category",
          "SAE Awareness date", "SAE Date of death", "SAE Death reason")
      )) |>
      adjust_colnames("^SAE ")
  } else {
    with(data, data[!grepl("Yes", `Serious Adverse Event`),]) |> 
      dplyr::select(-dplyr::starts_with("SAE"))
  }
}


#' Update Review Records
#'
#' Updates the review records data frame when a datatable checkbox is clicked.
#'
#' @param review_records The review records data frame to update.
#' @param review_selection The review selection data frame input from the
#'   datatable.
#' @param active_data The active review data frame.
#' 
#' @return A data frame containing the updated records data.
#'
#' @details Three main steps are performed: UPSERT, SUBSET, and ANTI-JOIN The
#'   UPSERT takes the review selection data frame and upserts it into the review
#'   records data frame. (An upsert will insert a record if the unique
#'   identifier is not yet present and update a record based on the unique
#'   identifier if it already exists.) The SUBSET step removes an empty reviews
#'   (partially review rows) and any records not part of the active review (as a
#'   precautionary measure). The ANTI-JOIN step removes any records that match
#'   the active review (records that will not be changing review status based on
#'   user inputs).
#' 
#' @noRd
update_review_records <- function(review_records, review_selection, active_data) {
  if (is.null(review_records))
    review_records <- data.frame(id = integer(), reviewed = character())
  review_records |> 
    dplyr::rows_upsert(
      review_selection,
      by = "id"
    ) |> 
    # Remove empty reviews and inactive data IDs
    subset(!is.na(reviewed) | !id %in% active_data$id) |> 
    # Only update records where the review status is being changed
    dplyr::anti_join(
      active_data,
      by = c("id", "reviewed")
    ) |> 
    dplyr::arrange(id)
}

#' Update Server Table from Selection
#' 
#' Updates the server table object based on the user selection.
#' 
#' @param tbl_data A data frame containing the server table.
#' @param review_selection The review selection data frame input from the
#'   datatable.
#' 
#' @return A data frame containing the updated table data.
#' 
#' @noRd
update_tbl_data_from_datatable <- function(tbl_data, review_selection) {
  update_row <- dplyr::distinct(review_selection, reviewed, row_id)
  row_ids <- tbl_data$o_reviewed |> lapply(\(x) x[["row_id"]]) |> unlist()
  tbl_data[row_ids == update_row$row_id, "o_reviewed"] <- list(list(
    modifyList(tbl_data[row_ids == update_row$row_id,]$o_reviewed[[1]], 
               list(updated = switch(update_row$reviewed, "Yes" = TRUE, "No" = FALSE, NA)))
  ))
  tbl_data
}

#' Overall Reviewed Field
#'
#' This field serves as the main communication mechanism between the Shiny
#' session and the DataTable objects in the browser.
#'
#' @format A list with up to five elements:
#' \describe{
#'    \item{reviewed}{A logical indicating the current review status of the table row.}
#'    \item{ids}{A vectors containing the `id`s associated with the table row.}
#'    \item{row_id}{A numeric value indicating the associated row in the DataTable. (Used to update server data set based on user changes to browser table.)}
#'    \item{disabled}{A logical indicating whether the table row is part of the active review.}
#'    \item{updated}{A logical indicating whether the user has changed the review status in the DataTable.}
#' }
#'
#' @details The first three elements, `reviewed`, `ids`, and `row_id`, are
#'   initialized when the datatable data set is created (via `create_table()`
#'   etc.). This occurs whenever there is a change with the review data. The
#'   `disabled` element gets updated whenever there is a change in which subject
#'   is actively being reviewed. The `updated` field gets changed in one of
#'   three events: the subject being reviewed is changed and `updated` gets set
#'   to `NULL`, a user changed review status in the DataTable object and
#'   `updated` gets set to the user inputted value, and finally when a user
#'   changes the overall review status in the sidebar and `updated` gets set to
#'   reflect that inputted value.
#' 
#' @noRd
# "o_reviewed"
