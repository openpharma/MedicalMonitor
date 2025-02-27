### These internal functions are used to customize events based on an event_id. 
### They are called within `get_metadata()` and `merge_meta_with_data()`.

#' Clean event metadata
#'
#' Internal function to verify and clean event data.
#'
#' @param data A data frame with event data.
#' @param event_id_cols A character vector with the event_id columns.
#'
#' @keywords internal
#' @return A data frame with the columns: event_id, event_id_pattern,
#'   event_name_custom, event_label_custom, is_regular_visit, is_baseline_event,
#'   generate_labels, meta_event_order, add_visit_number,
#'   add_event_repeat_number.
#' 
clean_event_metadata <- function(
    data, 
    event_id_cols = c("event_id", "event_id_pattern")
){
  ### Verify and clean event data:
  stopifnot(is.character(event_id_cols))
  if(!any(event_id_cols %in% names(data)) ){
    stop("At least one of the columns 'event_id' or 'event_id_pattern' must ",
         "be available in the metadata 'events' tab")
  }
  data <- add_missing_columns(data, event_id_cols)
  if (with(data, sum(is.na(event_id) & is.na(event_id_pattern) )) > 0 ){
    stop("The values in 'event_id' and 'event_id_pattern' cannot both be ",
         "missing in the 'events' metadata tab.")
  }
  # Expanding table so that all matching id's are shown:
  data <- data |> 
    add_missing_columns(c("event_name_custom", "event_label_custom", 
                          "is_regular_visit", "is_baseline_event")) |> 
    dplyr::mutate(
      # Generate labels for compact timeline if not matching on exact event_id:
      generate_labels = is.na(event_id),
      # Because event_id_pattern will always be used for merging:
      event_id_pattern = ifelse(
        !is.na(event_id), 
        paste0("^", event_id, "$"),
        event_id_pattern
      ),
      meta_event_order = dplyr::row_number(),
      # Only expected visits will show up in the compact timeline
      is_regular_visit = dplyr::coalesce(as.logical(is_regular_visit), TRUE),
      add_visit_number = generate_labels & is_regular_visit,
      add_event_repeat_number = generate_labels & !is_regular_visit,
      is_baseline_event = dplyr::coalesce(as.logical(is_baseline_event), FALSE)
    ) 
  if (all(data$is_baseline_event == FALSE)){
    data$is_baseline_event[data$is_regular_visit][1] <- TRUE 
  }
  if (sum(data$is_baseline_event) > 1){
    stop(" Verify metadata. Only one baseline event allowed.")
  }
  data
}



#' Add time vars to raw data
#'
#' @param data A data frame
#' @param events A data frame with events. Expected to be cleaned with the
#'   function [clean_event_metadata()].
#'
#' @return A data frame, with derivative time and event variables, needed for
#'   ClinSight to function properly.
#'
#' @keywords internal
#' @seealso [clean_event_metadata()]
add_timevars_to_data <- function(
    data,
    events
){
  stopifnot("[data] should be a data frame" = is.data.frame(data))
  required_cols <- c("site_code", "subject_id", "event_id", 
                     "event_date", "edit_date_time")
  missing_new_cols <- required_cols[!required_cols %in% names(data)] |> 
    paste0(collapse = ", ")
  if (nchar(missing_new_cols) > 0) stop(
    paste0("The following columns are missing while they are required: ", 
           paste0(missing_new_cols, collapse = ", "), ".")
  )
  data <- data |> 
    dplyr::mutate(
      edit_date_time = as.POSIXct(edit_date_time, tz = "UTC"),
      event_date = as.Date(event_date)
    ) |> 
    dplyr::arrange(
      factor(site_code, levels = order_string(site_code)),
      factor(subject_id, levels = order_string(subject_id))
    )
  if("day" %in% names(data) && "difftime" %in% class(data$day)){
    cat("Using pre-existing day column. \n")
    return(data)
  }
  stopifnot("[events] should be a data frame" = is.data.frame(events))
  stopifnot(c("event_id", "is_baseline_event") %in% names(events))
  stopifnot(is.character(events$event_id))
  stopifnot(is.logical(events$is_baseline_event))
  stopifnot("Events table cannot be empty" = nrow(events) != 0)
  stopifnot(sum(events$is_baseline_event) == 1)
  
  if(nrow(data) == 0){
    warning("Empty data frame provided. Returning early.\n")
    data[["baseline_date"]] <- as.Date(character())
    return(data)
  }
  baseline_id <- with(events, event_id[is_baseline_event])
  baseline_dates <-  data |> 
    dplyr::mutate(
      baseline_date = if (!baseline_id %in% event_id){
        min(event_date, na.rm = T)
      } else {
        max(event_date[event_id == baseline_id], na.rm = TRUE)
      },
      .by = subject_id
    ) |> 
    dplyr::distinct(subject_id, baseline_date)
  data |> 
    dplyr::left_join(baseline_dates, by = "subject_id") |> 
    dplyr::mutate(
      day = event_date - baseline_date,
      .by = subject_id
    ) |> 
    dplyr::select(-baseline_date) 
}

#' Merge event data from study data with metadata
#'
#' Collects all available events. Defines the correct order of events, and
#' creates event labels if needed.
#'
#' @param data A data frame with study data. Should contain  columns `event_id`
#'   and `vis_num` column.
#' @param events A data frame with events metadata, created with
#'   [get_metadata()].
#'
#' @return A data frame with clean event data, with event_label column as a
#'   factor with correct event_label levels.
#' @keywords internal
#' 
add_events_to_data <- function(
    data,
    events
){
  stopifnot("[data] should be a data frame" = is.data.frame(data))
  stopifnot("[events] should be a data frame" = is.data.frame(events))
  if (all(c("event_name", "event_label") %in% names(data))) { 
    cat("using pre-existing event_name and event_label\n")
    return(data) 
  }
  required_cols <- c("subject_id", "event_id", "day")
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if(length(missing_cols) != 0) {
    stop("The following columns are required but are missing: ", 
         paste0(missing_cols, collapse = ", "), ".")
  }
  # Goal: to create visit numbers in the order that events appear. 
  # Merging with data is needed because when visits are flexible (using event_id_pattern), 
  # the order of visits needs to be determined by means of when they occurred first.
  all_ids <- unique(data$event_id)
  event_patterns_to_order <- events |> 
    with(event_id_pattern[generate_labels & is_regular_visit]) |> 
    na.omit() |> 
    paste0(collapse = "|")
  if (nchar(event_patterns_to_order) == 0){
    cat("order can be derived from metadata\n")
    derived_event_order <- data.frame(event_id = character(), derived_order = character())
  } else {
    # deriving order by using date of first appearance if needed.
    derived_event_order <- unique(data[c("subject_id", "event_id", "day")]) |> 
      subset(grepl(event_patterns_to_order, event_id)) |> 
      # If visit occurs on multiple days in one patient (e.g. screening):
      dplyr::mutate(day = max(day, na.rm = TRUE), .by = c(subject_id, event_id)) |> 
      dplyr::mutate(derived_order = as.numeric(factor(day)), .by = subject_id) |> 
      dplyr::distinct(event_id, derived_order)
    if(any(duplicated(derived_event_order$derived_order))){
      cat("Event order is not unique based on event dates.",
          "Making a best guess to create a unique order.\n")
      derived_event_order <- derived_event_order |> 
        # selects the most unique order per event_id:
        dplyr::mutate(order_occurrence = dplyr::n(), .by = derived_order) |> 
        dplyr::slice_min(order_occurrence, by = event_id, with_ties = FALSE) |> 
        dplyr::select(-order_occurrence) |> 
        dplyr::arrange(event_id, derived_order) |> 
        dplyr::mutate(derived_order = dplyr::row_number())
    }
  }
  events_table <- events |> 
    dplyr::mutate(
      event_id = dplyr::coalesce(
        event_id, 
        sapply(event_id_pattern, \(x) paste0(all_ids[grepl(x, all_ids)], collapse = ","))
      )
    ) |> 
    expand_columns(columns = "event_id", separator = ",") |> 
    dplyr::left_join(derived_event_order, by = "event_id") |> 
    dplyr::arrange(meta_event_order, derived_order) |> 
    dplyr::distinct() |> # distinct really needed here? Not sure
    dplyr::mutate(
      event_order = dplyr::row_number(),
      vis_number = ifelse(is_regular_visit, cumsum(is_regular_visit), NA),
      vis_number = pmax(vis_number - vis_number[as.logical(is_baseline_event)], 0)
    ) |> 
    dplyr::mutate(
      event_name_custom = dplyr::coalesce(event_name_custom, event_id),
      # to create factors with correct levels (for compact timeline):
      event_label_custom = ifelse(is_regular_visit, dplyr::coalesce(event_label_custom, event_id), NA),
      event_label_custom = ifelse(
        generate_labels & !is.na(event_label_custom),
        paste0(event_label_custom, vis_number),
        event_label_custom
      ),
      event_label_custom = factor(event_label_custom, levels = unique(event_label_custom))
    )
  
  cols_to_remove <- c(names(events_table), "event_name_edc", "event_repeat_number")
  cols_to_remove <- cols_to_remove[!cols_to_remove == "event_id"]
  output <- data |> 
    dplyr::left_join(events_table, by = c("event_id")) |> 
    add_missing_columns("event_name_edc") |> 
    dplyr::mutate(
      event_repeat_number = as.numeric(factor(day)),
      .by = c(subject_id, event_name_custom)
    ) |>
    dplyr::mutate(
      event_name = dplyr::case_when(
        is.na(event_name_custom) ~ "Any visit",
        add_event_repeat_number ~ paste(event_name_custom, event_repeat_number),
        add_visit_number        ~ paste(event_name_custom, vis_number),
        .default = event_name_custom
      ),
      event_name = dplyr::case_when(
        event_name == "Any visit" ~ "Any visit",
        !is.na(event_name_edc) & 
          tolower(event_name_custom) != tolower(event_name_edc) ~ 
          paste0(event_name, " (", event_name_edc, ")"),
        .default = event_name
      ),
      event_label = event_label_custom
    ) |> 
    # to show events on start page chronologically:
    dplyr::arrange(subject_id, day, event_order) |> 
    dplyr::select(-dplyr::all_of(cols_to_remove))
  
  cat("Created the following event_label and event_name combinations:\n")
  print(unique(output[order(output$event_label), c("event_label", "event_name")]))
  output
}
