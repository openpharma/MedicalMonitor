#' Create table
#' 
#' Creates a wide form data table based on custom S3 classes. 
#'
#' @param data 
#' A data frame to be converted. 
#' Based on the custom class assigned to the table, a different 
#' transformation will be used. 
#' @param ... Other possible options
#'
#' @return A table in wide format.
#' @export
#' 
#' @seealso [create_table.default()], [create_table.continuous()], 
#' [create_table.adverse_events()], [create_table.medication()], 
#' [create_table.medical_history()], [create_table.bm_cytology()], 
#' [create_table.conc_procedures()]. 
#' 
create_table <- function (data, ...) {
  UseMethod("create_table", data)
}

#' Create wide table
#'
#' @param data A data frame to convert, in long format.
#' @param keep_vars all common variables that identify a unique single data row.
#' @param name_column Character string. Column that contains all the names for
#'   the wide table format.
#' @param value_column Character string. Column that contains all the values of
#'   the table in wide format.
#' @param expected_columns A character vector with the names of the expected
#'   variables. Needed to make sure that all expected columns are always
#'   created, even if some variables are implicitly missing (which might occur
#'   if there are not yet any values available for a specific variable). Also,
#'   implicitly missing variables might give errors if part of the script relies
#'   on the variables' presence.
#' @param ... Other options. Currently unused.
#'
#' @return A data frame in wide format
#' @export
#' 
create_table.default <- function(
    data, 
    name_column = "item_name",
    value_column = "item_value",
    keep_vars = c("subject_id", "event_name", "event_repeat", "event_date"),
    expected_columns = NULL,
    ...
){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(keep_vars))
  stopifnot(is.character(name_column))
  stopifnot(is.character(value_column))
  if ("reviewed" %in% names(data)) {
    data <- add_o_reviewed(data, keep_vars)
    keep_vars <- c("o_reviewed", keep_vars)
  }
  df <- data[c(keep_vars, name_column, value_column)] |> 
    tidyr::pivot_wider(
      names_from = {{name_column}}, 
      values_from = {{value_column}}, 
      values_fn = ~paste0(., collapse = "; ")
      )
  expected_columns <- na.omit(expected_columns) %||% character(0)
  if(length(expected_columns) == 0) return(df)
  add_missing_columns(df, expected_columns)[
    unique(c(keep_vars, expected_columns))
    ]
}

#' Add Overall Reviewed Field
#'
#' Adds a field to the data set summarizing the overall review status over the
#' rows uniquely defined by the ID columns.
#'
#' @param data A data frame to mutate
#' @param id_cols A set of columns that uniquely identify each observation
#'
#' @details This function servers as a helper to `create_table.default()`. If
#' the field `reviewed` is contained in the data frame, an overall review status
#' field will be added to the data frame. The field is a list consistent of two
#' named elements: `reviewed` and `ids`. The `reviewed` field is `TRUE` if all
#' records are reviewed, `FALSE` if all records are not reviewed, and `NA` if
#' some records are reviewed and some are not. The `ids` field contains a vector
#' of the IDs associated with the unique observation defined by `id_cols`.
#' 
#' @noRd
add_o_reviewed <- function(data, id_cols) {
  dplyr::mutate(
    data,
    o_reviewed = dplyr::case_when(
      any(reviewed == "No") & any(reviewed == "Yes") ~ list(list(reviewed = NA, ids = id)),
      any(reviewed == "Yes") ~ list(list(reviewed = TRUE, ids = id)),
      .default = list(list(reviewed = FALSE, ids = id))
    ),
    .by = dplyr::all_of(id_cols))
}


#' Create Table with continuous data.
#'
#' @param unit_column Character vector. The name of the column that contains
#'   units (as character values).
#' @param explanation_column Character vector. The name of the column that
#'   contains an explanation why a value is missing (if available).
#' @export
#' @inherit create_table.default
create_table.continuous <- function(
    data, 
    name_column = "item_name",
    value_column = "item_value", 
    unit_column = "item_unit",
    explanation_column = "reason_notdone",
    keep_vars = c("subject_id", "event_name"),
    expected_columns = NULL,
    ...
    ){
  stopifnot(is.data.frame(data))
  if(is.null(explanation_column)){
    explanation_column <- "dummy_expl_colname"
    data[[explanation_column]] <- ""
  }
  data[[explanation_column]] <- tidyr::replace_na(data[[explanation_column]], "")
  if(is.null(unit_column)){
    unit_column <- "dummy_unit_colname"
    data[[unit_column]] <- ""
  }
  data[[unit_column]] <- tidyr::replace_na(data[[unit_column]], "")
  df <- data |> 
    dplyr::mutate(
      "{unit_column}" := ifelse(
        is.na(.data[[value_column]]), "", .data[[unit_column]]
        ),
      "{value_column}" := as.character(.data[[value_column]]),
      "{value_column}" := dplyr::case_when(
        is.na(.data[[value_column]]) & !.data[[explanation_column]] == "" ~ 
          paste0("missing (", .data[[explanation_column]], ")"),
        is.na(.data[[value_column]]) & .data[[explanation_column]] == ""  ~ 
          "missing (reason unknown)",
        TRUE ~ .data[[value_column]]
      )
    ) |> 
    tidyr::unite(col = "VAL", dplyr::all_of(c(value_column, unit_column)),  sep = " ") 
  create_table.default(data = df, name_column = name_column, 
                        value_column = "VAL", keep_vars = keep_vars, 
                       expected_columns = expected_columns)
}

#' Get general data
#'
#' Creates a wide dataset with general data of participants for use within
#' widgets.
#'
#' @export
#' @inherit create_table.default
create_table.general <- function(
    data,
    name_column = "item_name",
    value_column = "item_value", 
    keep_vars = c("subject_id"),
    expected_columns = NULL,
    ...
    ){
  expected_columns <- na.omit(expected_columns) %||% character(0)
  df_names <- c(keep_vars, name_column, value_column, expected_columns)
  if(is.null(data)) {
    data <-  data.frame(matrix(ncol = length(df_names))) |> 
      setNames(df_names)
  }

  df <- data |> 
    dplyr::filter(!item_name %in% c("DrugAdminDate", "DrugAdminDose")) |>
    create_table.default(name_column, value_column, keep_vars, expected_columns)
  
  df |> 
    dplyr::mutate(
      status = ifelse(
        is.na(Eligible), 
        "Unknown",
        ifelse(
          Eligible == "No", 
          "Screen failure", 
          ifelse(
            Eligible == "Yes",
            "Enrolled",
            Eligible
          )
        )
      ),
      status = ifelse(
        !is.na(DiscontinuationDate),
        ifelse(
          is.na(DiscontinuationReason), 
          "Discontinued", 
          DiscontinuationReason
          ),
        status
        ),
      status_label = paste0(
        "<b>", subject_id, "</b><br>",
        "<b>Sex:</b> ",    Sex, "<br>",
        "<b>Age:</b> ",    Age, "yrs.", "<br>",
        "<b>Status:</b> ", status, "<br>",
        "<b>ECOG:</b> ",   ECOG, "<br>",
        "<b>Dx:</b> ",     WHO.classification
      ) 
    )  
}


#' Create Default 'Common Events' Table
#'
#' @export
#' @inherit create_table.default 
create_table.common_forms <- function(
    data, 
    name_column = "item_name",
    value_column = "item_value",
    keep_vars = c("subject_id", "form_repeat"),
    expected_columns = NULL,
    ...
){
  create_table.default(data, name_column, value_column, 
                       keep_vars, expected_columns)
}


#' Create Adverse Events table
#'
#' Function to create an adverse event dataset.
#'
#' @param worsening_start_column Character vector. Name of the column containing
#'   the date at which worsening of the (S)AE started.
#' @param SAE_start_column Character vector. Name of the column containing the
#'   SAE start date.
#' @param AE_start_column Character vector. Name of the column containing the AE
#'   start date.
#' @param AE_end_column Character vector. Name of the column containing the AE
#'   end date.
#' @export
#' @inherit create_table.default
create_table.adverse_events <- function(
    data,
    name_column = "item_name",
    value_column = "item_value", 
    keep_vars = c("subject_id", "form_repeat"), 
    expected_columns = NULL,
    worsening_start_column = "date of worsening",
    SAE_start_column = "SAE Start date",
    AE_start_column = "start date",
    AE_end_column = "end date",
    ...
){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(keep_vars))
  stopifnot(is.character(name_column))
  stopifnot(is.character(value_column))
  df <- create_table.default(data, name_column, value_column, 
                             keep_vars, expected_columns) |> 
    adjust_colnames("^AE ") 
  df[["Number"]] <- NULL
  
  # create new row when an AE gets worse:
  df_worsening <- df[!is.na(df[[worsening_start_column]]), ] |> 
    dplyr::mutate(
      "{AE_start_column}" := .data[[worsening_start_column]],
      # get new SAE start date. This is the day that the SAE got worse. If the 
      # worsening made the AE an SAE, the date of worsening will be the same as the SAE start date.
      "{SAE_start_column}" := dplyr::if_else(!is.na(.data[[SAE_start_column]]), 
                                .data[[worsening_start_column]], 
                                .data[[SAE_start_column]]),
      `CTCAE severity` = `CTCAE severity worsening`
    ) 
  df <- df |> 
    dplyr::mutate(
      "{AE_end_column}" := dplyr::if_else(!is.na(.data[[worsening_start_column]]), 
                          .data[[worsening_start_column]], .data[[AE_end_column]]),
      # below removes SAE tag from AEs, if the event was first an AE and then became a SAE at a later date. 
      # These events are added later with the `worsening` data frame, so the information is not lost.
      `Serious Adverse Event` = dplyr::if_else(
        !is.na(.data[[SAE_start_column]]) & 
          !is.na(.data[[worsening_start_column]]) &
          (.data[[AE_start_column]] != .data[[SAE_start_column]]),
        "No", 
        `Serious Adverse Event`
        )
    ) |> 
    dplyr::bind_rows(df_worsening) |> 
    dplyr::arrange(dplyr::desc(.data[["Serious Adverse Event"]]), 
                   .data[["form_repeat"]], .data[["start date"]])
  df |> 
    dplyr::select(
      -dplyr::all_of(c(worsening_start_column, "CTCAE severity worsening"))
    )
}


#' Create Concomitant Medication Table
#'
#' @export
#' @inherit create_table.default 
create_table.medication <- function(
    data,
    name_column = "item_name",
    value_column = "item_value", 
    keep_vars = c("subject_id", "form_repeat"),
    expected_columns = NULL,
    ...
){
  df <-  data |> 
    create_table.default(name_column, value_column, keep_vars, expected_columns) |> 
    adjust_colnames("^CM ") 
  df[["Number"]] <- NULL
  df <- df |> 
    dplyr::mutate(
      `Unit`      = ifelse(!is.na(`Unit Other`), `Unit Other`, `Unit`),
      `Frequency` = ifelse(!is.na(`Frequency Other`), `Frequency Other`, `Frequency`),
      `Route`     = ifelse(!is.na(`Route Other`), `Route Other`, `Route`)
    ) |> 
    dplyr::select(-dplyr::ends_with("Other"))
  
  df |> 
    dplyr::mutate(
      Name = paste0(tools::toTitleCase(tolower(.data[["Active Ingredient"]])), " (",
                    tools::toTitleCase(tolower(.data[["Trade Name"]])),
                    ")"),
      Dose = paste0(.data[["Dose"]], " ", .data[["Unit"]], " ", 
                    .data[["Frequency"]], "; ", .data[["Route"]]),
      in_use = (is.na(.data[["End Date"]])) 
    ) |> 
    dplyr::arrange(dplyr::desc(in_use), dplyr::desc(`Start Date`)) |> 
    dplyr::select(
      dplyr::any_of("o_reviewed"),
      dplyr::all_of(c(keep_vars, "Name")), 
      dplyr::everything(),
      -dplyr::all_of(c("in_use", "Active Ingredient", "Trade Name", 
                       "Unit", "Frequency", "Route"))
    )
}


#' Create Medical History Table
#'
#' @export
#' @inherit create_table.default 
create_table.medical_history <- function(
    data,
    name_column = "item_name",
    value_column = "item_value", 
    keep_vars = c("subject_id", "form_repeat"),
    expected_columns = NULL,
    ...
){
  df <- data |> 
    create_table.default(name_column, value_column, keep_vars, expected_columns) |> 
    adjust_colnames("^MH ") 
  df[["Number"]] <- NULL
    
  df |> 
    dplyr::mutate(Name = tools::toTitleCase(tolower(Name)))
}

#' Create Concomitant Procedures Table
#'
#' @export
#' @inherit create_table.default 
create_table.conc_procedures <- function(
    data,
    name_column = "item_name",
    value_column = "item_value", 
    keep_vars = c("subject_id", "form_repeat"),
    expected_columns = NULL,
    ...
){
  df <- data |> 
    create_table.default(name_column, value_column, keep_vars, expected_columns) |> 
    adjust_colnames("^CP ")
  df[["Number"]] <- NULL
  df
}

#' Create BM Cytology Table
#'
#' @export
#' @inherit create_table.default 
create_table.bm_cytology <- function(
    data,
    name_column = "item_name",
    value_column = "item_value", 
    keep_vars = c("subject_id", "event_name", "event_repeat", "event_date"),
    expected_columns = NULL,
    ...
){
  df <- create_table.default(data, name_column, value_column, keep_vars, expected_columns)
  df |> 
    dplyr::mutate(
      `Auer Rods` = ifelse(.data[["Auer Rods detectable?"]] == "No", "None", .data[["Auer Rods"]]),
      `Ringed Sideroblasts` = ifelse(.data[["Ringed Sideroblasts detectable?"]] == "No", "None", .data[["Ringed Sideroblasts"]])
    ) |> 
    dplyr::select(
      -tidyr::all_of(c("Auer Rods detectable?", "Ringed Sideroblasts detectable?"))
    )
}

# TODO: create a function like the one below. Not yet done due to time restrictions.
# merge_other_category <- function(
#     data, 
#     name_column = "item_name",
#     value_column = "item_value", 
#     var_name = c(""), 
#     var_name_other
# ){
#   
# }
