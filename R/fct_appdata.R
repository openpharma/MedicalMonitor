#' Get raw data from CSV files
#'
#' @param data_path Path to the folder that contains the CSV files with the
#'   application data.
#' @param synch_time Time at which the data was extracted from the EDC system.
#'   Defaults to the current date time. Important to set this correctly, since
#'   it will be shown in the application. By default, a warning will be given in
#'   the application if the synchronization time is more than one day old.
#' @param exclude character vector with regular expressions that identify csv
#'   files that should be excluded from the study data. Useful to exclude files
#'   with different data structures, or files with metadata.
#' @param delim Delimiter to use to read in files.
#' @param skip Number of rows to skip when reading in files.
#'
#' @return A data frame with raw application data.
#' @export
get_raw_csv_data <- function(
    data_path = Sys.getenv("RAW_DATA_PATH"),
    synch_time = time_stamp(),
    exclude = c("README.csv$", "Pending_forms.csv$", "MEDRA.csv$", "WHODrug.csv$", "_Queries.csv$"),
    delim = ",",
    skip = 1
){
  all_files <- list.files(data_path, pattern = ".csv")
  if(length(all_files) == 0) stop("No files found. Verify whether the path is correct.")
  synch_time <- synch_time %||% ""
  stopifnot(is.character(exclude))
  stopifnot(is.character(synch_time), length(synch_time) == 1)
  
  exclude_regex <- paste0(exclude, collapse = "|")
  all_files <- all_files[!grepl(exclude_regex, all_files)]
  
  raw_data <- readr::read_delim(
    file.path(data_path, all_files),  
    delim = delim, 
    skip = skip, 
    col_types = readr::cols(.default = readr::col_character()), 
    show_col_types = FALSE
  )
  if(identical(synch_time, "")){warning("No synch time provided")}
  cat("Adding synch time '", synch_time, "' as the attribute 'synch_time'",
      "to the data set.\n")
  attr(raw_data, "synch_time") <- synch_time
  raw_data
}

#' Merge metadata with raw data
#'
#' Study-specific function that will combine raw data gathered with
#' [get_raw_csv_data()] with study-specific metadata. It also fixes the metadata
#' suffix if needed, and renames the limits and significance values to the app
#' standard names. Some study-specific variables need to be created with this
#' step.
#'
#' @param data A data set with raw study data.
#' @param meta A list of data frames containing metadata.
#' @param expected_columns A character vector with the columns that should be
#'   expected in the data frame. If missing, these columns will be added with
#'   missing data (thus, will be made explicitly missing).
#'
#' @return A data frame.
#' @export
#' 
merge_meta_with_data <- function(
    data,
    meta,
    expected_columns = c("lower_limit", "upper_limit", "unit", 
                         "significance", "reason_notdone")
){
  stopifnot(is.data.frame(data))
  stopifnot(inherits(meta, "list"))
  stopifnot(is.character(expected_columns))
  # Preserve synch time manually since pivot functions do not preserve attributes
  synch_time <- attr(data, "synch_time") %||% ""
  merged_data <- data |> 
    rename_raw_data(column_names = meta$column_names) |> 
    readr::type_convert(clinsight_col_specs) |>
    apply_custom_functions(meta$settings$pre_merge_fns) |>
    add_timevars_to_data() |> 
    # fix MC values before merging:
    fix_multiple_choice_vars(expected_vars = meta$items_expanded$var) |> 
    dplyr::right_join(meta$items_expanded, by = "var") |> 
    dplyr::filter(!is.na(item_value)) |>
    apply_custom_functions(meta$settings$pre_pivot_fns) |>
    dplyr::mutate(
      suffix_names = suffix_names %|_|% ifelse(is.na(suffix) | grepl("ORRES$", suffix) | item_group == "General", "VAL", suffix)
    ) |> 
    dplyr::select(-var, -suffix) |> 
    dplyr::mutate(
      edit_date_time = max(edit_date_time, na.rm = TRUE), 
      .by = c(subject_id, item_name, event_name, event_repeat)
    ) |> 
    tidyr::pivot_wider(names_from = suffix_names, values_from = item_value) |> 
    add_missing_columns(expected_columns) |> 
    apply_custom_functions(meta$settings$post_pivot_fns) |>
    dplyr::rename(
      "lower_lim" = lower_limit,
      "upper_lim" = upper_limit,
      "item_unit" = unit,
      "item_value" = VAL
    ) |> 
    dplyr::mutate(
      region = region %|_|% ifelse(
        is.na(site_code), 
        "Missing", 
        gsub("_*[[:digit:]]+$", "", site_code)
      )
    ) |> 
    merge_item_pairs_by_suffix(suffix = "_ITEM_TO_MERGE_WITH_PAIR") |> 
    apply_custom_functions(meta$settings$post_merge_fns)
  attr(merged_data, "synch_time") <- synch_time
  merged_data
}


#' Apply study-specific suffix fixes
#' 
#' These changes are study/EDC-specific and part of the legacy code for ClinSight.
#' 
#' @param data A data frame
#' 
#' @return A data frame.
#' @keywords internal
apply_study_specific_suffix_fixes <- function(data) {
  dplyr::mutate(data,
    suffix = ifelse(item_name == "ECG interpretation", "LBCLSIG", suffix),
    suffix = ifelse(is.na(suffix), "VAL", suffix),
    # TODO: improve code below to handle exceptions in a more general manner
    suffix_names = ifelse(suffix %in% c("LBORRES", "VSORRES", "EGORRES") | 
                      item_group %in% c("Cytogenetics", "General"), 
                    "VAL", suffix)
  )
}

#' Apply EDC-specific suffix fixes
#' 
#' These changes are study/EDC-specific and part of the legacy code for ClinSight.
#' 
#' @param data A data frame
#' @param expected_columns A character vector with the columns that should be
#'   expected in the data frame. If missing, these columns will be added with
#'   missing data (thus, will be made explicitly missing).
#' 
#' @return A data frame.
#' @keywords internal
apply_edc_specific_changes <- function(
    data, 
    expected_columns = c("LBORNR_Lower", "LBORNR_Upper", "LBORRESU", 
                         "LBORRESUOTH", "LBCLSIG", "LBREASND")
) {
  data |> 
    add_missing_columns(expected_columns) |> 
    dplyr::mutate(
      lower_limit    = as.numeric(ifelse(!is.na(lower_limit), lower_limit, LBORNR_Lower)),
      upper_limit    = as.numeric(ifelse(!is.na(upper_limit), upper_limit, LBORNR_Upper)),
      LBORRESU       = ifelse(is.na(LBORRESU), unit, LBORRESU),
      LBORRESU       = ifelse(LBORRESU == "Other", LBORRESUOTH, LBORRESU),
      LBORRESU       = ifelse(is.na(LBORRESU), "(unit missing)", LBORRESU),
      unit           = LBORRESU,
      significance   = LBCLSIG,
      reason_notdone = LBREASND
    ) |> 
    dplyr::select(-dplyr::all_of(expected_columns))
}


#' Apply study-specific fixes
#'
#' These changes are probably study-specific and need to be changed accordingly.
#'
#' @param data A data frame (for example, raw data merged).
#' @param form_id_vars A character vector with the names of the columns that
#'   identify a form.
#' @return A data frame.
#' 
apply_study_specific_fixes <- function(
    data, 
    form_id_vars = c("subject_id", "event_name", "item_group")
){
  ## apply study-specific fixes:
  # fix significance in ECG before proceeding (stored in its own separate variable):
  ECG_significance <- data |> 
    dplyr::filter(item_name == "ECG interpretation") |> 
    dplyr::mutate(
      significance = dplyr::case_match(
        significance,
        "Normal"        ~ "within limits",
        "Abnormal, NCS" ~ "NCS",
        "Abnormal, CS"  ~ "CS"
      )
    ) |> 
    dplyr::select(dplyr::all_of(c(form_id_vars, "form_repeat", "significance")))
  data <- data |> 
    dplyr::filter(item_name != "ECG interpretation") |> 
    dplyr::rows_update(ECG_significance, by = c(form_id_vars, "form_repeat"))
  
  ## Add weight change:
  weight_change_data <- data |> 
    dplyr::filter(item_group == "Vital signs", item_name == "Weight") |> 
    get_base_value(c("item_name" = "Weight")) |> 
    dplyr::mutate(
      base_weight = as.numeric(base_weight),
      item_value = as.character(round(100*(as.numeric(item_value) - base_weight)/base_weight, 1)),
      upper_lim = 10,
      lower_lim = -10,
      item_name = "Weight change since screening",
      item_unit = "%",
    ) |> 
    dplyr::select(-base_weight)
  data <- dplyr::bind_rows(data, weight_change_data)
  
  # Rename dichotomic value that is only in the database if the answer is 'Yes'.
  data <- data |> 
    dplyr::mutate(
      item_value = ifelse(
        item_group == "Medication" & item_name == "CM MDS/AML Specific", 
        "Yes", 
        item_value
      ),
      .by = c(subject_id, form_repeat)
    ) 
}

#' Apply custom modification functions
#' 
#' @param data A data frame (for example, raw data merged).
#' @param functions A character vector containing the names of the functions to
#'   apply to the data. Default is NULL.
#' @param .default A character vector containing the names of the functions to
#'   apply if none are provided. Default is "identity".
#' @keywords internal
apply_custom_functions <- function(data, functions = NULL, .default = "identity") {
  Reduce(\(x1, x2) do.call(x2, list(x1)), # Apply next function to output of previous
         functions %||% .default, # Apply default functions if no additional functions provided
         init = data) # Initialize with the data object
}

#' Get appdata
#'  
#' Converts data to a list of data frames in the expected format to be used by the 
#' the Shiny application.
#'
#' @param data A data frame, commonly raw data merged with metadata.
#' @param meta A data frame with metadata. Used to convert the data set to the 
#' appropriate format.
#'
#' @return A list of data frames.
#' @export
#'
get_appdata <-  function(
    data,
    meta = metadata
){
  tableclasses <- gsub("create_table.", "", as.character(utils::methods("create_table")))
  var_levels <- dplyr::distinct(meta$items_expanded, form_type, item_name, item_group)
  
  data <- split(data, ~item_group)
  ## Apply changes specific for continuous data:
  appdata <- lapply(data, \(x){
    if(nrow(x) == 0) return(x)
    item_group_x <- unique(x$item_group)
    if(length(item_group_x) != 1) stop(
      "item_group consists of multipe elements which is not allowed: ", 
      item_group_x
    )
    form_type_x <- unique(with(var_levels, form_type[item_group == item_group_x]))
    if(length(form_type_x) != 1) stop(
      "form_type consists of multipe elements which is not allowed: ", 
      form_type_x
    )
    tableclass <- simplify_string(form_type_x)
    if(tableclass %in% tableclasses){
      class(x) <- unique(c(tableclass, class(x)))
    }
    tableclass <- simplify_string(item_group_x)
    if(tableclass %in% tableclasses){
      class(x) <- unique(c(tableclass, class(x)))
    }
    if(!all(x$item_type == "continuous")) return(x)
    df <- x |> 
      dplyr::mutate(
        # to preserve the order of the variables in the metadata file:
        item_name = factor(
          item_name, 
          levels = with(var_levels, item_name[item_group == item_group_x])
        )
      ) |> 
      # using group_by for imputing upper limit with max value if needed
      dplyr::group_by(site_code, item_name) |> 
      dplyr::mutate(
        item_value = as.numeric(item_value),
        upper_lim = ifelse(is.na(upper_lim) & !is.na(lower_lim), pmax(1.5*max(lower_lim), max(item_value, na.rm = TRUE)), upper_lim),
        lower_lim = ifelse(is.na(lower_lim) & !is.na(upper_lim), 0, lower_lim),
        value_scaled = ifelse(lower_lim == 0 & upper_lim == 0, item_value, (item_value-lower_lim)/(upper_lim-lower_lim)),
        out_of_lim = ifelse(item_value>upper_lim | item_value<lower_lim, 1, 0),
        significance = dplyr::case_when(
          significance == "NCS"                   ~ "out of limits, clinically insignificant",
          significance == "CS"                    ~ "out of limits, clinically significant",
          is.na(out_of_lim) & is.na(significance) ~ "limits unknown",
          out_of_lim == 0                         ~ "within limits",
          is.na(significance) & out_of_lim == 1   ~ "out of limits, significance pending",
          TRUE   ~ significance
        ),
        out_of_lim = factor(out_of_lim), # for vital signs figures
        significance = factor(significance, levels = names(col_palette)),
        text_label = paste0(
          "<b>", subject_id, "</b>",
          "\n",
          event_date,
          "\n",
          event_name, " (day ",
          day, ")",
          "\nValue: ",
          round(item_value, 2),
          " ",
          item_unit
        )
      ) |> 
      dplyr::ungroup() 
    class(df) <- unique(c("continuous", class(x)))
    df
  }) 
  appdata
}


