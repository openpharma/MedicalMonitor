#' Load metadata
#'
#' Loads all sheets of an Excel file in a named list. Used for reading in
#' metadata.
#'
#' @param filepath Filepath to the .xlsx file.
#' @param expand_tab_items Character vector with the names of the tabs of which
#'   the items need to be expanded. If not empty, a new data frame will be
#'   created named 'expanded_items', containing all items in the tabs of
#'   `expand_tab_items`. Will abort if a tab name is provided that does not
#'   exist in the metadata.
#' @param expand_cols Column names containing the columns for expansion. Will be
#'   ignored if the variable `expand_tab_items` is left empty.
#'
#' @return A list with data frames.
#' @export
#' 
get_metadata <- function(
    filepath,
    expand_tab_items = c("common_forms", "study_forms", "general"),
    expand_cols = "suffix"
){
  stopifnot(is.character(filepath))
  if(!grepl(".xlsx$", filepath)) stop(
    "currently only .xlsx files are supported as metadata input"
  )
  sheets <- readxl::excel_sheets(filepath)
  sheets <- setNames(sheets, sheets)
  meta <- lapply(sheets, function(x){
    readxl::read_excel(filepath, sheet = x, col_types = "text")
  })
  
  meta$settings <- meta$settings |> 
    lapply(\(x) as.character(na.omit(x))) |> 
    Filter(f = length)
  
  meta$settings$treatment_label <- meta$settings$treatment_label %||% "\U1F48A T\U2093" |> 
    # So that raw Unicode will be converted correctly:
    sprintf(fmt = '"%s"') |>
    str2expression() |>
    as.character()
  
  if(length(expand_tab_items[nchar(expand_tab_items) > 0 ] ) == 0) return(meta)
  if("items_expanded" %in% names(meta)) warning({
    "Table 'items_expanded' already present. The old table will be overwritten."
  })
  missing_tab_items <- expand_tab_items[!expand_tab_items %in% names(meta)]
  if(length(missing_tab_items) > 0) {
    stop_message <- paste0(
      "The following tab names in 'expand_tab_items' were not found", 
      " in the metadata: ", 
      paste0(missing_tab_items, collapse = ", "), 
      ". \nAre all tab names spelled correctly?"
    )
    stop(stop_message)
  }
  
  ### Verify and clean items_expanded
  meta$items_expanded <- meta[expand_tab_items] |> 
    dplyr::bind_rows(.id = "form_type") |> 
    expand_columns(
      columns = expand_cols,
      separator = ",",
      unite_with = "var",
      remove_cols = FALSE
    ) 
  ### Verify and clean form-level data:
  meta[["form_level_data"]] <- get_form_level_data(
    meta[["form_level_data"]], 
    all_forms = unique(meta$items_expanded$item_group)
  )
  
  ## Verify and clean event data:
  meta[["events"]] <- clean_event_metadata(meta[["events"]])
  
  missing_cols <- required_meta_cols[!required_meta_cols %in% names(meta$items_expanded)]
  if(length(missing_cols) != 0){
    warning(
      sprintf(
        "Required column '%s' will be created since it is missing in the metadata\n", 
        missing_cols
      )
    )
    meta$items_expanded <- add_missing_columns(meta$items_expanded, missing_cols)
  }
  
  lapply(setNames(nm = names(meta)), \(x){
    if(!x %in% expand_tab_items) return(meta[[x]])
    meta[[x]] |> 
      dplyr::select(-var, -suffix) |> 
      as.data.frame()
  })
}

#' Rename raw data
#'
#' Helper function to rename raw data
#'
#' @param data A data frame with raw study data.
#' @param column_names A data frame with column names. Should have at
#'   least the columns `name_raw`, containing the current column names, and
#'   `name_new`, containing the new column names. `name_new` should contain all
#'   names that are required for ClinSight to function properly
#'   (`required_col_names`).
#'
#' @return A data frame
#'
#' @keywords internal
rename_raw_data <- function(
    data, 
    column_names = metadata$column_names
){
  stopifnot("[data] should be a data frame" = is.data.frame(data))
  stopifnot("[column_names] should be a data frame" = is.data.frame(column_names))
  if(!all(c("name_raw", "name_new") %in% names(column_names))){
    stop("Expecting the columns 'name_raw' and 'name_new' in [column_names]")
  }
  missing_colnames <- with(column_names, name_raw[!name_raw %in% names(data)]) |> 
    paste0(collapse = ", ")
  if(nchar(missing_colnames) > 0) stop(
    paste0("The following columns are missing in the raw data while they are ", 
           "defined in 'name_raw' of column_names:\n", 
           missing_colnames, ".")
  )
  missing_new_cols <- required_col_names[!required_col_names %in% column_names$name_new] |> 
    paste0(collapse = ", ")
  if(nchar(missing_new_cols) > 0) stop(
    paste0("The following columns are missing in 'name_new' of column_names while they ", 
           "are required for ClinSight to run:\n", 
           missing_new_cols, ".")
  )
  # Remove unneeded colums, and rename them:
  df <- data[column_names$name_raw] |> 
    setNames(column_names$name_new)
  # Remove rows without subject_id and return results:
  df[!is.na(df$subject_id), ]
}

#' Correct multiple choice variables
#'
#' In some EDC systems, if there is a multiple choice variable in which multiple
#' answers are possible, the variable will be renamed with a suffix with the
#' multiple answers in it. For example var1, var2, for answers 1 and 2. This
#' function cleans this specific output so that the variable name remains
#' consistent.
#'
#' @param data A data frame. 
#' @param expected_vars Character vector containing the expected names of the
#'   variables.
#' @param var_column column name in which the variable names are stored
#' @param value_column column name in which the values of the variables are
#'   stored
#' @param suffix Multiple choice suffix. Used to define multiple choice values
#' @param common_vars variables used for identifying unique rows in the dataset.
#' @param collapse_with character value to collapse the multiple choice options
#'   with. If this value is NULL, the rows will be left as is.
#'
#' @return data frame with corrected multiple choice variables
#' @examples
#'  df <- data.frame(
#'   ID = "Subj1",
#'   var = c("Age", paste0("MH_TRT", 1:4)),
#'   item_value = as.character(c(95, 67, 58, 83, 34))
#'  )
#'  fix_multiple_choice_vars(df, common_vars = "ID")
#' @export
#' 
fix_multiple_choice_vars <- function(
    data,
    expected_vars = metadata$items_expanded$var,
    var_column = "var",
    value_column = "item_value",
    suffix = "[[:digit:]]+$",
    common_vars = c("subject_id", "event_repeat", "event_date", "form_repeat"),
    collapse_with = "; "
){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(expected_vars))
  stopifnot("var_column should be a vector of length 1" = {
    is.character(var_column) & length(var_column) == 1
    })
  stopifnot("suffix should be a character vector of length 1" = {
    is.character(suffix) & length(suffix) == 1
  })
  stopifnot(is.character(common_vars))
  if(!is.null(collapse_with)){
    stopifnot("collapse_with should be a character vector of length 1" = {
      is.character(collapse_with) & length(collapse_with) == 1
    })
  }
  
  all_vars <- unique(data[[var_column]])
  
  missing_vars <- expected_vars[!expected_vars %in% all_vars]
  if(length(missing_vars) == 0) return(data)
  
  vars_to_adjust <- lapply(
    missing_vars, 
    \(x){all_vars[grep(paste0("^", x, suffix), all_vars)] }
  )
  
  multiple_choice_vars <- missing_vars[sapply(vars_to_adjust, length) != 0]
  if(length(multiple_choice_vars) == 0) return(data)
  cat("multiple choice vars that will be adjusted: ", multiple_choice_vars, sep = "\n")
  data_adjusted <- data |> 
    dplyr::filter(.data[[var_column]] %in% unlist(vars_to_adjust), !is.na(.data[[value_column]])) |> 
    dplyr::arrange(.data[[var_column]]) |> 
    dplyr::mutate(var = gsub(suffix, "", var)) 
  if(!is.null(collapse_with)){
    # Probably redundant since the variables will be collapsed already in the 
    # functions `create_table.xxx`. 
    data_adjusted <- data_adjusted |>   
      dplyr::mutate(
        item_value = paste0(item_value, collapse = collapse_with),
        .by = dplyr::all_of(c(var_column, common_vars))
      )
  }
  # note: Column edit_date_time can still cause multiple rows after step below.
  data_adjusted <- dplyr::distinct(data_adjusted)
  
  df <-  data |> 
    dplyr::filter(!.data[[var_column]] %in% unlist(vars_to_adjust)) |> 
    dplyr::bind_rows(data_adjusted)
  df  
}


#' Get meta variables
#' 
#' Prepares and extracts consistent variable names based on both metadata 
#' and the provided data. To be used in both the UI and server functions 
#' of a Shiny app. 
#'
#' @param data List. Data to use.
#' @param meta List. metadata to use.
#'
#' @return a list with all important names to be used in a clinical trial.
#' @export
#'
get_meta_vars <- function(data = appdata, meta = metadata){
  stopifnot(inherits(data, "list"))
  stopifnot(inherits(meta, "list"))
  if(length(data) == 0) stop("Empty list with data provided")
  vars <- list()
  # add metadata variables:
  vars$items <- meta$items_expanded |> 
    dplyr::distinct(item_name, item_group) |> 
    split(~item_group) |> 
    lapply(\(x){setNames(simplify_string(x$item_name), x$item_name)})
  study_forms <- unique(meta$study_forms$item_group)
  common_forms <- unique(meta$common_forms$item_group)
  vars$all_forms <- data.frame(
    "main_tab" = c(
      rep("Common events", times = length(common_forms)),
      rep("Study data", times = length(study_forms))
      ),
   "form" = c(common_forms, study_forms)
  )
  
  # add variables dependent on dataset:
  vars$subject_id <- order_string(get_unique_vars(data, "subject_id")[[1]])
  vars$Sites     <- get_unique_vars(data, c("site_code", "region")) |> 
    dplyr::arrange(factor(site_code, levels = order_string(site_code)))
  vars$table_names <- setNames(meta$table_names$raw_name, meta$table_names$table_name) 
  # adding form-level data here since it meta vars are already passed through in 
  # the modules that need this information (e.g. mod_main_sidebar):
  vars$form_level_data <- meta$form_level_data
  vars
}

#' Get form-level data.
#'
#' Internal function to clean form-level data and return a data frame with all
#' forms that should be specified, and include form-level data for all of them.
#' Will also set default values (as defined in the package) if the value is not
#' set and/or is missing.
#'
#' @param data A data frame with form-level data. Should at least contain the
#'   `form_column`.
#' @param all_forms A character vector containing the names of all forms for
#'   which form-level data should be specified.
#' @param form_column Character string with the column in which the form names
#'   are stored in `data`.
#'
#' @return A cleaned data frame with form-level data.
#' 
#' @keywords internal
get_form_level_data <- function(
    data,
    all_forms,
    form_column = "item_group"
){
  stopifnot(is.character(form_column))
  stopifnot(is.character(all_forms))
  all_forms_df <- setNames(data.frame(all_forms), form_column)
  default_data <- data.frame(all_forms_df, form_level_defaults)

  if(is.null(data) || !is.data.frame(data) ){
    warning("No valid update table found. Falling back to defaults.")
    return(default_data)
  }
  
  if(!form_column %in% names(data)){
    stop(sprintf("'%s' missing in 'form_level_data' table.", form_column))
  }
  
  missing_forms <- data[!data[[form_column]] %in% all_forms, ][[form_column]]
  if(length(missing_forms) != 0){
    warning(
      "Ignoring vars defined in 'form_level_data' but not in metadata:\n",
      sprintf("'%s' ", missing_forms)
      )
    data <- data[!data[[form_column]] %in% missing_forms, ]
  }
  
  if(nrow(data) == 0){
    warning("No forms with form-level data found. Returning defaults.")
    return(default_data)
  }
  # Return data in two steps to preserve the order as in `all_forms`
  
  # Add missing columns
  data <- data |> 
    add_missing_columns(names(form_level_defaults)) |> 
    readr::type_convert(col_types = form_level_default_specs)
  
  # Use default only if value is missing after type conversion:
  all_forms_df |> 
    dplyr::left_join(data, by = form_column) |>
    tidyr::replace_na(as.list(form_level_defaults))
}

#' Get base value
#' 
#' Adds base value to a long-format data frame.
#'
#' @param data data frame in long format.
#' @param var A named character vector with the variable name to use. 
#' The name of the character vector should match the column in the data frame with the variable names. 
#' @param event A names character vector with the event that identifies the baseline value. 
#' The name of the character vector should match the column in the data frame with the event names. 
#' @param value_column A string containing the column name with the item values.
#' @param id_column The columns identifying a unique participant (subject_id).
#'
#' @return as data frame with an additional column named "base_`varname`". 
#' @export
#' @examples
#'  library(dplyr)
#'  library(tidyr)
#'  # create a long time series data frame:
#'  df <- as.data.frame(EuStockMarkets) |> 
#'   mutate(time = time(EuStockMarkets)) |>
#'   pivot_longer(-time)
#'  output <- get_base_value(df, c("name" = "DAX"), event = c("time" = df$time[1]),
#'                           value_column = "value", id_column = NULL)
#' 
get_base_value <- function(
    data,
    var = c("item_name" = "Weight"),
    event = c("event_name" = "Screening"),
    value_column = "item_value",
    id_column = "subject_id"
){
  stopifnot(is.data.frame(data), is.character(var), 
            inherits(event, c("numeric", "character")), 
            is.character(value_column))
  stopifnot("'var' should be a named character vector" = !is.null(names(var)))
  stopifnot("'event' should be a named character vector"= !is.null(names(event)))
  basevar_colname <- paste0("base_", simplify_string(var))
  
  if(nrow(data) == 0) return({data[[basevar_colname]] <- double(0); data})
  id_column <- if(is.null(id_column)) character(0) else id_column
  keep_cols <- na.omit(c(id_column, value_column))
  df <- data[data[[names(var)]] == var & data[[names(event)]] == event, keep_cols]
  
  if(nrow(df) == 0) return({data[[basevar_colname]] <- NA_real_; data})
  names(df)[length(names(df))] <- basevar_colname
  if(length(id_column) == 0) return({cbind(data, df)})
  dplyr::left_join(data, df, by = id_column)
}


#' Adjust column names 
#' 
#' Small helper function to adjust column names in a data frame. Can be used in 
#' a `magrittr` or native `R` pipe. 
#' 
#' Used in the application in the [create_table()] form-specific S3 methods 
#' to remove common prefixes for better table layout. For example, the prefix 
#' "AE" in front of all column names of the Adverse event table can be removed 
#' with this function.
#' 
#' @param data A data frame
#' @param pattern Pattern to search for in the names.
#' @param replacement Replacement for the pattern. Default to empty (thus, 
#' removing the pattern from the data frame names).
#'
#' @return A data frame with adjusted column names.
#' @export
#'
#' @examples
#' adjust_colnames(head(iris), "^Sepal", "Flower")
adjust_colnames <- function(
    data,
    pattern,
    replacement = ""
){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(pattern), is.character(replacement))
  names(data) <- gsub(pattern = pattern, replacement = replacement, names(data))
  data
}

#' Add missing columns
#' 
#' Small helper function. Checks if all expected columns are in the data frame 
#' and if not, adds a column with all missing variables in the data. Thus, the 
#' columns will be made explicitly missing in the data frame. 
#' 
#' @param data A data frame. 
#' @param columns Expected columns in the data frame.
#'
#' @return A data frame with at least all the columns named in `columns`. 
#' The added columns will be of class `character`. 
#' @export
#'
#' @examples
#' add_missing_columns(head(iris), c("important_column1", "important_column2"))
add_missing_columns <- function(
    data,
    columns
){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(columns))
  n_columns_input <- length(columns)
  columns <- columns[!is.na(columns)]
  columns <- columns[!columns == ""]
  if(length(columns) == 0) {
    warning("No non-missing columns defined in `columns`. 
            Returning data frame unaltered.")
    return(data)
  } 
  if(length(columns) < n_columns_input){
    warning(
      paste0(
        n_columns_input - length(columns), 
        " missing or empty strings removed from the 'columns' argument"
      )
    )
  }
  for(i in columns){
    if(!i %in% names(data)){
      data[[i]] <- rep(NA_character_, times = nrow(data))
    }
  }
  data
}

#' Custom interactive datatable
#'
#' Small wrapper around [DT::datatable()]. Will be used to create tables in a
#' consistent style.
#'
#' @param data A data frame to use within the application.
#' @param rename_vars An optional named character vector. If provided, it will
#'   rename any column names found in this vector to the provided name.
#' @param title Optional. Character string with the title of the table.
#' @param selection See [DT::datatable()]. Default set to 'single'.
#' @param extensions See [DT::datatable()]. Default set to 'Scroller'.
#' @param dom See \url{https://datatables.net/reference/option/dom}. A div
#'   element will be inserted before the table for the table title. Default set
#'   to 'fti' resulting in 'f<"header h5">ti'.
#' @param options See [DT::datatable()]. Must be a list.
#'   * Modifiable defaults:
#'     * `scrollY = '400px'`
#'     * `scrollX = TRUE`
#'     * `scroller = TRUE`
#'     * `deferREnder = TRUE`
#'     * `scrollResize = TRUE`
#'     * `scrollCollapse = TRUE`
#'     * `colReorder = TRUE`
#'   * Non-modifiable defaults:
#'     * `dom`: Defined by the `dom` parameter.
#'     * `initComplete`: Defaults to a function to insert table title into dataTable container.
#' @param allow_listing_download Logical, whether to allow the user to download
#'   the table as an Excel file. Defaults to the `allow_listing_download`
#'   configuration option in `golem-config.yml`, but can be overwritten here if
#'   needed.
#' @param export_label Character string with the table export label. Only used
#'   for downloadable tables (if `allow_listing_download` is `TRUE`).
#' @param ... Other optional arguments that will be passed to [DT::datatable()].
#'
#' @return A `DT::datatable` object.
#' @export
#'
#' @examples datatable_custom(mtcars)
datatable_custom <- function(
    data, 
    rename_vars = NULL, 
    title = NULL, 
    selection = "single",
    extensions = c("Scroller", "ColReorder"),
    dom = "fti",
    options = list(),
    allow_listing_download = NULL,
    export_label = NULL,
    ...
    ){
  stopifnot(is.data.frame(data))
  colnames <- names(data)
  if(!is.null(rename_vars)){
    stopifnot(is.character(rename_vars))
    colnames <- dplyr::rename(data[0,], dplyr::any_of(rename_vars)) |> 
      names()
  }
  stopifnot(is.null(title) | is.character(title))
  stopifnot(grepl("t", dom, fixed = TRUE))
  stopifnot(is.list(options))
  allow_listing_download <- allow_listing_download %||% 
    get_golem_config("allow_listing_download")
  stopifnot(is.null(allow_listing_download) | is.logical(allow_listing_download))
  stopifnot(is.null(export_label) | is.character(export_label))
  
  default_opts <- list(
    scrollY = 400,
    scrollX = TRUE,
    scroller = TRUE,
    deferRender = TRUE,
    scrollCollapse = TRUE,
    colReorder = list(
      enable = TRUE,
      realtime = FALSE,
      fixedColumnsLeft = 1
    )
  )
  fixed_opts <- list(
    initComplete = DT::JS(
      "function() {",
      paste0(
        "$(this.api().table().container()).find('.header').html(", 
        htmltools::htmlEscape(deparse(title %||% "")), 
        ")"
        ),
      "}"
      ),
    dom = gsub(pattern = "(t)", replacement = '<"header h5">\\1', dom)
  )
  
  # This will conditionally add a download button to the table
  if(nrow(data) > 0 & isTRUE(allow_listing_download)) {
    export_label <- export_label %||% "_label.missing_"
    extensions <- c("Buttons", extensions)
    fixed_opts[["buttons"]] <- list(list(
      extend = 'excel',
      text = '<i class="fa-solid fa-download"></i>',
      filename = paste("clinsight", export_label, sep = "."),
      title = paste0(export_label, " | extracted from ClinSight")
    ))
    fixed_opts[["dom"]] <- paste0('B', fixed_opts[["dom"]])
  }
  
  opts <- default_opts |>
    modifyList(options) |>
    modifyList(fixed_opts)
  
  DT::datatable(
    data, 
    selection = selection,
    options = opts,
    extensions = extensions,
    colnames = colnames,
    ...
  ) 
}
