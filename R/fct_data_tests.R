#' Check data
#'
#' Checks data for compatibility with metadata.
#'
#' @param data List of data frames with app data.
#' @param meta data frame with meta data.
#' @param required_cols Character vector, containing the column names that are
#'   required for the application to run appropriately.
#'
#' @export
#' 
check_appdata <- function(
    data = appdata,
    meta = metadata,
    required_cols = c("site_code", "subject_id", "event_date", "event_name", 
                      "item_group", "form_repeat", "item_name",    
                       "edit_date_time", "day")
){
  
  cat("Required column names:", required_cols, 
      "\nVerifying if all required columns are in the dataset:\n\n")
  missing_colnames <- lapply(data, \(x){
    required_cols[!required_cols %in% names(x)]
  })
  if(!all(sapply(missing_colnames, length) == 0)) warning(
    "Some required column names are missing."
  )
  lapply(1:length(missing_colnames), \(x){
    check <- if(length(missing_colnames[[x]]) == 0) "\U2713" else {
      paste0(
        "missing cols: ", 
        paste0(missing_colnames[[x]], collapse = ", ")
      )
    }
    cat(names(missing_colnames)[x], ":", check, "\n")
  }) |> 
    invisible()
  cat("\n")
  
  # all_items <- meta$items_expanded$var
  # missing_items <- all_items[!(all_items %in% raw_data$var)]
  
  all_vars <- meta$items_expanded$item_name
  data_vars <- unique(unlist(lapply(data, \(x){as.character(x[["item_name"]])})))
  missing_ids <- meta$items_expanded |> 
    dplyr::filter(item_name %in% all_vars[!(all_vars %in% data_vars) ]) |> 
    dplyr::select(var, item_name)
  if(nrow(missing_ids != 0)){ 
    cat("Variables missing in the data, but defined in the metadata:\n")
    warning("Not all variables defined in metadata are present in the data.")
    print.data.frame(missing_ids)
  }
  cat("data check completed.\n")
}

