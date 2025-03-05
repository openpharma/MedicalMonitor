#' Merge a pair of items
#'
#' @param data A data frame.
#' @param item_name A character string with the name of the item to merge.
#' @param item_name_other A character string with the name of the item to merge with.
#' @param merge_action A character string with the action to take when merging.
#' @param name_column A character string with the name of the column containing the item names.
#' @param value_column A character string with the name of the column containing the item values.
#' @param id_cols A character vector with the names of the columns that uniquely identify a row.
#'
#' @return A data frame with the merged items.
#' @keywords internal
#'
merge_item_pair <- function(
    data, 
    item_name,
    item_name_other,
    merge_action = c("combine", "replace"),
    name_column = "item_name", 
    value_column = "item_value",
    id_cols = idx_cols
){
  stopifnot(
    is.data.frame(data),
    is.character(item_name), 
    is.character(item_name_other),
    is.character(merge_action), 
    is.character(name_column), 
    is.character(value_column),
    is.character(id_cols),
    "item_name cannot be missing" = !is.na(item_name),
    "item_name_other cannot be missing" = !is.na(item_name_other),
    name_column %in% names(data),
    value_column %in% names(data),
    all(id_cols %in% names(data))
  )
  merge_action <- match.arg(merge_action)
  if (item_name == item_name_other){
    stop("item_name cannot be the same as item_name_other")
  }
  if (any(duplicated(data[c(id_cols, name_column)]))){
    warning(
      "id_cols (", paste0(id_cols, collapse = ", "), ") and name_column (", 
      name_column, ") do not uniquely identify the rows. ",
      "Cannot merge '", item_name, "' with '", item_name_other, "'."
    )
    return(data)
  }
  selected_data <- data[
    data[[name_column]] %in% c(item_name, item_name_other), , drop = FALSE
  ]   
  if (!item_name_other %in% data[[name_column]]){
    cat("No item pair for merging found for '", item_name, "'.\n", sep = "")
    return(data)
  }
  cat("merging item '", item_name, "' with its matched pair\n", sep = "")
  if ("edit_date_time" %in% names(data)){
    # Since we merge item_name and item_name_other that can have a different edit_date_time:
    selected_data <- selected_data |> 
      add_missing_columns("edit_date_time") |> 
      dplyr::mutate(
        edit_date_time = max(edit_date_time, na.rm = TRUE), 
        .by = dplyr::all_of(id_cols)
      )
  }
  selected_data <- selected_data |> 
    tidyr::pivot_wider(
      names_from = dplyr::all_of(name_column), 
      values_from = dplyr::all_of(value_column), 
      # Just a safeguard if columns are not uniquely identified:
      values_fn = ~ifelse(all(is.na(.)), NA_character_, paste0(na.omit(.), collapse = "; "))
    ) |> 
    add_missing_columns(c(item_name, item_name_other)) |> 
    dplyr::mutate(
      "{name_column}"  := item_name,
      # Either item_name or item_name_other should now be non-missing
      "{item_name}" := ifelse(is.na(.data[[item_name]]), "", .data[[item_name]]),
      "{value_column}" := dplyr::case_when(
        is.na(.data[[item_name_other]]) ~ .data[[item_name]],
        .data[[item_name]] == "" ~ .data[[item_name_other]],
        merge_action == "combine" ~ paste0(.data[[item_name]], " (", .data[[item_name_other]], ")"),
        merge_action == "replace" ~ .data[[item_name_other]],
        .default = NA_character_
      )
    ) |> 
    dplyr::select(-dplyr::all_of(c(item_name, item_name_other)))
  # 'other' column not needed anymore and would cause duplicates
  data[!data[[name_column]] == item_name_other, , drop = FALSE] |>
    # also add item_names if only the 'other' column was available:
    dplyr::rows_upsert(selected_data, by = c(id_cols, name_column))
}


#' Merge items by suffix
#' 
#' Merges items with a suffix in their name with the item without the suffix.
#' 
#' @param data A data frame.
#' @param suffix A character string with the suffix to merge. 
#' @param ... Additional arguments passed to [merge_item_pair].
#'
#' @return A data frame with the merged items.
#' @keywords internal
#'
merge_item_pairs_by_suffix <- function(
    data, 
    suffix = "_ITEM_TO_MERGE_WITH_PAIR",
    ...
){
  suffix <- suffix %||% ""
  stopifnot(is.data.frame(data), is.character(suffix))
  if (is.na(suffix) || suffix == ""){
    cat("No suffix defined. Skipping merging of items.\n")
    return(data)
  }
  cat("Using suffix '", suffix, "' to detect item pairs\n", sep = "")
  other_vars <- unique(data$item_name[grepl(suffix, data$item_name)])
  vars_to_merge <- data.frame(
    item_name = gsub(pattern = suffix, replacement = "", other_vars),
    item_name_other = other_vars
  )
  if (nrow(vars_to_merge) == 0){
    cat("No variable pairs found for merging. Returning data.\n", sep = "")
    return(data)
  }
  cat(
    "Verifying if following items need merging with their defined pair: \n  ", 
    paste0(vars_to_merge$item_name, collapse = ", "), "\n", 
    sep = ""
  )
  output <- data
  for (i in 1:nrow(vars_to_merge)){
    output <- merge_item_pair(
      data = output, 
      item_name = vars_to_merge[["item_name"]][i],
      item_name_other = vars_to_merge[["item_name_other"]][i],
      ...
    )
  }
  output
}

#' Clean merge item pair.
#'
#' Called in get_metadata to add additional rows with merge item pairs. This is
#' needed so that all these 'other' columns are preserved when merging data with
#' metadata.
#'
#' @param data A data frame with metadata
#' @param merge_col A character vector with the column name in which the
#'   variable to merge with is defined.
#' @param suffix_to_add A Character vector with the suffix to add to the
#'   item_name. This is used to detect item pairs and merge them when in
#'   [merge_meta_with_data()].
#'
#' @keywords internal
#' 
clean_merge_pair_metadata <- function(
    data, 
    merge_col = "merge_with",
    suffix_to_add = "_ITEM_TO_MERGE_WITH_PAIR"
){
  stopifnot(is.data.frame(data))
  stopifnot(c("var", "item_name") %in% names(data))
  stopifnot(is.character(merge_col), is.character(suffix_to_add))
  if (merge_col %in% names(data) && !all(is.na(data[[merge_col]]))){
    additional_rows <- data[!is.na(data[[merge_col]]),] 
    if (any(additional_rows[[merge_col]] == additional_rows[["var"]])){
      stop("variables in column 'var' cannot be the same as in ", merge_col)
    }
    additional_rows$var <- additional_rows$merge_with
    additional_rows$item_name <- paste0(
      additional_rows$item_name, 
      suffix_to_add
    )
    data <- rbind(data, additional_rows)
    data$merge_with <- NULL
  }
  data
}
