# This script creates small internal helper files that need to be available for 
# some functions in this package. 

# Color palette used in the shiny application for various figures.
col_palette <- c("#1a9641", "#a6d96a", "#fdae61", "#d7191c", "grey50")
names(col_palette) <- c("within limits", 
                        "out of limits, clinically insignificant", 
                        "out of limits, significance pending", 
                        "out of limits, clinically significant", 
                        "limits unknown")

# Empty data frame used to reset query data frame if needed.
query_data_skeleton <- dplyr::tibble(
  "query_id"      = character(),
  "type"         = character(),
  "subject_id"    = character(),
  "event_label"   = character(),
  "item_group"    = character(),
  "item"          = character(),
  "timestamp"     = character(),
  "n"             = numeric(),     
  "reviewer"      = character(),
  "query"         = character(),
  "resolved"      = character(),
  "resolved_date" = character(),
  "edit_reason"   = character()
)

# columns specifications required for the application to function properly. 
# Used to convert columns to the right format, and to test whether all the required 
# columns are available when merging raw data with metadata.
clinsight_col_specs <- c(
  "site_code" = "c", 
  "subject_id" = "c", 
  "event_id" = "c", 
  "event_date" = "D", 
  "event_repeat" = "i", 
  "form_id" = "c", 
  "form_repeat" = "i", 
  "var" = "c", 
  "item_value" = "c", 
  "edit_date_time" = "T",
  ".default" = "c"
  ) |> 
  readr::as.col_spec()

# Required columns needed in study data for the application to function.
required_col_names <- names(clinsight_col_specs$cols)

required_meta_cols <- c(
  "var", 
  "suffix", 
  "item_name", 
  "item_group", 
  "unit", 
  "lower_limit", 
  "upper_limit", 
  "item_type"
  )

# Since the columns required to define unique records are not user defined, they
# should be captured by an internal object to keep from having to simplify some
# functions.
idx_cols <- c(
  "subject_id",
  "event_name",
  "item_group",
  "form_repeat",
  "item_name"
)

db_version <- "1.1"

# Used in get_form_level_data(). Set a default if ClinSight needs the columns 
# to function properly. 
# Choosing NA for vars like item_scale, because setting these variables by 
# default for forms like 'Adverse events' does not make so much sense. 
form_level_defaults <- data.frame(
  "item_scale" = as.logical(NA),
  "use_unscaled_limits" = as.logical(NA), 
  "review_required" = TRUE
)

# Converting column types, and do not guess default: 
form_level_default_specs <- c(form_level_defaults, ".default" = "c") |> 
  sapply(class) |> 
  readr::as.col_spec()

usethis::use_data(col_palette, query_data_skeleton, required_col_names, 
                  required_meta_cols, clinsight_col_specs, 
                  idx_cols, db_version,
                  form_level_defaults, form_level_default_specs, 
                  overwrite = TRUE, internal = TRUE)
