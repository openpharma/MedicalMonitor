#' Application Metadata
#'
#' A list of data frames containing metadata that will be used for the
#' application. The metadata will be merged with with raw data. It controls the
#' variables that will be used in the application, and in which tab the
#' variables will be shown. The goal is that most, if not all, study-specific
#' data will be captured in the metadata, leaving the scripts to run the
#' application largely unaltered between studies.
#'
#' @format ## `metadata` A list with the following data frames:
#'  - `events`: Contains the columns `event_number`, `event_name`, and
#'   `event_label`. Used to create a simple timeline in the application, with
#'   predefined number of planned visits.
#'  - `common_forms`: Contains the columns `var`, `suffix`, `item_name` ,
#'   `item_type`, `item_group`. Used to select and rename the variables of
#'   interest in the common forms.
#'  - `study_forms`: Contains the same columns as the data frame `common_forms`,
#'   and in addition the columns `unit`, `lower_limit`, `uppter_limit`. Used to
#'   select and rename the variables of interest in the common forms.
#'  - `general`: Contains the same columns as `common_forms`.
#'  - `form_level_data`: For specifying form-level data. When reading in metadata
#'   from the Excel format, this table will created and sanitized if needed,
#'   creating the minimum columns  `item_group`, `item_scale`,
#'   `use_unscaled_limits`, and `review_required`. The column `item_group`
#'   contains the name of the forms to which the settings apply. Columns
#'   `item_scale` and `use_unscaled_limits` are used in
#'   [mod_study_forms_server()] to specify the scaling of the figures. It is
#'   recommended to set the required values here; by default, they are both set
#'   to FALSE in [mod_study_forms_server()]. The last column that is expected
#'   here is `review_required`, which can be used to specify whether review is
#'   required for a form. Will default to TRUE for each form it is unset for a
#'   form.
#'
#' @source Can be created with an Excel file. The Excel file format is chosen so
#'   that the metadata can be changed easily per study. See
#'   `raw-data/metadata.R` for details.
#' 
"metadata"



#' Custom color palette 
#' 
#' A named vector of colors to be used in the figures of the application.
#'
#' @format ## `col_palette`
#' A named vector with the following components:
#' - `within limits` : "#1a9641"
#' - `out of limits, clinically insignificant`: "#a6d96a"
#' - `out of limits, significance unknown`: "#fdae61"
#' - `out of limits, clinically significant`: "#d7191c"
#' - `limits unknown`: "grey50"
#' 
"col_palette"


#' Clinical Trial test data
#'
#' A data.frame containing randomly created clinical trial data. Used for
#' testing purposes. It will also be used to run the app with example data when
#' executing `run_app()` with the default configuration settings, or when
#' running `golem::run_dev()`.
#'
#' @format a data.frame with 6,483 rows and 24 variables.
#'
#' @source Created with `data-raw/create_random_data.R`
"clinsightful_data"




