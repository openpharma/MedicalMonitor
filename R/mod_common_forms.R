#' Common forms - Shiny module UI
#'
#' @inherit mod_common_forms_server
#' @seealso [mod_common_forms_server()]
#' 
mod_common_forms_ui <- function(id, form){
  ns <- NS(id)
  bslib::nav_panel(
    title = form, 
    if (form == "Adverse events") mod_timeline_ui(ns("timeline_fig")),
    bslib::layout_sidebar(
      fillable = FALSE,
      if(form == "Adverse events"){
        mod_review_form_tbl_ui(ns("review_form_SAE_tbl"))
      },
      mod_review_form_tbl_ui(ns("review_form_tbl")),
      sidebar = bslib::sidebar(
        bg = "white", 
        position = "right",
        shinyWidgets::materialSwitch(
          inputId = ns("show_all_data"),
          label = "Show all participants", 
          status = "primary",
          right = TRUE
        ),
        bslib::card_body(
          HTML("<b>Bold*:</b> New/updated data"), 
          fillable = FALSE
          )
      )
    )
  )
}

#' Common forms - Shiny module Server
#'
#' Shiny module. Used to display common form data in the dedicated tab.
#'
#' The module displays tables of common form data in a wide format. Included
#' common forms are currently: `Adverse events`, `Medical History`,
#' `Medication`, and `Conc. Procedures`. The applicable forms can be flexibly
#' changed in the metadata. The tables shown are overview tables in wide format,
#' similar to the ones in [mod_study_forms_server()]. When the common form
#' `Adverse events` is selected, the module will show an additional table with
#' Severe Adverse Events above the table with Adverse Events. In addition, it
#' will show a timeline by calling module
#' [mod_timeline_ui()]/[mod_timeline_server()]. The timeline shows study events
#' (such as drug administrations) and study visits together with Adverse Events,
#' so that temporal relationships between these events can be quickly revealed.
#' The `common forms` module is used in the main server to create all applicable
#' common form pages.
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param form A character string with the name of the form to display.
#' @param form_data A reactive value containing the study data of the respective
#'   form.
#' @param form_review_data A reactive value containing the review data of the
#'   respective form.
#' @param form_items A named character vector with the names of the expected
#'   variables defined in the applicable `form`. Used in the UI to create a
#'   filter with drop-down menu, to select the desired variables in a figure.
#'   Used in the module Server to make sure that all expected columns are always
#'   created, even if some variables are implicitly missing (which might occur
#'   if there are not yet any values available for a specific variable). Also,
#'   implicitly missing variables might give errors if part of the script relies
#'   on the variables' presence. See also the parameter `expected_columns` in
#'   [create_table.default()].
#' @param active_subject A reactive value containing the active subject ID.
#' @param id_item Character vector containing the column names of the columns
#'   that can uniquely identify one item/row.
#' @param table_names An optional character vector. If provided, will be used
#'   within [datatable_custom()], to improve the column names in the final
#'   interactive tables.
#' @param timeline_data A reactive with a data frame containing the timeline
#'   data. Used to create the timeline figure. Created with
#'   [get_timeline_data()].
#'
#'
#' @seealso [mod_common_forms_ui()], [mod_timeline_ui()],
#'   [mod_timeline_server()], [mod_review_form_tbl_ui()],
#'   [mod_review_form_tbl_server()]
#' 
mod_common_forms_server <- function(
    id, 
    form,
    form_data,
    form_review_data,
    form_items,
    active_subject,
    id_item = c("subject_id", "event_name", "item_group", 
                "form_repeat", "item_name"),
    table_names = NULL,
    timeline_data
){
  stopifnot(is.character(form), length(form) == 1)
  stopifnot(is.reactive(form_data), is.reactive(form_review_data))
  stopifnot(is.character(form_items))
  stopifnot(is.reactive(active_subject))
  stopifnot(is.character(id_item))
  stopifnot(is.null(table_names) || is.character(table_names))
  stopifnot(is.reactive(timeline_data))
  names(form_items) <- names(form_items) %||% form_items
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
     
    mod_review_form_tbl_server(
      "review_form_tbl", 
      form = form,
      form_data = form_data, 
      form_review_data = form_review_data, 
      form_items = form_items,
      active_subject = active_subject,
      show_all = reactive(input$show_all_data), 
      table_names = table_names, 
      title = form
    )
    
    if (form == "Adverse events") {
      mod_review_form_tbl_server(
        "review_form_SAE_tbl", 
        form = form,
        form_data = form_data, 
        form_review_data = form_review_data, 
        form_items = form_items,
        active_subject = active_subject,
        show_all = reactive(input$show_all_data), 
        table_names = table_names, 
        title = "Serious Adverse Events"
      )
      mod_timeline_server(
        "timeline_fig", 
        form_review_data = form_review_data,
        timeline_data = timeline_data,
        active_subject = active_subject
      ) 
    }
    
  })
}

## To be copied in the UI
# mod_common_forms_ui("common_forms_1")

## To be copied in the server
# mod_common_forms_server("common_forms_1")
