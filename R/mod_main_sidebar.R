#' App sidebar - Shiny module UI
#'
#' @inherit mod_main_sidebar_server
#' @seealso [mod_main_sidebar_ui()]
#' 
mod_main_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns("show_review_controls"),
      type = "hidden",
      tabPanel("panel_empty", ""),
      tabPanel(
        "panel_controls", 
        bslib::card( 
          bslib::card_header(mod_navigate_forms_ui(ns("navigate_forms_1"))),
          htmltools::HTML("<br><br>"),
          mod_review_forms_ui(ns("review_forms_1")),
          if (isTRUE(get_golem_config("allow_query_inputs"))) {
            tagList(
              htmltools::HTML("<hr><br>"),
              mod_query_add_ui(ns("write_query"))
            )
          }
        )
      )
    ),
    htmltools::HTML("<br><br><br><br>"),
    mod_review_config_ui(ns("review_config_1")),
    mod_db_synch_info_ui(ns("synch_info"))
  )
}

#' App sidebar - Shiny module Server
#'
#' A shiny module. Used to create the sidebar in the main application.
#'
#' This sidebar contains the main controls for the application. In the sidebar,
#' a panel is available which allows the user to apply a review to the active
#' form and patient (forms can be marked as reviewed, a comment can be added,
#' and the review can be saved. In addition, it contains buttons to navigate to
#' the next/previous forms and to add a query to the active form.
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param r Common reactivevalues.  Will be passed on to
#'   [mod_query_add_server()].
#' @param navinfo Reactive values created with [shiny::reactiveValues()]. Used
#'   to send back information about the page change to the server, when clicking
#'   on the adverse event box. Required for [mod_navigate_forms_server()].
#'   Furthermore, it contains the currently active form (`navinfo$active_form`),
#'   needed for [mod_review_forms_server()] and [mod_query_add_server()], and
#'   the currently active tab (`navinfo$active_tab`), needed for
#'   [mod_review_forms_server()]. These are reactive values containing a
#'   character vector with the currently active tab or form.
#'   `navinfo$active_tab` is also used to only show review controls when a the
#'   tab is either common events or study data. Other active tab values are, for
#'   example, `Start` (start page), or `Queries`.
#' @param app_data List of data frames. Contains the application data, with data
#'   of each form stored in a data frame. Required to set the review
#'   configuration in [mod_review_config_server()].
#' @param app_tables List of data frames with the app data in wide table format.
#'   Required to set the review configuration in [mod_review_config_server()]
#' @param app_vars A list with common variables found in the data and metadata.
#'   Required to set the review configuration in [mod_review_config_server()].
#' @param forms_to_review A reactive value containing a character vector with
#'   the forms that need review of the active participant.
#' @param db_path Character vector with the path to the app database. Required
#'   in [mod_query_add_server()].
#' @param available_data A data frame containing all available data, usually
#'   created with the function [get_available_data()]. The data frame will be
#'   passed on to the module [mod_query_add_server()], which requires this data
#'   frame and is embedded in `mod_main_sidebar_server()`.
#'
#' @seealso [mod_main_sidebar_ui()], [mod_query_add_server()]
#' 
mod_main_sidebar_server <- function(
    id, 
    r, 
    navinfo,
    app_data,
    app_tables,
    app_vars,
    db_path, 
    forms_to_review,
    available_data
){
  stopifnot(is.reactivevalues(r))
  
  stopifnot(is.reactivevalues(navinfo))
  stopifnot(is.character(db_path))
  stopifnot(is.data.frame(available_data))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(navinfo$active_tab, {
      req(navinfo$active_tab)
      check_box_tab <- ifelse(navinfo$active_tab %in% c("Common events", "Study data"), 
                              "panel_controls", "panel_empty")
      shiny::updateTabsetPanel(
        session = session,
        inputId = "show_review_controls",
        selected = check_box_tab
      )
    })
    
    if (isTRUE(get_golem_config("allow_query_inputs"))) {
      mod_query_add_server(
        id = "write_query", 
        r = r, 
        active_form = reactive(navinfo$active_form), 
        db_path = db_path, 
        available_data = available_data
      )
    }
    
    mod_review_forms_server(
      id = "review_forms_1", 
      r = r, 
      review_required_data = app_vars$form_level_data[c("item_group", "review_required")],
      active_form = reactive(navinfo$active_form), 
      active_tab = reactive(navinfo$active_tab), 
      db_path = db_path
    )
    
    mod_navigate_forms_server(
      id = "navigate_forms_1", 
      navinfo = navinfo,
      forms_to_review = forms_to_review,
      all_forms = app_vars$all_forms
    )
    
    mod_review_config_server(
      "review_config_1", 
      r = r, 
      app_data = app_data,
      app_tables = app_tables, 
      sites = app_vars$Sites, 
      subject_ids = app_vars$subject_id
    )
    mod_db_synch_info_server(
      id = "synch_info",
      app_data = app_data,
      db_path = db_path
      )
  })
}

