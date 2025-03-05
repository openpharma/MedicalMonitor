#' Write Query - Shiny module UI
#'
#'
#' @inherit mod_query_add_server
#' @seealso [mod_query_add_server()], [mod_queries_ui()], [mod_queries_server()]
#' 
mod_query_add_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::actionButton(
      inputId = ns("create_query"),
      label = "Create query", 
      icon = icon("file-lines"),
      class = "btn-warning m2"
    )
  )
}

#' Write Query - Shiny module Server
#'
#' Shiny module. Used to create a new query and add it to the database. This
#' module can be activated by pressing an action button, which is currently
#' located in the applications' main sidebar ([mod_main_sidebar_server()]).
#'
#' Upon activation of this module, a window will be shown with a fixed sidebar
#' and a field with the main content. The main content contains a field in which
#' a query can be written, contains  a `Save` and `Cancel` button, and shows the
#' name of the reviewer who is currently logged in. In the sidebar, information
#' is given about the currently active patient and form. In addition, drop-down
#' menus are shown to narrow down to which data the query is concerning. In
#' these menus, the applicable visit, and (optional) the applicable item within
#' the form can be selected.
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param r Common `reactivevalues`. Used to access the active `subject_id` and
#'   the current reviewer's name `user_name`.
#' @param active_form A reactive value, containing the active form.
#' @param db_path Character string with the path to the database.
#' @param available_data A data frame containing all available data, usually
#'   created with the function [get_available_data()].
#'
#' @seealso [mod_query_add_ui()], [mod_queries_ui()], [mod_queries_server()]
#' 
mod_query_add_server <- function(
    id, 
    r, 
    active_form, 
    db_path, 
    available_data
){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.reactive(active_form))
  stopifnot(is.character(db_path))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    modal_add_query <- function(sel_data){
      modalDialog(
        title = HTML(paste0("<b>", unique(sel_data$subject_id), "</b>")),
        size = "l",
        fade = FALSE,
        footer = bslib::layout_columns(
          col_widths = c(6,6), 
          shiny::actionButton(
            inputId = ns("query_add_input"),
            label = "Add query",
            class = "btn-warning m2"
          ),
          modalButton("Cancel")
        ),
        bslib::card(
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              id = ns("query_sidebar"),
              open = "always",
              HTML(paste0("<b>", unique(sel_data$item_group), "</b>")),
              shiny::selectizeInput(
                inputId = ns("query_select_visit"),
                label = NULL,
                choices = unique(sel_data$event_name),
                options = list(
                  placeholder = 'Select a visit',
                  onInitialize = if(length(unique(sel_data$event_name)) == 1) NULL else {
                      I('function() { this.setValue(""); }')
                  }
                )
              ),
              shiny::selectizeInput(
                inputId = ns("query_select_item"),
                label = NULL,
                choices = unique(sel_data$item_name), 
                options = list(
                  placeholder = 'Select item (optional)',
                  onInitialize = I('function() { this.setValue(""); }')
                )
              )
            ),
            bslib::card_body(
              shiny::textAreaInput(
                inputId = ns("query_text"),
                label = NULL,
                height = "250px",
                width = "100%",
                placeholder = "add query text here"
              ),
              bslib::card_body(
                shinyWidgets::materialSwitch(
                  inputId = ns("query_major"),
                  label = "Major query", 
                  status = "danger",
                  inline = TRUE, 
                  right = TRUE
                ),
                bslib::popover(
                  icon("circle-info"),
                  title = "Major queries",
                  id = ns("query_major_info"),
                  markdown("Only use for issues that could have a major impact 
                  on either the patient safety or the study outcomes.")
                ), 
                class = "d-flex flex-row", 
                fillable = FALSE,
                gap = 0
              ),
              verbatimTextOutput(ns("reviewer"))
            ),
            verbatimTextOutput(ns("query_error"))
          ),
          min_height = "500px"
        ),
        easyClose = TRUE
      )
    }
    
    selected_data <- reactiveVal()
    observeEvent(input$create_query, {
      req(available_data, r$subject_id, active_form())
      df <- with(available_data, available_data[
        subject_id == r$subject_id & item_group == active_form(), 
      ])
      if(nrow(df) == 0){
        df <- dplyr::tibble(
          subject_id = r$subject_id,
          event_name = "Any visit", 
          item_group = active_form(), 
          item_name = NA_character_
        )
      }
      selected_data(df)
      shiny::showModal(modal_add_query(sel_data = selected_data()))
    })
    
    query_save_error <- reactiveVal(FALSE)
    observeEvent(input$query_add_input, {
      req(input$query_select_visit, input$query_text, r$user_name, r$user_role)
      query_save_error(FALSE)
      golem::cat_dev("Query text to add: ", input$query_text, "\n")
      new_query <- dplyr::tibble(
        "query_id"      = paste0(r$subject_id, create_unique_id(5)), 
        "type"          = ifelse(input$query_major, "Major", "Normal"),
        "subject_id"    = r$subject_id,
        "event_label"   = input$query_select_visit, 
        "item_group"    = active_form(), 
        "item"          = input$query_select_item, 
        "timestamp"     = time_stamp(),
        "n"             = 1,
        "reviewer"      = paste0(r$user_name," (", r$user_role, ")"),
        "query"         = input$query_text,
        "resolved"      = "No",
        "resolved_date" = NA_character_,
        "edit_reason"   = NA_character_
      ) 
      golem::print_dev(new_query)
      
      db_save(data = new_query, db_path = db_path, db_table = "query_data")
      query_in_db <- db_get_query(
        db_path, query_id = new_query$query_id, n = new_query$n
      )
      query_in_db <- unique(query_in_db[names(new_query)])
      query_in_db <- query_in_db[query_in_db$timestamp == new_query$timestamp[1], ]
      
      if(identical(new_query, query_in_db)){
        r$query_data <- dplyr::bind_rows(r$query_data, new_query)
      }
      query_in_memory <- r$query_data[nrow(r$query_data), -1]
      query_save_error(any(
        !identical(new_query, query_in_db), 
        !identical(query_in_db, query_in_memory)
      ))
      
      if(query_save_error()){
        return({
          showNotification(
            paste0("An error occurred while trying to save the data.",
                   "Try again or contact an administrator if the ",
                   "problem persists. ", 
                   "Resynching in-memory data with database.."), 
            id = ns("query_save_error"), 
            type = "error",  
            duration = 5
          )
          r$query_data <- collect_query_data(db_path)
        })
      }
      # clear query field input:
      updateTextInput(inputId = "query_text", value = "")
      # add user feedback that query is added to database:
      showModal(
        modalDialog(
          title = "Query successfully added",
          fade = FALSE, 
          easyClose = TRUE,
          HTML(paste0(
            "Subject: ", htmlEscape(query_in_db$subject_id), "<br>",
            "Event: ", htmlEscape(query_in_db$event_label), "<br>",
            "Form: ", htmlEscape(query_in_db$item_group), "<br>",
            "Item: ", htmlEscape(query_in_db$item), "<br>",
            "Query: ", htmlEscape(query_in_db$event_label), "<br>",
            "Author: ", htmlEscape(query_in_db$reviewer)
          )),
          footer = modalButton("Close")
        )
      )
    })
    
    output[["query_error"]] <- renderPrint({
      req(input$query_add_input)
      validate(
        need(r$user_name, "User name missing. Cannot save query anonymously."),
        need(r$user_role, "User role missing. Cannot save query without user role."),
        need(input$query_select_visit, "Please select a visit"),
        need(input$query_text, "Please add a query message")
      )
    })
    
    output[["reviewer"]] <- renderPrint({
      req(r$user_name)
      cat("Author: ", r$user_name, " (", r$user_role, ")\n", sep = "")
    })
    
  })
}

## To be copied in the UI
# mod_write_queries_ui("write_queries_1")

## To be copied in the server
# mod_write_queries_server("write_queries_1")
