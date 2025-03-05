#' Review forms table - Shiny module UI
#'
#' @inherit mod_review_form_tbl_server
#' @seealso [mod_review_form_tbl_server()]
#'
mod_review_form_tbl_ui <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("table"))
}

#' Review forms table - Shiny module Server
#'
#' Shiny module. Used to handle the logic and presentation of form review data.
#'
#' This module handles the logic associated with the form review tables. It
#' handles all the logic associated with the table check boxes and the observers
#' needed to keep the browser table in-synch with the server table.
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param form A character string with the name of the form to display.
#' @param form_data Common reactive value. Used to manage the server data
#'   displayed in the DataTable.
#' @param form_review_data Common reactive value containing the review data of
#'   the form.
#' @param form_items Named character vector with all form_items to display.
#' @param active_subject Reactive value containing the active subject id.
#' @param show_all Common reactive value, a logical indicating whether all
#'   records should be displayed.
#' @param table_names An optional character vector. If provided, will be used
#'   within [datatable_custom()], to improve the column names in the final
#'   interactive tables.
#' @param title An optional character vector. If provided, will be used within
#'   [datatable_custom()], as the title for the table.
#'   
#' @seealso [mod_review_form_tbl_ui()], [mod_common_forms_ui()],
#'   [mod_common_forms_server()], [mod_study_forms_ui()],
#'   [mod_study_forms_server()]
#' 
mod_review_form_tbl_server <- function(
    id,
    form,
    form_data,
    form_review_data,
    form_items,
    active_subject, 
    show_all,
    table_names = NULL,
    title = NULL
){
  stopifnot(is.reactive(form_data))
  stopifnot(is.reactive(show_all))

  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    reload_data <- reactiveVal(0)
    datatable_rendered <- reactiveVal(NULL)
    table_data <- reactiveVal()
    
    merged_form_data <- reactive({
      validate(need(
        form_data(),
        paste0("Warning: no data found in database for the form '", form, "'")
      ))
      golem::cat_dev(form, "| Computing merged data\n")
      get_form_table(
        form_data(),
        form_review_data(), 
        form = form, 
        form_items = form_items,
        active_subject = active_subject(),
        is_reviewed = NULL,
        is_SAE = identical(title, "Serious Adverse Events")
      )
    }) |> 
      bindEvent(form_data(), form_review_data(), active_subject())
    
    ############################### Observers: #################################
    
    
    observe({
      golem::cat_dev(form, "| Resetting userData\n")
      reload_data(reload_data() + 1)
      datatable_rendered(NULL)
      session$userData$update_checkboxes[[form]] <- NULL
      session$userData$review_records[[form]] <- data.frame(id = integer(), reviewed = character())
    }) |> 
      bindEvent(active_subject(), form_review_data(), form_data())
    
    observeEvent(datatable_rendered(), {
      table_data(merged_form_data())
    }, ignoreInit = TRUE)
    
    observeEvent(session$userData$update_checkboxes[[form]], {
      req(datatable_rendered())
      golem::cat_dev(form, "| Updating checkboxes\n")
      reload_data(reload_data() + 1)
      checked <- session$userData$update_checkboxes[[form]]
      df <- table_data() |> 
        dplyr::mutate(
          o_reviewed = dplyr::if_else(
            subject_id == active_subject(), 
            lapply(o_reviewed, modifyList, list(updated = checked)),
            o_reviewed
          )
        )
      table_data(df)
    })
      
    observeEvent(input$table_review_selection, {
      golem::cat_dev(form, "| table review selection changed to:\n")
      golem::print_dev(input$table_review_selection[c("id", "reviewed")])
      # Update review values for session's user data
      session$userData$update_checkboxes[[form]] <- NULL
      session$userData$review_records[[form]] <-
        update_review_records(
          session$userData$review_records[[form]],
          input$table_review_selection[, c("id", "reviewed")],
          subset(form_review_data(), subject_id == active_subject(),
                 c("id", "reviewed"))
        )
      
      # Update the table's data reactive
      df <- update_tbl_data_from_datatable(
        table_data(), 
        input$table_review_selection
      )
      table_data(df)
    })
    
    # Any time the data in the form table is updated, "show all" is toggled,
    # or the subject being viewed is changed, the server data for the datatable
    # needs to be updated
    observe({
      req(!is.null(show_all()))
      req(table_data(), datatable_rendered())
      golem::cat_dev(form, "| renewing datatable server data\n")
      DT::dataTableAjax(
        table_proxy$session, 
        subset(table_data(), show_all() | subject_id == active_subject()), 
        rownames = FALSE,
        outputId = table_proxy$rawId
      )
    }) 
    
    # Any time the review table is updated, "show all" is toggled, or the
    # subject being viewed is changed, the datatable should be reloaded to show
    # the new data
    observeEvent(reload_data(), {
      req(!is.null(show_all()))
      req(table_data(), datatable_rendered())
      golem::cat_dev(form, "| reload_data() triggered\n")
      DT::reloadData(table_proxy)
    }, ignoreInit = TRUE)
    
    observeEvent(show_all(), {
      req(table_data(), datatable_rendered())
      golem::cat_dev(
        form, "| show_all() trigger changed. Incrementing reload_data()",
        "and toggle showing subject_id column \n"
      )
      reload_data(reload_data() + 1)
      index <- match("subject_id", colnames(table_data())) - 1
      if (show_all()) {
        DT::showCols(table_proxy, index)
      } else {
        DT::hideCols(table_proxy, index)
      }
    })
    
    ############################### Outputs: ###################################
    
    output[["table"]] <- DT::renderDT({
      datatable_rendered(TRUE)
      datatable_custom(
        subset(merged_form_data(), isolate(show_all() | subject_id == active_subject())), 
        rename_vars = c("Review Status" = "o_reviewed", table_names), 
        rownames= FALSE,
        title = title,
        export_label = paste(
          ifelse(identical(title, "Serious Adverse Events"), "SAEs", simplify_string(form)), 
          ifelse(show_all(), "all_patients", active_subject()), 
          sep = "."
        ),
        escape = FALSE,
        selection = "none",
        callback = checkbox_callback,
        options = list(
          columnDefs = list(
            list(
              targets = "o_reviewed",
              orderable = FALSE,
              render = checkbox_render
            ),
            list(
              targets = "subject_id",
              visible = isolate(show_all())
            )),
          rowCallback = row_callback
        ))
    })
    table_proxy <- DT::dataTableProxy("table")
    
    if(form %in% c("Vital signs", "Vitals adjusted")){
      shiny::exportTestValues(
        table_data = tryCatch(
          subset(merged_form_data(), show_all() | subject_id == active_subject()),
          error = function(e) e
        )
      )
    } 
  })
}
    
## To be copied in the UI
# mod_review_form_tbl_ui("review_form_tbl_1")
    
## To be copied in the server
# mod_review_form_tbl_server("review_form_tbl_1")
