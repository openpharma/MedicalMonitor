#' Review form - Shiny module UI
#'
#'
#' @inherit mod_review_forms_server 
#' @seealso [mod_review_forms_server()]
#' 
mod_review_forms_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::tabsetPanel(
      id = ns("confirm_review_tabs"),
      type = "hidden",
      tabPanel("empty_panel", ""),
      tabPanel(
        "show_checkbox",
        bslib::layout_columns(
          col_widths = c(7, 5),
          bslib::tooltip(
            checkboxInput(
              inputId = ns("form_reviewed"),
              label = "Reviewed",
              value = FALSE 
            ) |> 
              shiny::tagAppendAttributes(class = "cs_checkbox", .cssSelector = "input"),
            "Mark as reviewed",
            placement = "bottom"
          ),
          bslib::tooltip(
            shinyWidgets::prettySwitch(
              inputId = ns("add_comment"), 
              label = NULL,  
              status = "primary",
              slim = TRUE,
              value = FALSE
            ), 
            "Add a comment",
            placement = "right", options = list(`offset` = "0,-25")
          )
        ),
        conditionalPanel(
          condition = "input.add_comment === true",
          ns = NS(id),
          shiny::textAreaInput(
            inputId = ns("review_comment"), 
            label = NULL
          )
        ),
        progress_bar(ns("progress_bar")),
        bslib::layout_columns(
          col_widths = c(11, 12),
          shiny::actionButton(
            inputId = ns("save_review"), 
            "Save", 
            icon = icon("floppy-disk"),
            class = "btn-primary m2"
          ),
          textOutput(ns("save_review_error"))
        )
      )
    )
  )
}

#' Review form - Shiny module Server
#'
#' @description A `shiny` module. Used to mark data as reviewed and add comments
#'   to data of a specific form/page in the database and in the internal
#'   application data.
#'
#'   This module is used to display whether the form that is actively being
#'   displayed in the application has any values that are newly entered or
#'   updated since the last review session. In addition, if a page contains new
#'   data, the user can mark all data in the form as being reviewed and can
#'   (optionally) add a comment to this review action. The data will be saved in
#'   a database. All review activity is stored with an audit-trail, with
#'   date/time stamps and with the reviewer's name.
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param r Common reactive values. Used to access subject_id, user_name,
#'   review_data, and status_data. The module can change data in the review_data
#'   and to status_data data frames.
#' @param active_form Reactive value. Used to determine which form is active and
#'   whether that form needs review.
#' @param active_tab Reactive value containing the active tab. Needed to hide
#'   the review controls if non-relevant tabs are selected (tabs that do not
#'   contain common forms or study forms).
#' @param review_required_data a data frame with information about whether
#'   review is mandatory for each form. Should contain the columns `item_group`
#'   and `review_required`.
#' @param db_path Character string with the file path to the database.
#'
#' @seealso [mod_review_forms_ui()]
#' 
mod_review_forms_server <- function(
    id, 
    r, 
    active_form, 
    active_tab, 
    review_required_data,
    db_path
){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.reactive(active_form))
  stopifnot(is.reactive(active_tab))
  stopifnot(is.data.frame(review_required_data))
  if(!all(c("item_group", "review_required") %in% names(review_required_data))){
    stop("Either the 'study_forms or 'review_required' column is missing ", 
         "from the review_required data frame")
  }
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    review_data_active <- reactive({
      with(r$review_data, r$review_data[
        subject_id == r$subject_id & item_group == active_form(),
        ])
    })
    
    review_indeterminate <- reactiveVal()
    
    observeEvent(review_indeterminate(), {
      shinyjs::runjs(sprintf("$('#%s').prop('indeterminate', %s)", ns("form_reviewed"), tolower(review_indeterminate())))
    })
    
    observe({
      req(session$userData$review_records[[active_form()]])
      review_status <-
        review_data_active()[,c("id", "reviewed")] |> 
        dplyr::rows_update(session$userData$review_records[[active_form()]][,c("id", "reviewed")], by = "id") |> 
        dplyr::distinct(reviewed) |> 
        dplyr::pull()
      
      shinyjs::runjs(sprintf("$('#%s').prop('checked', %s)", ns("form_reviewed"), tolower(identical(review_status, "Yes"))))
      review_indeterminate(length(review_status) > 1)
    }) |>
      bindEvent(active_form(), session$userData$review_records[[active_form()]])
    
    observeEvent(r$subject_id, {
      golem::cat_dev("mod_review_forms | Reset review records\n")
      session$userData$update_checkboxes[[active_form()]] <- NULL
      session$userData$review_records[[active_form()]] <- data.frame(id = integer(), reviewed = character())
    })
    
    observeEvent(input$form_reviewed, {
      session$userData$update_checkboxes[[active_form()]] <- input$form_reviewed
      
      session$userData$review_records[[active_form()]] <-
        review_data_active() |> 
        dplyr::mutate(reviewed = ifelse(input$form_reviewed, "Yes", "No")) |> 
        dplyr::select(id, reviewed) |> 
        dplyr::anti_join(
          subset(r$review_data, item_group == active_form()),
          by = c("id", "reviewed")
        ) |> 
        dplyr::arrange(id)
    }, ignoreInit = TRUE)    
    
    observeEvent(c(active_form(), r$subject_id), {
      cat("Update confirm review button\n\n\n")
      req(r$review_data)
      review_indeterminate(FALSE)
      if(nrow(review_data_active()) == 0){ 
        cat("No review data found for Subject id: ", r$subject_id, 
            " and group: ", active_form(), "\n") 
        review_status <- "No"
        review_comment <- ""
      } else {
        # in the rare case of multiple rows selected, the unique reviewed label 
        # will be selected below. if there are multiple labels ("Yes and No"), 
        # it will give a warning. This would be rare since it would mean a datapoint with the same edit date-time was reviewed but another one was not. 
        # probably better to use defensive coding here to ensure the app does not crash in that case. However we need to define which review status we need to select
        # in this case get the reviewed = "No"
        review_status <- unique(review_data_active()[["reviewed"]])
        review_comment <- with(review_data_active(), comment[edit_date_time == max(as.POSIXct(edit_date_time))]) |> unique() |> paste(collapse = "; ")
        if(length(review_status) != 1)
          review_indeterminate(TRUE)
      }
      
      shinyjs::runjs(sprintf("$('#%s').prop('checked', %s)", ns("form_reviewed"), tolower(identical(review_status, "Yes"))))
      shinyjs::runjs(sprintf("$('#%s').prop('indeterminate', %s)", ns("form_reviewed"), tolower(review_indeterminate())))
      
      shinyWidgets::updatePrettySwitch(
        session = session,
        inputId = "add_comment",
        value = !identical(review_comment, "")
      )
      updateTextAreaInput(
        inputId = "review_comment",
        value = review_comment
      )
    })
    
    user_allowed_to_review <- reactive({
      isFALSE(is.null(r$user_name) || r$user_name == "")
    })
    
    role_allowed_to_review <- reactive({
      get_roles_from_config()[r$user_role] %in% get_golem_config("allow_to_review")
    })
    
    review_required <- reactive({
      req(active_form(), review_required_data)
      with(
        review_required_data, 
        review_required[item_group == active_form()]
      ) %||% TRUE
    })
    
    enable_any_review <- reactive({
      all(c(
        review_required(),
        user_allowed_to_review(),
        role_allowed_to_review(),
        nrow(review_data_active()) != 0
      ))
    })
    
    enable_save_review <- reactive({
      req(
        active_form(),
        session$userData$review_records[[active_form()]], 
        is.logical(enable_any_review())
        ) 
      if(!enable_any_review()) return(FALSE)
      nrow(session$userData$review_records[[active_form()]]) != 0
    })
    
    observeEvent(c(enable_any_review(), enable_save_review()), {
      req(is.logical(enable_any_review()), is.logical(enable_save_review()))
      shinyjs::toggleState("form_reviewed", enable_any_review())
      if(enable_save_review()){
        shinyjs::enable("save_review")
        shinyjs::enable("add_comment")
        shinyjs::enable("review_comment")
      } else {
        shinyjs::disable("save_review")
        shinyjs::disable("add_comment")
        shinyjs::disable("review_comment")
      }
    })
    
    ## only show review panel with common events and study data tabs:
    observeEvent(active_tab(), {
      check_box_tab <- "empty_panel"
      cat("main tabs: ", active_tab(), "\n\n")
      if(active_tab() %in% c("Common events", "Study data")){
        check_box_tab <- "show_checkbox"
      } 
      shiny::updateTabsetPanel(
        session = session,
        inputId = "confirm_review_tabs",
        selected = check_box_tab
      )
    })
    
    review_save_error <- reactiveVal(FALSE)
    observeEvent(input$save_review, {
      req(review_data_active())
      req(enable_save_review())
      review_save_error(FALSE)
      # golem::cat_dev("Save review status reviewed:", input$form_reviewed, "\n")
      
      review_records <- session$userData$review_records[[active_form()]][c("id", "reviewed")] |> 
        dplyr::mutate(
          comment     = ifelse(is.null(input$review_comment), "", input$review_comment),
          reviewer    = paste0(r$user_name, " (", r$user_role, ")"),
          timestamp   = time_stamp(),
          status      = ifelse(reviewed == "Yes", "old", "new")
        ) 
      
      golem::cat_dev(
        active_form(), "|", "Adjusting review status for", 
        length(unique(review_records$id)), "ids\n" 
      )
      
      cat("Writing review progress to database\n")
      db_save_review(
        review_records, 
        db_path = db_path,
        table = "all_review_data" 
      )
      
      review_records_db <- db_get_review(
        db_path, id = review_records$id
      )[, names(review_records)] 
      
      if (isTRUE(all.equal(review_records_db, review_records, check.attributes = FALSE))){
        cat("Update review data and status in app\n")
        r$review_data <- r$review_data |> 
          dplyr::rows_update(review_records, by = "id")
      }
      
      updated_records_memory <- r$review_data[
        r$review_data$id %in% review_records$id,
        names(review_records_db)
      ]
      
      session$userData$update_checkboxes[[active_form()]] <- NULL
      session$userData$review_records[[active_form()]] <- data.frame(id = integer(), reviewed = character())
      
      review_save_error(any(
        !isTRUE(all.equal(review_records_db, review_records, check.attributes = FALSE)),
        !isTRUE(all.equal(updated_records_memory, review_records_db, check.attributes = FALSE))
      ))
      
      if(review_save_error()){
        return({
          showNotification(
            paste0("An error occurred while trying to save the data. ",
                   "Try again or contact an administrator if the ",
                   "problem persists. ", 
                   "Resynching in-memory data with database.."), 
            duration = 5, 
            id = ns("review_save_error"),
            type = "error"
          )
          r$review_data <- db_get_table(db_path, db_table = "all_review_data")
        })
      }
      showNotification("Input saved successfully", duration = 1, type = "message") 
    })
    
    output[["progress_bar"]] <- render_progress_bar({
      req(
        review_data_active(),
        active_form()
        )
      
      list(
        completed = sum(review_data_active()$reviewed == "Yes"),
        unmarking = sum(session$userData$review_records[[active_form()]]$reviewed == "No"),
        marking = sum(session$userData$review_records[[active_form()]]$reviewed == "Yes"),
        total = nrow(review_data_active())
      )
    })
    
    output[["review_header"]] <- renderText({active_form()})
    
    output[["save_review_error"]] <- renderPrint({
      validate(need(
        role_allowed_to_review(), 
        paste0("Review not allowed for a '", r$user_role, "'.")
        ))
      validate(need(
        review_required(), 
        "Review not required"
      ))
      validate(need(
        nrow(review_data_active()) != 0,
        "Nothing to review"
      ))
      validate(need(
        user_allowed_to_review(), 
        "No user name found. Cannot save review"
      ))
      validate(need(
        any(review_data_active()[["reviewed"]] != "Yes"),
        "Form already reviewed"
      ))
      validate(need(input$form_reviewed, "Requires review"))
    })
    
    shiny::exportTestValues(
      review_save_error = review_save_error()
    )
  })
}

## To be copied in the UI
# mod_review_forms_ui("review_controls_1")

## To be copied in the server
# mod_review_forms_server("review_controls_1", r)
