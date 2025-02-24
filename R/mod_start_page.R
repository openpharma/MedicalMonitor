#' Start page - Shiny module UI
#' 
#' @inherit mod_start_page_server 
#'
#' @seealso [mod_start_page_server()]
mod_start_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_body(
        HTML("<h3 style='text-align:center'>", 
             "Start review</h3>"),
        HTML("<b>Bold</b>: New/updated forms are available.<br>"),
        shinycssloaders::withSpinner(
        DT::DTOutput(ns("overview_table")), 
        type = 5
        ),
        fillable = FALSE
      ),
      bslib::card_body(
        mod_go_to_form_ui(ns("go_to_patient"), "Go to patient"),
        actionButton(ns("go_to_nav_review"), 
                     label = "Check forms to review",  
                     class = "btn-primary m2"
        ),
        fillable = FALSE
      )
    )
  )
}
    
#' Start page - Shiny module Server
#'
#' A Shiny module. Start page of the main application.
#'
#' The start page contains navigation buttons and a table that shows one row per
#' patient. The table shows general study information such as study status
#' (active or inactive participant), main diagnosis, age, sex, number of
#' conducted study visits, and the patient's data status (new data, updated
#' data, or old data). In addition, in the module there are two navigation
#' options. First, the user can navigate to the selected patient directly.
#' Secondly, the user can indirectly call the module
#' [mod_navigate_review_server()] to open a modal in which all the forms are
#' shown that need review of the selected patient. navigate to patient that is
#' currently highlighted. highlighted to a participant  study and options to
#' navigate directly to the selected forms/patients n the overview tables.
#' @param rev_data Common `reactiveValues()`. Used for navigation purposes, to
#'   set the subject for which we want to browse the review data from. 
#' @param table_names An optional character vector. If provided, will be used
#'   within [datatable_custom()], to improve the column names in the final
#'   interactive tables.
#' @inherit mod_go_to_form_server
#'
#' @seealso [mod_start_page_ui()], [mod_go_to_form_ui()],
#'   [mod_go_to_form_server()], [mod_navigate_review_server()],
#'   [mod_navigate_review_ui()]
#' 
mod_start_page_server <- function(id, r, rev_data, navinfo, all_forms, table_names){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.reactivevalues(navinfo))
  stopifnot(is.reactivevalues(rev_data))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    mod_go_to_form_server(
      "go_to_patient", 
      r = r, 
      navinfo = navinfo,
      navtable = reactive(rev_data$overview()), 
      tablerow = reactive(input$overview_table_rows_selected), 
      all_forms = all_forms,
      form_name = NULL
      )
    
    observeEvent(input$go_to_nav_review, {
      req(input$overview_table_rows_selected)
      cat("Browse from start_page to form in selected table row\n")
      selected_row <- rev_data$overview()[input$overview_table_rows_selected, ]
      rev_data$subject <- selected_row["subject_id"][[1]]
      rev_data$show_all <- FALSE
      rev_data$open_modal <- sum(c(rev_data$open_modal, 1), na.rm = TRUE)
    })
    
    output[["overview_table"]] <- DT::renderDataTable({
      bold_rows <- which(rev_data$overview()[["needs_review"]])
      tab <- datatable_custom(
        dplyr::select(rev_data$overview(), -needs_review), 
        rename_vars = table_names,
        callback = dblclick_to_form(ns("go_to_patient")), 
        allow_listing_download = FALSE
      )
      if(length(bold_rows) == 0) return(tab)
      DT::formatStyle(
        tab,
        0,
        target = "row",
        fontWeight = DT::styleEqual(bold_rows, "bold")
        )
    })
 
  })
}
    
## To be copied in the UI
# mod_start_page_ui("start_page_1")
    
## To be copied in the server
# mod_start_page_server("start_page_1")
