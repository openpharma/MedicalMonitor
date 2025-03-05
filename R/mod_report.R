#' Create Report - Shiny module UI
#'
#'
#'
#' @inherit mod_report_server
#' @seealso [mod_report_server()]
#' 
mod_report_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(
      ns("create_report"),
      label = "Report",
      icon = icon("pen-to-square"),
      class = "btn-warning m2"
    )
  )
}

#' Create Report - Shiny module Server
#' 
#' Shiny module. Used to create PDF reports of all review activities.
#'
#' @param id Character string, used to connect the module UI with the module Server. 
#' @param r Common reactiveValues. Used to access .... 
#' @param rev_data Common reactiveValues. Used to access `rev_data$summary` (a data 
#' frame with summarized review data) and `rev_data$show_all` (logical, whether 
#' to show data of everyone in the tables or only data of the selected subject id.),
#' and `rev_data$show_modal` (to communicate that the review data modal should be opened). 
#' @param db_path Character string with the path to the app database.
#' @param table_names An optional character vector. If provided, will be used
#'   within [datatable_custom()], to improve the column names in the final
#'   interactive tables.
#' 
mod_report_server <- function(id, r, rev_data, db_path, table_names){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.reactivevalues(rev_data))
  stopifnot(is.character(db_path))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    forms_missing_review <- reactive({nrow(rev_data$summary())})
    
    missing_data_modal <- function(n_missing_review){
      modalDialog(
        title = "Create Report",
        htmlOutput(ns("missing_review_info")),
        HTML("<br>"),
        actionButton(inputId = ns("get_incomplete_report"), 
                     "Create incomplete report"),
        footer = bslib::layout_columns(
          col_widths = c(6,6), 
          actionButton(inputId = ns("check_missing_data"), 
                       "Check remaining forms", 
                       class = "btn-warning m2"),
          modalButton("Cancel")
        ),
        fade = FALSE,
        easyClose = TRUE
      )
    }
    
    report_modal <- function(user_name = r$user_name){
      modalDialog(
        size = "xl",
        bslib::card(
          fill = FALSE, 
          full_screen = TRUE,
          max_height = "80vh",
          bslib::card_header("Report preview - included data", class = "h3"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::card_body(
                htmlOutput(ns("user_name")), 
                tags$hr(),
                shiny::dateInput(
                  label = "Include from date:",
                  ns("include_from_date"), 
                  max = Sys.Date()
                )
              ),
              bslib::card_body(
                tags$hr(),
                shinyWidgets::radioGroupButtons(
                  size = "sm",
                  inputId = ns("report_type"), 
                  label = "Report type:",
                  choiceNames = list(paste0(icon("file-lines"), "<br>Session"), 
                                     paste0(icon("book"), "<br>Everything")),
                  choiceValues = list("session", "all"),
                  selected = "session"
                )
              )
            ),
            bslib::navset_tab(
              bslib::nav_panel(
                title = "Data reviewed", 
                DT::dataTableOutput(ns("review_completed_table"))
              ),
              bslib::nav_panel(
                title = "Queries raised",
                DT::dataTableOutput(ns("queries_raised_table"))
              )
            )
          )
        ),
        footer = bslib::layout_columns(
          col_widths = c(6,6), 
          downloadButton(
            ns("report"), 
            "Generate report", 
            class = "btn-warning m2"
          ),
          modalButton("Cancel")
        ),
        fade = FALSE,
        easyClose = TRUE
      )
    }
    
    
    output[["user_name"]] <- renderText({
      paste0(
        "Include activity from:\n ", 
        tags$b(paste0(r$user_name,  " (", r$user_role, ")"))
        )
    })
    observeEvent(input$create_report, {
      req(forms_missing_review())
      if(forms_missing_review() == 0){
        prep_data(prep_data() + 1)
        showModal(report_modal())
      } else{ showModal(missing_data_modal())}
    })
    
    observeEvent(input$check_missing_data, {
      rev_data$show_all = TRUE
      rev_data$open_modal <- sum(c(rev_data$open_modal, 1), na.rm = TRUE)
    })
    
    prep_data <- reactiveVal(0)
    review_data <- reactiveVal()
    query_data <- reactiveVal()
    
    observeEvent(input$get_incomplete_report, {
      prep_data(prep_data() + 1)
      showModal(report_modal())
    })
    
    observeEvent(prep_data(), {
      req(prep_data() != 0)
      con <- get_db_connection(db_path)
      df <- dplyr::tbl(con, "all_review_data") |> 
        summarize_review_data(
          date_time_vars = c("timestamp", "edit_date_time"),
          common_vars = c("subject_id", "item_group", 
                          "reviewer", "comment")
        )
      review_data(df)
      
      qdf <- collect_query_data(db_path)
      query_data(qdf)
    })
    
        
    selected_review_data <- reactive({
      req(review_data())
      req(input$report_type)
      select_report_data(review_data(), "review_data", input$report_type,
                         paste0(r$user_name,  " (", r$user_role, ")"), 
                         input$include_from_date)
    })
    
    selected_query_data <- reactive({
      req(query_data())
      select_report_data(query_data(), "query_data", input$report_type, 
                         paste0(r$user_name,  " (", r$user_role, ")"), 
                         input$include_from_date)
    })
    
    
    
    output[["missing_review_info"]] <- renderText({
      paste0("<b>", forms_missing_review(), "</b> Forms still need review. ", 
             "Please finish the review before proceeding.")
    })
    
    output[["review_completed_table"]] <- DT::renderDT({
      datatable_custom(selected_review_data(), table_names, rownames = FALSE, 
                       allow_listing_download = FALSE)
    })
    
    output[["queries_raised_table"]] <- DT::renderDT({
      req(query_data())
      datatable_custom(selected_query_data()[, -1], table_names, rownames = FALSE, 
                       allow_listing_download = FALSE)
    })
    
    output[["report"]] <- downloadHandler(
      filename = "report.pdf",
      content = function(file){ create_report(
        file, 
        reviewer = r$user_name, 
        study_sites = unique(bind_rows_custom(r$filtered_data)$site_code),
        review_df = selected_review_data(),
        query_df = selected_query_data()
        ) }
    )
    
  })
}

## To be copied in the UI
# mod_report_ui("report_1")

## To be copied in the server
# mod_report_server("report_1")
