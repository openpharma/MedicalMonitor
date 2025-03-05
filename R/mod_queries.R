#' Queries - Shiny module UI
#'
#' @inherit mod_queries_server
#'
#' @seealso [mod_queries_server()], [mod_go_to_form_ui()], [mod_go_to_form_server()]
#' 
mod_queries_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(8,4),
      bslib::card(
        bslib::card_body(
          shinyWidgets::materialSwitch(
            inputId = ns("show_resolved"),
            label = "Show resolved queries", 
            status = "primary",
            right = TRUE
          ),
          fill = FALSE
        ),
        bslib::card_body(
          shinycssloaders::withSpinner(
            DT::DTOutput(ns("queries")),
            type = 5
          )
        ),
        bslib::card_body(
          max_height = "150px",
          mod_go_to_form_ui(ns("go_to_form")),
          fillable = FALSE
        )
      ),
      bslib::layout_columns(
        col_widths = c(12,12),
        bslib::card(
          id = ns("details_panel"),
          bslib::card_body(
            htmlOutput(ns("selected_query_title")),
            DT::DTOutput(ns("selected_query")),
            HTML("<br>")
          ), 
          full_screen = TRUE
        ),
        if (isTRUE(get_golem_config("allow_query_inputs"))) {
          mod_query_follow_up_ui(ns("query_follow_up_1"))
        }
      )
    )
  )
}

#' Queries - Shiny module Server
#'
#' A Shiny module. Used to display all raised queries.
#'
#' The module displays all raised queries in a table. Upon clicking on a query,
#' the query follow-up messages will be shown. In addition, it will be possible
#' to write a follow-up message. The logic for writing the follow-up message is
#' extracted in a different module, named [mod_query_follow_up_server()]. New
#' queries are created in the module [mod_query_add_server()]. The button to
#' initiate this module can be found in the main sidebar
#' [mod_main_sidebar_server()].
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param r Common reactive values. Used to access the data frame `query_data`,
#'   and the reviewers' `user_name`. User name is needed for creating a
#'   follow-up to an existing query.
#' @param table_names An optional character vector. If provided, will be used
#'   within [datatable_custom()], to improve the column names in the final
#'   interactive tables.
#' @param db_path A character string with the path to the database.
#'
#' @inherit mod_go_to_form_server
#' @seealso [mod_queries_ui()], [mod_go_to_form_ui()],
#'   [mod_go_to_form_server()], [mod_query_follow_up_ui()],
#'   [mod_query_follow_up_server()], [mod_query_add_ui()],
#'   [mod_query_add_server()]. 
#' 
mod_queries_server <- function(id, r, navinfo, all_forms, db_path, table_names){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.character(db_path))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    selected_query <- reactive({
      req(nrow(initial_queries())>0)
      req(input$queries_rows_selected)
      req(input$queries_rows_selected <= nrow(initial_queries()))
      with(initial_queries(), query_id[input$queries_rows_selected])
    })
    
    selected_query_data <- reactive({
      req(nrow(r$query_data)>0)
      req(selected_query())
      req(selected_query() %in% unique(r$query_data$query_id))
      with(r$query_data, r$query_data[query_id == selected_query(), ]) |> 
        dplyr::mutate(reviewer = paste0(reviewer, " ", timestamp))
    })
    
    if (isTRUE(get_golem_config("allow_query_inputs"))) {
      mod_query_follow_up_server("query_follow_up_1",  r = r, 
                                 selected_query = selected_query, 
                                 db_path = db_path)
    }
    
    initial_queries <- reactive({
      df <- with(r$query_data, r$query_data[n == 1, ] )
      if(identical(nrow(df),0)) return(df) 
      df <- df |>
        dplyr::slice_min(timestamp, by = c(subject_id, event_label, query_id)) |>
        dplyr::arrange(.data[["resolved"]], .data[["type"]]) |> 
        # Only show part of a long message for better display in table, since 
        # the entire message will already be shown when clicking on the row.
        dplyr::mutate(
          query = ifelse(
            nchar(query)>40, 
            paste0(trimws(substr(query, 1, 40)), "..."), 
            query
          )
        )
      if(isTRUE(input$show_resolved)) return(df)
      with(df, df[resolved == "No", ] )
    })
    
    observeEvent(input$queries_rows_selected, {
      input$queries_rows_selected
    })
    
    mod_go_to_form_server(
      id = "go_to_form", 
      r = r, 
      navinfo = navinfo,
      navtable = initial_queries, 
      tablerow = reactive(input$queries_rows_selected),
      all_forms = all_forms,
      form_name = "item_group", 
      subject_id = "subject_id"
    )
    
    output[["queries"]] <- DT::renderDT({
      req(initial_queries())
      query_cols <- c("subject_id", "type", "event_label", 
                      "item_group", "query", "timestamp")
      table_title <- "Open queries"
      if(input$show_resolved){
        query_cols <- c("resolved", query_cols)
        table_title <- "All queries"
      }
      
      # determine DT dom / exts / opts
      
              
      datatable_custom(
        initial_queries()[query_cols], 
        table_names, 
        title = table_title,
        callback = dblclick_to_form(ns("go_to_form")),
        export_label = paste(ifelse(input$show_resolved, "all", "open"),
                           "queries", sep = ".")
      )
    })
    
    output[["selected_query_title"]] <- renderText({
      req(selected_query_data())
      df <- selected_query_data()[1,]
      paste0(
        "<b>",
        htmlEscape(df$subject_id), ": ",
        htmlEscape(df$item), 
        ifelse(identical(df$type, "Major"), " - <i>Major query</i>", ""),
        "</b><br>", 
        htmlEscape(df$item_group), ", ", 
        df$event_label,
        ifelse(identical(df$resolved, "Yes"), " (resolved)", "")
      )
    })
    
    output[["selected_query"]] <- DT::renderDT({
      req(selected_query_data())
      datatable_custom(
        selected_query_data()[c("query", "reviewer")], 
        rename_vars = table_names,
        dom = 't',
        options = list(
          scroller = FALSE,
          pageLength = -1
        ),
        class = "row-border hover",
        rownames = FALSE,
        selection = "none",
        allow_listing_download = FALSE
      )
    })
  })
}

## To be copied in the UI
# mod_queries_ui("queries_1")

## To be copied in the server
# mod_queries_server("queries_1")
