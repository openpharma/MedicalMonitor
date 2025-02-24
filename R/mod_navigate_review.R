#' Navigate review tables - Shiny module UI
#'
#' @description A shiny Module.
#'
#' @inherit mod_navigate_review_server 
#' 
#' @seealso [mod_navigate_review_server()]
mod_navigate_review_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::value_box(
      id = ns("review_value_box"),
      title = "To review:",
      theme = "primary",
      value = uiOutput(ns("forms_to_review")),
      showcase = icon("clipboard-list", class = 'fa-2x')
    )
  )
}

#' Navigate review tables - Shiny module Server
#'
#' A Shiny module. Used to display all data that needs review and all queries that are still open.
#' It enables the user to directly navigate to a specific form or patient.
#' 
#' The module will be called from the main server and will open a modal with 
#' two tables: a table with forms that need to be reviewed and a table with 
#' queries that are raised. There is a switch in the modal, which can toggle 
#' the tables to only show data of the currently active and selected participant, 
#' or to show data of all the participants that need to be reviewed. 
#' There is a button to navigate to the form of a selected data frame. Once 
#' clicked, this will activate the module [mod_go_to_form_server()], which is a 
#' low-level helper module to change the active participant and active review 
#' form to the one in the selected table row.
#'
#' @param id Character string, used to connect the module UI with the module Server. 
#' @param r Common reactiveValues. Used to access `subjectid` (the active Subject), 
#' to select the correct review data and to change the active subject id if 
#' requested by the user. 
#' @param rev_data Common reactiveValues. Used to access `rev_data$summary` (a data 
#' frame with summarized review data), `rev_data$show_all` (logical, whether 
#' to show data of everyone in the tables or only data of the selected subject id.)
#' @param all_forms A data frame containing two columns: a column named "form" with 
#' all the form to navigate through in the correct order, and a column named 
#' "main_tab", showing the name of the main_tab in which the form can be found. 
#' Used for navigation purposes, will be passed onto the internal Shiny module 
#' [mod_go_to_form_server()].
#' @param table_names An optional character vector. If provided, will be used
#'   within [datatable_custom()], to improve the column names in the final
#'   interactive tables.
#' @inherit mod_go_to_form_server
#' @seealso [mod_navigate_review_ui()], [mod_go_to_form_ui()], 
#' [mod_go_to_form_server()]
#' 
mod_navigate_review_server <- function(
    id, 
    r, 
    rev_data, 
    navinfo, 
    all_forms,
    table_names = NULL
){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.reactivevalues(rev_data))
  stopifnot(is.reactivevalues(navinfo))
  stopifnot(isolate(is.reactive(rev_data$summary)))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    modal_rev_data <- reactive({
      req(rev_data$summary())
      if(input$show_all_data){rev_data$summary()} else{
        dplyr::filter(rev_data$summary(), subject_id == r$subject_id) 
      }
    }) 
    
    modal_nav_review <- function(){
      modalDialog(
        title = shiny::htmlOutput(ns("header_text")),
        size = "xl",
        fade = FALSE,
        footer = modalButton("Cancel"),
        bslib::navset_tab(
          id = ns("review_tabs"), 
          bslib::nav_panel(
            title = "Review data",
            DT::DTOutput(ns("review_df")),
            mod_go_to_form_ui(ns("go_to_form"))
          ),
          bslib::nav_panel(
            title = "Open queries",
            DT::DTOutput(ns("queries_table"))
          ),
          bslib::nav_spacer(),
          bslib::nav_item(
            shinyWidgets::materialSwitch(
              inputId = ns("show_all_data"),
              label = "Show everyone", 
              value = FALSE,
              status = "primary"
            )
          )
        ),
        easyClose = TRUE
      )
    }
    
    observeEvent(rev_data$open_modal, {
      cat("review details click detected\n")
      r$subject_id <- rev_data$subject
      shinyWidgets::updateMaterialSwitch(session, "show_all_data", value = rev_data$show_all)
      showModal(modal_nav_review())
    }, ignoreInit = TRUE)
    
    shinyjs::onclick("review_value_box", {
      golem::cat_dev("click on forms to review detected\n")
      shinyWidgets::updateMaterialSwitch(session, "show_all_data", value = FALSE)
      showModal(modal_nav_review())
    })
    
    output[["header_text"]] <- renderText({
      if(input$show_all_data){paste0("<b>All data</b>")} else{
        paste0("<b>", r$subject_id, "</b>")
      }
    })
    
    output[["review_df"]] <- DT::renderDT({
      df <- modal_rev_data() 
      df[["reviewed"]] <- NULL 
      if(!input$show_all_data) df$subject_id <- NULL
      datatable_custom(df, table_names,
                       callback = dblclick_to_form(ns("go_to_form")), 
                       allow_listing_download = FALSE)
    }) 
    
    queries_table_data <- reactive({
      # Select the initial query for every query id with slice_min: 
      #TODO: try to use the tables of mod_queries instead to remove duplication
      df <- dplyr::slice_min(r$query_data, timestamp, by = query_id) |> 
        dplyr::filter(resolved == "No") |> 
        dplyr::mutate(
          ID = paste0(item, " (", item_group, ", ", event_label, ")"),
          ID = ifelse(type == "Major", paste0(ID, " Major query"), ID)
        ) |> 
        dplyr::select(tidyr::all_of(c("subject_id", "ID", "query"))) 
      if(input$show_all_data) df else { 
        dplyr::filter(df, subject_id == r$subject_id) |> 
          dplyr::select(-subject_id)
      }
    })
    
    output[["queries_table"]] <- DT::renderDT({
      datatable_custom(
        queries_table_data(), 
        rename_vars = table_names,
        options = list(
          scroller = FALSE,
          pageLength = -1
          ),
        rownames = FALSE,
        selection = "none",
        allow_listing_download = FALSE
        )
    })
    
    mod_go_to_form_server(
      id = "go_to_form", 
      r = r, 
      navinfo = navinfo,
      navtable = modal_rev_data, 
      tablerow = reactive(input$review_df_rows_selected),
      all_forms = all_forms
    )
    
    forms_to_review <- reactive({
      with(rev_data$summary(), Form[subject_id == r$subject_id])
    })
    
    output[["forms_to_review"]] <- renderText({
      length(unique(forms_to_review()))
    })
    
  })
}


## To be copied in the UI
# mod_navigate_review_ui("navigate_review_1")

## To be copied in the server
# mod_navigate_review_server("navigate_review_1")
