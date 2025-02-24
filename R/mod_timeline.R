#' Interactive timeline - Shiny module UI
#'
#'
#' @inherit mod_timeline_server 
#' 
mod_timeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    timevis::timevisOutput(ns("timeline"))
  )
}

#' Interactive timeline - Shiny module Server
#'
#' A shiny module. Creates an interactive timeline. Used to provide a quick
#' overview of adverse events, severe adverse events, and study activities such
#' as Investigational Product administration. Helpful to judge whether for
#' example an event is related to an Investigational Product.
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param r Common reactive values. Used to access the data frames
#'   `review_data`, `filtered_tables`, and the active `subject_id`.
#'   `review_data` will be used to minimize data points on the timeline that are
#'   already reviewed.
#' @param form A character vector, the form in which the timeline needs to be
#'   embedded. Currently, only the form 'Adverse events' is supported.
#' @param treatment_label Character with the treatment label to use. Defaults to
#'   "💊 Tₓ" if missing.
#'
#' @seealso [mod_timeline_ui()], [mod_common_forms_ui()],
#'   [mod_common_forms_server()]
mod_timeline_server <- function(id, r, form, treatment_label = "\U1F48A T\U2093"){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.character(form), length(form) == 1)
  stopifnot(is.null(treatment_label) || is.character(treatment_label))
  treatment_label <- treatment_label %||% "\U1F48A T\U2093"
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    timeline_data <- reactive({
      req(form == "Adverse events")
      get_timeline_data(
        r$filtered_data, 
        r$filtered_tables, 
        treatment_label = treatment_label
      )
    })
    timeline_data_active <- reactive({
      req(timeline_data())
      # join below is on both form_repeat and item_group. Join by item_group 
      # is to prepare for future timelines when other forms will also be included. 
      review_active <- with(
        r$review_data, 
        r$review_data[item_group == form & subject_id == r$subject_id, ]
        ) |> 
        dplyr::mutate(
          needs_review = any(reviewed == "No"),
          .by = c(form_repeat, item_group)
        ) |> 
        dplyr::distinct(subject_id, form_repeat, item_group, needs_review)
      
      df <- with(timeline_data(), timeline_data()[subject_id == r$subject_id, ]) |> 
        dplyr::left_join(review_active, by = c("subject_id", "form_repeat", "item_group")) |> 
        dplyr::mutate(
          className = ifelse(
            is.na(needs_review), 
            className,
            ifelse(needs_review, className, "bg-secondary")
            )
        )
      df
    })

    if(form != "Adverse events"){
      shinyjs::hide("timeline")
    }
    
    observeEvent(input$timeline_selected, {
      timevis::centerItem("timeline", input$timeline_selected)
    })
    
    output[["timeline"]] <-  timevis::renderTimevis({
      req(form == "Adverse events")
      timevis::timevis(
        data = timeline_data_active(),
        groups = timeline_data_active() |>
          dplyr::mutate(
            "id" = .data[["group"]],
            order = as.numeric(.data[["group"]]),
            "content" = .data[["group"]]) |>
          dplyr::distinct(id, content, order),
        options = list(zoomable = FALSE)
      ) |> 
        htmlwidgets::onRender("timelineRedrawCustom")
    }) 
  })
}

## To be copied in the UI
# mod_timeline_ui("timeline_1")

## To be copied in the server
# mod_timeline_server("timeline_1")
