shiny::registerInputHandler('CS.reviewInfo', function(val, ...) {
  with(val, data.frame(
    id = unlist(ids), 
    reviewed = ifelse(isTRUE(review), "Yes", ifelse(isFALSE(review), "No", NA_character_)),
    row_id = row_id
    ))
}, TRUE)

checkbox_callback <- DT::JS("checkboxCallback(table);")
checkbox_render <- DT::JS("checkboxRender")
row_callback <- DT::JS("rowCallback")

progress_bar <- function(outputId) {
  div(
    id = outputId,
    class = "cs-progress-container",
    div(
      class = "cs-progress-bar",
      div(class = c("cs-progress", "completed")),
      div(class = c("cs-progress", "unmarking")),
      div(class = c("cs-progress", "marking"))
    ),
    div(
      class = "cs-completed"
    )
  )
}

render_progress_bar <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- exprToFunction(expr, env, quoted)
  
  function(){
    func()
  }
}
