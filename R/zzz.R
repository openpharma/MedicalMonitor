.onLoad <- function(libname, pkgname) {
  shiny::registerInputHandler('CS.reviewInfo', function(val, ...) {
    with(val, data.frame(
      id = unlist(ids), 
      reviewed = ifelse(isTRUE(review), "Yes", ifelse(isFALSE(review), "No", NA_character_)),
      row_id = row_id
      ))
  }, TRUE)
}
