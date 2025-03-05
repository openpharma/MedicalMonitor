describe("datatable_custom() works", {
  it("creates a datatable", {
    expect_true(inherits(datatable_custom(mtcars), "datatables"))
  })
  it("creates a datatable and can change column names", {
    outcome <- datatable_custom(mtcars, c("mpg_new" = "mpg", "cyl_new" = "cyl"))
    expect_true(inherits(datatable_custom(mtcars), "datatables"))
    expect_equal(
      names(outcome$x$data), 
      c(" ", names(mtcars)) 
      )
    expect_equal(
      stringr::str_extract_all(outcome$x$container, "<th>.*?</th>")[[1]], 
      paste0("<th>", c(" ", "mpg_new", "cyl_new", names(mtcars)[-c(1,2)]), "</th>")
    )
  })
  it("creates a datatable and can add a title to the table", {
    outcome <- datatable_custom(iris, title = "Test title")
    expect_true(inherits(outcome, "datatables"))
    expect_true(grepl("Test title", outcome$x$options$initComplete))
  })
  it("Produces a valid datatable with zero rows if no rows provided in the input table", {
    outcome <- datatable_custom(iris[0,])
    expect_true(inherits(outcome, "datatables"))
    # -1 to remove the standard row name column: 
    expect_equal(outcome$x$data[, -1], iris[0, ])
  })
  it("Errors if not a valid data frame is provided", {
    expect_error(datatable_custom(NULL))
  })
  it("Errors if rename_vars is not a character vector", {
    expect_error(datatable_custom(iris, rename_vars = data.frame()))
      })
  it("Errors if title is not a character vector", {
    expect_error(datatable_custom(iris, title = data.frame()))
  })
  it(
    "Optionally adds a button for downloading an Excel table, 
    with the filename and title of downloadable table containing 
    the 'export_label' argument.", 
    {
      outcome <- datatable_custom(
        mtcars, 
        export_label = "test_label",
        allow_listing_download = TRUE
      )
      expected_settings <- list(
        extend = "excel", 
        text = "<i class=\"fa-solid fa-download\"></i>", 
        filename = "clinsight.test_label", 
        title = "test_label | extracted from ClinSight"
      )
      expect_equal(outcome$x$options$buttons[[1]], expected_settings)
      expect_true("Buttons" %in% unlist(outcome$x$extensions))
    }
  )
  it(
    "The filename and title of a downloadable table contain '_label.missing_' 
    if the 'export_label' is missing", 
    {
      outcome <- datatable_custom(mtcars, allow_listing_download = TRUE)
      expected_settings <- list(
        extend = "excel", 
        text = "<i class=\"fa-solid fa-download\"></i>", 
        filename = "clinsight._label.missing_", 
        title = "_label.missing_ | extracted from ClinSight"
      )
      expect_equal(outcome$x$options$buttons[[1]], expected_settings)
      expect_true("Buttons" %in% unlist(outcome$x$extensions))
    }
  )
})
