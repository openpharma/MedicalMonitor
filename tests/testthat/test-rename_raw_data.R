describe("rename_raw_data() renames raw study data and throws informative errors if needed.", {
  
  it("renames a data frame as expected", {
    col_names <- rbind(metadata$column_names, list("Placeholder", "pl"))
    testdata <- lapply(mtcars, as.character) |> as.data.frame()
    col_names$name_raw <- names(testdata)
    
    expect_no_error(outcome <- rename_raw_data(testdata, column_names = col_names))
    expect_true(is.data.frame(outcome))
    expect_equal(names(outcome), col_names$name_new)
  })
  
  it("errors with incorrect input", {
    col_names <- rbind(metadata$column_names, list("Placeholder", "pl"))
    col_names$name_raw <- names(mtcars)
    expect_error(
      rename_raw_data(mtcars, column_names = "incorrect input"),
      "should be a data frame"
    )
    expect_error(
      rename_raw_data("incorrect input", column_names = col_names), 
      "should be a data frame"
    )
    incorrect_raw_names <- col_names
    incorrect_raw_names$name_raw <- paste0("incorrect_name_", 1:nrow(col_names))
    expect_error(
      rename_raw_data(mtcars, column_names = incorrect_raw_names), 
      "The following columns are missing"
    )
    incorrect_new_names <- col_names
    incorrect_new_names$name_new <- paste0("incorrect_name_", 1:nrow(col_names))
    expect_error(
      rename_raw_data(mtcars, column_names = incorrect_new_names), 
      "The following columns are missing"
    )
  })
})
