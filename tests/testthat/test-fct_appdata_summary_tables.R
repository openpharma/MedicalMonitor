describe("get_timeline_data works", {
  it("creates a data frame with timeline data with the expected columns", {
    appdata <- clinsightful_data |> 
      dplyr::filter(
        subject_id %in% c("BEL_08_885"),
        item_group %in% c("Adverse events",  "Vital signs", "General")
      ) |> 
      get_appdata()
    appvars <- get_meta_vars(appdata)
    apptables <- lapply(setNames(names(appdata), names(appdata)), \(x){
      create_table(appdata[[x]], expected_columns = names(appvars$items[[x]]))
    })
    output <- get_timeline_data(appdata, apptables)
    expect_true(is.data.frame(output))
    expect_equal(
      names(output), 
      c("subject_id", "content", "form_repeat", "item_group", "start", "group", 
        "end", "title", "className", "id", "order")
    )
  })
  it("does not error with missing data", {
    appdata <- clinsightful_data |> 
      dplyr::filter(
        subject_id %in% "BEL_08_885",
        item_group %in% c("Adverse events",  "Vital signs", "General")
      ) |> 
      get_appdata()
    appvars <- get_meta_vars(appdata)
    apptables <- lapply(setNames(names(appdata), names(appdata)), \(x){
      create_table(appdata[[x]], expected_columns = names(appvars$items[[x]]))
    })
    expected_columns <- c("subject_id", "content", "form_repeat", "item_group", 
                          "start", "group", "end", "title", "className", "id", "order")
    
    output <- get_timeline_data(appdata["Adverse events"], apptables["Adverse events"])
    expect_true(is.data.frame(output))
    expect_equal(names(output), expected_columns)
    
    output <- get_timeline_data(appdata["Vital signs"], apptables["Vital signs"])
    expect_true(is.data.frame(output))
    expect_equal(names(output), expected_columns)
    
    output <- get_timeline_data(appdata["General"], apptables["General"])
    expect_true(is.data.frame(output))
    expect_equal(names(output), expected_columns)
    
    expect_warning(
      get_timeline_data(appdata["Gener"], apptables["Gener"]),
      "No data found"
    )
    output <- get_timeline_data(appdata["Gener"], apptables["Gener"]) |> 
      suppressWarnings() 
    expect_true(is.data.frame(output))
    expect_equal(names(output), expected_columns)
  })
  
})
