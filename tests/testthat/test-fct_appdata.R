describe("get_raw_csv_data works", {
  it("Produces the expected output.", {
    data_path <- test_path("fixtures", "csvtestdata")
    expect_no_error(df <- get_raw_csv_data(data_path, synch_time = "2024-01-01 00:00:00"))
    expect_snapshot(df)
    expect_equal(attr(df, "synch_time"),  "2024-01-01 00:00:00")
  })
})

describe(
  paste0("merge_meta_with_data. Feature 1 | Merge raw data with metadata. ",
         "As a user, I want to be able to merge raw data with ", 
         "metadata. Furthermore, I want to be able to fix suffixes and rename the ", 
         "limits and significance values to the standard names used in the app."), 
  {
    data_path <- test_path("fixtures", "csvtestdata")
    raw_data <- get_raw_csv_data(data_path, synch_time = "2024-01-01 00:00:00")
    
    it("Produces a data frame without errors", {
      expect_true(is.data.frame(merge_meta_with_data(raw_data, metadata)))
      expect_equal(nrow(merge_meta_with_data(raw_data, metadata)), 543)
      expect_true(
        all(metadata$raw_column_names$col_new[1:6] %in% 
          names(merge_meta_with_data(raw_data, metadata)))
        )
    })
    
    it(paste0("Scenario 1 - Given a data frame with raw data,", 
              "I expect that the output will be the same as recorded in a snapshot."), {
                df <- merge_meta_with_data(raw_data, metadata)
                expect_snapshot(df[c(1, 1000, 2000, 3000, 4000, 5000), ])
                expect_snapshot(df)
              })
  }
)

describe(
  "merge_meta_with_data. Feature 2 | Recreate clinsightful_data.
  As a user, I want to be able to recreate internal package data with raw data files", 
  {
    it("Scenario 1 | recreate data. clinsightful_data can be recreated as expected", {
      df <- get_raw_csv_data(
        app_sys("raw_data"), 
        synch_time = "2023-09-15 10:10:00 UTC"
      ) |> 
        merge_meta_with_data(meta = metadata)
      expect_equal(clinsightful_data, df)
    })
  }
)


describe(
  "get_appdata works", 
  {
    it("produces the expected output", {
      expect_snapshot(get_appdata(clinsightful_data, metadata))  
    })
  }
)

