describe(
  "db_create(). Feature 1 | Create app user database. 
      As a user, I want to be able to create an app database that 
      contains the necessary tables for the application to function. ", 
  {
    it("Can create an app database", {
      rev_data <- data.frame(
        subject_id = "Test_name",
        event_name = "Visit 1",
        item_group = "Test_group",
        form_repeat = 1,
        item_name = "Test_item",
        event_date = "2023-11-01",
        edit_date_time = "2023-11-05 01:26:00"
      )
      review_expected <- data.frame(
        reviewed = "No",
        comment = "",
        reviewer = "",
        status = "new"
      )
      temp_path <- withr::local_tempfile(fileext = ".sqlite") 
      
      db_create(rev_data, temp_path)
      
      con <- get_db_connection(temp_path)
      expect_equal(
        DBI::dbListTables(con), 
        c("all_review_data", "all_review_data_log", "db_synch_time", 
          "db_version", "query_data", "sqlite_sequence")
        )
      expect_equal(
        colnames(dplyr::tbl(con, "all_review_data")), 
        c("id", names(rev_data), "reviewed", "comment", "reviewer", "timestamp", "status")
      )
      rev_data_remote <- dplyr::collect(dplyr::tbl(con, "all_review_data"))
      expect_true(is.character(rev_data_remote[["timestamp"]])) 
      rev_data_remote[["timestamp"]] <- NULL
      expect_equal(
        rev_data_remote,
        dplyr::as_tibble(cbind(list(id = 1), rev_data, review_expected))
      )
      
      expect_equal(
        colnames(dplyr::tbl(con, "all_review_data_log")), 
        c("id", "review_id", "edit_date_time", "reviewed", "comment", 
          "reviewer", "timestamp", "status", "dml_type", "dml_timestamp")
      )
      
      expect_equal(
        dplyr::collect(dplyr::tbl(con, "db_synch_time")), 
        dplyr::tibble(synch_time = "")
      )
      expect_equal(
        dplyr::collect(dplyr::tbl(con, "db_version")), 
        dplyr::tibble(version = "1.1")
      )
      expect_equal(
        dplyr::collect(dplyr::tbl(con, "query_data")), 
        dplyr::as_tibble(cbind(list(id = numeric()), query_data_skeleton))
      )
    })
    it("Can create a .sqlite app database if the database folder does not exist, 
       with the required tables named [all_review_data], db_synch_time, and query_data", {
      rev_data <- data.frame(
        subject_id = "Test_name",
        event_name = "Visit 1",
        item_group = "Test_group",
        form_repeat = 1,
        item_name = "Test_item",
        event_date = "2023-11-01",
        edit_date_time = "2023-11-05 01:26:00"
      )
      temp_path <- file.path(withr::local_tempdir(), "non_existing_folder/db.sqlite")
      
      db_create(rev_data, temp_path)
      
      expect_true(file.exists(temp_path))
      con <- get_db_connection(temp_path)
      expect_equal(
        DBI::dbListTables(con), 
        c("all_review_data", "all_review_data_log", "db_synch_time", 
          "db_version", "query_data", "sqlite_sequence")
      )
    })
  }
)


describe(
  "db_update(). Feature 2 | Update user data database.", 
  {
    comvars = c("key_col1", "item_group")
    old_data <- data.frame(
      key_col1  = "Test_name",
      item_group = "Visit 1",
      item_name = "Test_item",
      event_date = "2023-11-01",
      edit_date_time = "2023-11-05 01:26:00"
      )
    new_data <- data.frame(
      key_col1 = "9999",
      item_group = "ZZ", 
      item_name = "new_test_item",
      event_date    = "1950/01/01", 
      edit_date_time = "2023-11-12 01:01:01"
    ) 
    review_cols <- data.frame(
      reviewed = "Yes",
      comment = "", 
      reviewer = "Admin",
      timestamp = "2023-11-13 01:01:01", 
      status = "old"
    )
    it("Never changes original entries in the database", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite") 
      con <- get_db_connection(temp_path)
      
      db_add_primary_key(con, "all_review_data", cbind(old_data, review_cols), comvars)
      db_add_log(con, c("id", comvars))
      DBI::dbWriteTable(con, "db_synch_time", data.frame("synch_time" = "2024-01-01 01:01:01 UTC"))
      
      df_old <- cbind(id = 1, old_data, review_cols)
      log_old <- DBI::dbGetQuery(con, "SELECT * FROM all_review_data_log")
      
      rev_data <- rbind(old_data, new_data)
      # newer synch_time indicating need for update:
      attr(rev_data, "synch_time") <- "2024-02-02 01:01:01 UTC" 
      db_update(rev_data, db_path = temp_path, common_vars = comvars)
      expect_equal(
        DBI::dbGetQuery(con, "SELECT * FROM all_review_data")[1,],
        df_old
        )
      expect_equal(
        nrow(log_old),
        0
      )
      expect_equal(
        DBI::dbGetQuery(con, "SELECT * FROM all_review_data_log"),
        log_old
      )
    })
    it("Adds a new row to a database if there are new rows", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite") 
      con <- get_db_connection(temp_path)
      db_add_primary_key(con, "all_review_data", cbind(old_data, review_cols), comvars)
      db_add_log(con, c("id", comvars))
      DBI::dbWriteTable(con, "db_synch_time", data.frame("synch_time" = "2024-01-01 01:01:01 UTC"))
      
      log_old <- DBI::dbGetQuery(con, "SELECT * FROM all_review_data_log")
      
      rev_data <- rbind(old_data, new_data)
      attr(rev_data, "synch_time") <- "2024-02-02 01:01:01 UTC" 
      db_update(rev_data, db_path = temp_path, common_vars = comvars)
      expect_snapshot(DBI::dbGetQuery(con, "SELECT * FROM all_review_data"))
      expect_equal(
        nrow(log_old),
        0
      )
      expect_equal(
        DBI::dbGetQuery(con, "SELECT * FROM all_review_data_log"),
        log_old
      )
    })
    it("Still performs an update if synch_time is not available", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite") 
      con <- get_db_connection(temp_path)
      db_add_primary_key(con, "all_review_data", cbind(old_data, review_cols), comvars)
      
      rev_data <- rbind(old_data, new_data) # no synch_time attribute added
      db_update(rev_data, db_path = temp_path, common_vars = comvars)
      
      # exclude time stamp since it defaults to current date/time when 
      # synch_date is not available:
      check_cols <- paste0(c("key_col1", "item_group", "item_name", "event_date", 
                       "edit_date_time", "reviewed", "status"), collapse = ", ")
      query <- paste("SELECT", check_cols, "FROM all_review_data")
      expect_snapshot(DBI::dbGetQuery(con, query))
    })
    
    it("Adds a new row for each data point with a new EditdateTime and updates rows with an updated EditdateTime", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite") 
      con <- get_db_connection(temp_path)
      db_add_primary_key(con, "all_review_data", cbind(old_data, review_cols), comvars)
      db_add_log(con, c("id", comvars))
      DBI::dbWriteTable(con, "db_synch_time", data.frame("synch_time" = "2024-01-01 01:01:01 UTC"))
      
      rev_data <- old_data |> 
        dplyr::mutate(edit_date_time = "2023-11-13 01:01:01")
      attr(rev_data, "synch_time") <- "2024-02-02 01:01:01 UTC" 
      
      db_update(rev_data, db_path = temp_path, common_vars = comvars)
      expect_snapshot(DBI::dbGetQuery(con, "SELECT * FROM all_review_data"))
      log_tbl <- DBI::dbGetQuery(con, "SELECT * FROM all_review_data_log")
      # Drop dml_timestamp
      expect_snapshot(log_tbl[!names(log_tbl) %in% "dml_timestamp"])
    })
    it("Does not change the database if there are no changes (synch_time is the same)", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite") 
      con <- get_db_connection(temp_path)
      
      synch_time <- "2024-01-01 01:01:01 UTC"
      rev_data <- cbind(old_data, review_cols)
      attr(rev_data, "synch_time") <- synch_time
      db_add_primary_key(con, "all_review_data", rev_data, comvars)
      db_add_log(con, c("id", comvars))
      DBI::dbWriteTable(con, "db_synch_time", data.frame("synch_time" = synch_time))
      
      log_old <- DBI::dbGetQuery(con, "SELECT * FROM all_review_data_log")
      
      expect_snapshot({
        db_update(rev_data, db_path = temp_path, common_vars = comvars)
      })
      attr(rev_data, "synch_time") <- NULL
      expect_equal(rev_data, DBI::dbGetQuery(con, "SELECT * FROM all_review_data")[,-1])
      expect_equal(
        nrow(log_old),
        0
      )
      expect_equal(
        DBI::dbGetQuery(con, "SELECT * FROM all_review_data_log"),
        log_old
      )
    })
    
  }
)

describe(
  "db_save_review() works", 
  {
    # # for dev purposes:      
    # rv_row <- dplyr::tbl(db, "all_review_data") |>
    #   dplyr::filter(reviewed == "No") |>
    #   dplyr::filter(subject_id == "IME-AU01-001", item_group == "Adverse events") |>
    #   dplyr::collect()
    df <- data.frame(
      key_col1 = "9999",
      item_group = "Group 1", 
      item_name = "Test item",
      event_date    = "1950/01/01", 
      edit_date_time = "2023-11-12 01:01:01"
    )
    
    old_review <- data.frame( 
      reviewed = "No",
      comment = "", 
      reviewer = "",
      timestamp = "2023-11-14 01:01:01", 
      status = "new"
    ) 
    
    new_review <- data.frame(
      reviewed = "Yes",
      comment = "Succes", 
      reviewer = "Admin 01",
      timestamp = "2023-11-14 01:01:01", 
      status = "old"
    )
    
    it("Can update a review info row", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite")
      con <- get_db_connection(temp_path)
      db_add_primary_key(con, "all_review_data", cbind(df, old_review))
      db_add_log(con, "id")
      
      db_save_review(
        cbind(id = 1, df, new_review), 
        temp_path, 
        table = c("all_review_data")
      )
      expect_equal(
        dplyr::collect(dplyr::tbl(con, "all_review_data")), 
        dplyr::as_tibble(cbind(id = 1, df, new_review))
      )
      log_tbl <- dplyr::collect(dplyr::tbl(con, "all_review_data_log"))
      expect_equal(
        log_tbl[c("id", "review_id", "edit_date_time", names(old_review))],
        cbind(id = 1, review_id = 1, edit_date_time = "2023-11-12 01:01:01",
              old_review),
        ignore_attr = TRUE
      )
    })
    it("Updates all items from a item_group as reviewed", {
      df <- data.frame(
        key_col1 = "9999",
        item_group = "Group 1", 
        item_name = c("Test item", "Test item2"),
        event_date    = c("1950/01/01", "1960/01/01"), 
        edit_date_time = c("2023-11-12 01:01:01", "2023-11-10 01:01:01")
      )
      old_review <- data.frame( 
        reviewed = "No",
        comment = "", 
        reviewer = "",
        timestamp = "", 
        status = "new"
      ) 
      new_review <- data.frame(
        reviewed = "Yes",
        comment = "Succes", 
        reviewer = "Admin 01",
        timestamp = "2023-11-14 01:01:01", 
        status = "old"
      )
      review_row <- data.frame(
        id = 1:2,
        key_col1 = "9999",
        item_group = "Group 1"
      ) |> 
        cbind(new_review)
      
      temp_path <- withr::local_tempfile(fileext = ".sqlite")
      con <- get_db_connection(temp_path)
      db_add_primary_key(con, "all_review_data", cbind(df, old_review))
      db_add_log(con, "id")
      db_save_review(
        review_row, 
        temp_path, 
        table = c("all_review_data")
      )
      expect_true(is.data.frame(dplyr::collect(dplyr::tbl(con, "all_review_data"))))
      results <- dplyr::collect(dplyr::tbl(con, "all_review_data"))
      expect_equal(nrow(results), 2)
      expect_equal(results$status, c("old", "old"))
      expect_equal(
        results,
        dplyr::as_tibble(cbind(id = 1:2, df, new_review)))
      
      expect_true(is.data.frame(dplyr::collect(dplyr::tbl(con, "all_review_data_log"))))
      results <- dplyr::collect(dplyr::tbl(con, "all_review_data_log"))
      expect_equal(nrow(results), 2)
      expect_equal(results$status, c("new", "new"))
      expect_equal(
        results[c("id", "review_id", names(old_review))],
        dplyr::as_tibble(cbind(id = 1:2, review_id = 1:2, old_review)))
    })
    
    
    
    it("warns with duplicate records in the review_row object", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite")
      con <- get_db_connection(temp_path)
      
      db_add_primary_key(con, "all_review_data", cbind(df, old_review))
      db_add_log(con, "id")
      db_save_review(
        rbind(cbind(id = 1:2, df, new_review), cbind(id = 1:2, df, new_review)), 
        temp_path, 
        table = "all_review_data"
      ) |> expect_warning()
    })
  }
)

describe(
  "db_save() works", 
  {
    temp_path <- withr::local_tempfile(fileext = ".sqlite") 
    con <- get_db_connection(temp_path)
    db_add_primary_key(con, "query_data", query_data_skeleton)
    
    it("can save a query", {
      new_query <- data.frame(
        query_id = "ID124234", 
        type = "Normal",
        subject_id = "ID1",
        event_label = "Visit 1",
        item_group = "Vital signs",
        item = "Heart rate",
        timestamp = "2023-11-01 01:01:01", 
        n = 1, 
        reviewer = "Admin01", 
        query = "Test query",
        resolved = "",
        resolved_date = "", 
        edit_reason = ""
      )
      db_save(data = new_query, db_path = temp_path, db_table = "query_data")
      expect_equal(
        dplyr::collect(dplyr::tbl(con, "query_data"))[,-1], 
        dplyr::as_tibble(new_query)
        )
    })
  }
)


describe("db_get_query can collect latest query data from a database", {
  temp_path <- withr::local_tempfile(fileext = ".sqlite") 
  con <- get_db_connection(temp_path)
  
  new_query <- dplyr::tibble(
    query_id = "ID124234", 
    subject_id = "ID1",
    n = 1:2,
    timestamp = c("2024-02-05 01:01:01", "2024-04-01 01:01:01"),
    other_info = "testinfo"
    ) 
  db_add_primary_key(con, "query_data", new_query)
  
  it("Can collect the desired data.", {
    query_output <- db_get_query(temp_path, query_id = "ID124234", n = 1)
    expect_equal(query_output[,-1], new_query[1, ])
  })
  
  it("Collects an empty data frame if query_id or n are not found", {
    query_output <- db_get_query(temp_path, query_id = "non-existent", n = 1)
    expect_equal(query_output[,-1], new_query[0,])
    query_output <- db_get_query(temp_path, query_id = "ID124234", n = 6)
    expect_equal(query_output[,-1], new_query[0,])
  })
  
  it("Collects all rows with the same query ID if n is set to NULL", {
    query_output <- db_get_query(temp_path, query_id = "ID124234", n = NULL)
    expect_equal(query_output[,-1], new_query)
  })
  
})


describe("db_get_review can collect latest review data from a database", {
  temp_path <- withr::local_tempfile(fileext = ".sqlite") 
  con <- get_db_connection(temp_path)
 
  review_data <- data.frame(
    subject_id = "Test_name",
    event_name = "Visit 1",
    item_group = "Test_group",
    form_repeat = 1,
    item_name = "Test_item",
    edit_date_time = "2023-11-05 01:26:00",
    timestamp = "2024-02-05 01:01:01"
  ) |> 
    dplyr::as_tibble()
  db_add_primary_key(con, "all_review_data", review_data)
  db_add_log(con, "id")

  it("Can collect the desired data.", {
    output <- db_get_review(temp_path, subject_id = "Test_name", item_group = "Test_group")
    expect_equal(output[,-1], review_data)
  })
  
  it("Collects an empty data frame if the requested subject or form are not found", {
    output <- db_get_review(temp_path, subject_id = "Non-existent", 
                            item_group = "Test_group")
    expect_equal(output[,-1], review_data[0,])
    output <- db_get_review(temp_path, subject_id = "Test_name", 
                            item_group = "Non-existent")
    expect_equal(output[,-1], review_data[0,])
  })
  
  it("Throws a warning if no filters are specified and returns full table", {
    expect_warning(output <- db_get_review(temp_path))
    expect_equal(output[,-1], review_data)
  })
  
  it("Throws a warning if specified filters are unnamed and returns full table", {
    expect_warning(
      output <- db_get_review(temp_path, "Test_name"), 
      "Unnamed arguments passed"
      )
    expect_equal(output[,-1], review_data)
  })
  
  it("Errors if provided filters cannot be coerced to a data frame", {
    expect_error(
      db_get_review(
        temp_path, 
        event_name = c("Visit 1", "Visit 2"), 
        subject_id = c("Test_name", "another name", "third name")
        ), 
      "arguments imply differing number of rows"
    )
  })

})
