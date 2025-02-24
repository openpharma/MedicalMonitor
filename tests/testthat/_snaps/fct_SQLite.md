# db_update(). Feature 2 | Update user data database.: Adds a new row to a database if there are new rows

    Code
      DBI::dbGetQuery(con, "SELECT * FROM all_review_data")
    Output
        id  key_col1 item_group     item_name event_date      edit_date_time reviewed
      1  1 Test_name    Visit 1     Test_item 2023-11-01 2023-11-05 01:26:00      Yes
      2  2      9999         ZZ new_test_item 1950/01/01 2023-11-12 01:01:01       No
        comment reviewer               timestamp status
      1            Admin     2023-11-13 01:01:01    old
      2             <NA> 2024-02-02 01:01:01 UTC    new

# db_update(). Feature 2 | Update user data database.: Still performs an update if synch_time is not available

    Code
      DBI::dbGetQuery(con, query)
    Output
         key_col1 item_group     item_name event_date      edit_date_time reviewed
      1 Test_name    Visit 1     Test_item 2023-11-01 2023-11-05 01:26:00      Yes
      2      9999         ZZ new_test_item 1950/01/01 2023-11-12 01:01:01       No
        status
      1    old
      2    new

# db_update(). Feature 2 | Update user data database.: Adds a new row for each data point with a new EditdateTime and updates rows with an updated EditdateTime

    Code
      DBI::dbGetQuery(con, "SELECT * FROM all_review_data")
    Output
        id  key_col1 item_group item_name event_date      edit_date_time reviewed
      1  1 Test_name    Visit 1 Test_item 2023-11-01 2023-11-13 01:01:01       No
        comment reviewer               timestamp  status
      1             <NA> 2024-02-02 01:01:01 UTC updated

---

    Code
      log_tbl[!names(log_tbl) %in% "dml_timestamp"]
    Output
        id review_id      edit_date_time reviewed comment reviewer
      1  1         1 2023-11-05 01:26:00      Yes            Admin
                  timestamp status dml_type
      1 2023-11-13 01:01:01    old   UPDATE

# db_update(). Feature 2 | Update user data database.: Does not change the database if there are no changes (synch_time is the same)

    Code
      db_update(rev_data, db_path = temp_path, common_vars = comvars)
    Output
      [1] "Database up to date. No update needed"

