# Thu Jan 25 16:13:45 2024 ------------------------------
# LSA Useful for use as a minimal query review dataset, for testing modules that write 
# queries (for example, mod_query_follow_up). 

make_review_testdata <- function(){
  data.frame(
    subject_id = c("885", "885", "361", "361"),
    event_name = c("Any visit", "Any visit", "Visit 5", "Visit 6"),
    item_group = c("Adverse events", "Adverse events", "Vital signs", "Vital signs"),
    form_repeat = c(1, 2, 4, 5),
    item_name = c("Atrial Fibrillation", "Cystitis",  "Systolic blood pressure", "Systolic blood pressure"),
    event_date = c("2023-08-15", "2023-09-01", "2023-07-01", "2023-08-01"),
    edit_date_time = c("2023-09-30 01:01:01", "2023-10-01 01:01:01", "2023-08-30 01:01:01", "2023-08-02 01:01:01")
  ) |> 
    dplyr::mutate(
      reviewed = c("Yes", "No", "Yes", "No"), 
      comment = c("test comment", "", "another comment", ""), 
      reviewer = c("Reviewer 1", "", "Reviewer 2", ""), 
      timestamp = c("2000-01-01 01:01:01", "", "2023-09-01 01:01:01", ""),
      status = c("old", "new", "old", "new")
    )
}

fixture_path <- system.file("tests/testthat/fixtures", package = "clinsight")
unlink(file.path(fixture_path, "review_testdb.sqlite"))
db_temp_connect(file.path(fixture_path, "review_testdb.sqlite"), {
  db_add_primary_key(con, "all_review_data", make_review_testdata(), idx_cols)
  db_add_log(con)
})
