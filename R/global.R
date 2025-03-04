## needed for base R function mocking during test:
file.show <- NULL

# use script below for development purposes, to load data global environment
# with eval(global)
global <- quote({
  data_folder <- Sys.getenv("DATA_FOLDER", app_sys())
  data_local <- file.path(data_folder, "study_data.rds")
  raw_data_remote <- Sys.getenv("RAW_DATA_PATH")
  meta_data <- get_metadata(file.path(data_folder, "metadata.xlsx"))
    
  if(!file.exists(data_local)){
    warning("No data found. Trying to rebuild data from remote source")
    study_data <- raw_data_remote |> 
      get_raw_csv_data() |> 
      merge_meta_with_data(meta = meta_data)
    cat("saving data locally\n")
    saveRDS(study_data, data_local)
    if(!file.exists(data_local)) stop("Could not save data set locally.")
  }
  Sys.setenv("GOLEM_CONFIG_ACTIVE" = "shinymanager")
  run_app(data_folder = data_folder)
})


utils::globalVariables(
  c(
    "appdata",
    "clinsightful_data",
    "app_vars",
    "db_path",
    "metadata",
    "subject_id",
    "edit_date_time",
    "event_date",
    "event_name",
    "event_repeat",
    "form_repeat",
    "event_label",
    "item_group",
    "item_name",
    "item_value",
    "ID",
    "site_code",
    "reviewed",
    "DiscontinuationDate",
    "DiscontinuationReason",
    "status",
    "sex",
    "suffix",
    "timestamp",
    "Age",
    "ECOG",
    "Name",
    "Unit Other",
    "Unit",
    "form",
    "raw_data",
    "query_id",
    "needs_review",
    "reviewed",
    "review_required",
    "Sex",
    "var",
    "in_use",
    "lower_lim", 
    "lower_limit",
    "upper_lim", 
    "upper_limit",
    "out_of_lim",
    "n",
    "query",
    "type",
    "resolved",
    "reviewer",
    "CTCAE severity worsening",
    "Eligible", 
    "base_weight",
    "Frequency",
    "Frequency Other", 
    "LBCLSIG", 
    "LBORNR_Lower", 
    "LBORNR_Upper",
    "LBORRESU", 
    "LBORRESUOTH", "LBREASND", 
    "Route", "Route Other", 
    "SAE End date", 
    "SAE Start date", 
    "Serious Adverse Event", 
    "Start Date",
    "VAL", 
    "WHO.classification", 
    "all_data", 
    "apptables",
    "con", 
    "content",
    "day",
    "end date",
    "group", 
    "have_changed",
    "input", 
    "item", 
    "item_unit",
    "must_change", 
    "n_rows", 
    "new_query",
    "review_row",
    "significance", 
    "start date", 
    "style", 
    "unit", 
    "vars", 
    "vis_day",
    "event_id",
    "region",
    "suffix_names",
    "form_type",
    "id",
    "o_reviewed",
    "row_id",
    "start",
    "end",
    "className",
    "DrugAdminDate",
    "DrugAdminDose"
  )
)


