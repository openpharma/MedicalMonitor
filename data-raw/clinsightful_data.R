devtools::load_all(".")

clinsightful_data <- clinsight::get_raw_csv_data(
  app_sys("raW_data"), 
  synch_time = "2023-09-15 10:10:00 UTC"
) |> 
  merge_meta_with_data(metadata)

usethis::use_data(clinsightful_data, overwrite = TRUE)