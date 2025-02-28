## Goal of the script: to adjust CSV test data so that unique event_ids will be used.
## This is needed to fix the even_id which is not unique in the raw data. 
## I believe this is not good practice, it should be unique for each event and 
## should be fixed in advance.
## When we make it unique, we can catch edge cases such as two different visits 
## being on the same day. 

###### 1. Update test data
devtools::load_all()
data_path <- app_sys("tests/testthat/fixtures/csvtestdata")
all_files <- list.files(data_path, pattern = ".csv")
raw_data <- lapply(setNames(nm = all_files), \(x){
  readr::read_delim(
    file.path(data_path, x),  
    delim = ",", 
    col_types = readr::cols(.default = readr::col_character()), 
    show_col_types = FALSE
  ) |> 
    dplyr::mutate(
      `Event Id` = dplyr::case_when(
        `Event Id` %in% c("VIS", "VISEXT") ~ paste0(`Event Id`, `Event sequence number`),
        .default = `Event Id`
      )
    )
})

#### Write results back to the files:
lapply(names(raw_data), \(x){
  readr::write_csv(raw_data[[x]], file.path(data_path, x))
})

##### 2. Update second test data
data_path <- app_sys("tests/testthat/fixtures/testapp-raw/data1pt")
all_files <- list.files(data_path, pattern = ".csv")
raw_data <- lapply(setNames(nm = all_files), \(x){
  readr::read_delim(
    file.path(data_path, x),  
    delim = ",", 
    col_types = readr::cols(.default = readr::col_character()), 
    show_col_types = FALSE
  ) |> 
    dplyr::mutate(
      `Event Id` = dplyr::case_when(
        `Event Id` %in% c("VIS", "VISEXT") ~ paste0(`Event Id`, `Event sequence number`),
        .default = `Event Id`
      )
    )
})
lapply(names(raw_data), \(x){
  readr::write_csv(raw_data[[x]], file.path(data_path, x))
})

###### Update clinsightful_data:
data_path <- app_sys("raw_data")
all_files <- list.files(data_path, pattern = ".csv")
raw_data <- lapply(setNames(nm = all_files), \(x){
  readr::read_delim(
    file.path(data_path, x),  
    delim = ",", 
    col_types = readr::cols(.default = readr::col_character()), 
    show_col_types = FALSE
  ) |> 
    dplyr::mutate(
      eventid = dplyr::case_when(
        eventid %in% c("VIS", "VISEXT") ~ paste0(eventid, eventseq),
        .default = eventid
      )
    )
})

#### Write results back to the files:
lapply(names(raw_data), \(x){
  readr::write_csv(raw_data[[x]], file.path(data_path, x))
})

### Verify changes:
#### vis_num and vis_day are removed, and event_id is updated, and the 
#### arrangement changed:
# df <- get_raw_csv_data(
#   app_sys("raw_data"), 
#   synch_time = "2023-09-15 10:10:00 UTC"
# ) |> 
#   merge_meta_with_data(meta = metadata) 
# waldo::compare(
#   df |> 
#     dplyr::arrange(subject_id, item_group, event_date, event_name) |> 
#     dplyr::select(-event_id), 
#   clinsightful_data |> 
#     dplyr::arrange(subject_id, item_group, event_date, event_name) |> 
#     dplyr::select(-vis_num, -vis_day, -event_id)
# )
# Result: ✔ No differences
