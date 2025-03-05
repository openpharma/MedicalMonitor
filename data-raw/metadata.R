## code to prepare metadata. Raw data is expected to be in Excel format.
devtools::load_all(".")

metadata <- get_metadata(filepath = app_sys("metadata.xlsx"))

usethis::use_data(metadata, overwrite = TRUE)
