pkg_name <- "clinsight"
library(pkg_name, character.only = TRUE)

raw_data_folder <- "data1pt"
meta_path <- "altered_metadata.xlsx"

if(rlang::is_interactive()){
  raw_data_folder <- app_sys("tests/testthat/fixtures/testapp-raw/data1pt")
  meta_path <- testthat::test_path("fixtures/testapp-raw/altered_metadata.xlsx") 
}

load_and_run_app <- function(){
  temp_folder <- tempfile(tmpdir = tempdir())
  dir.create(temp_folder)
  old_golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE")
  Sys.setenv("GOLEM_CONFIG_ACTIVE" = "test")
  meta <- get_metadata(meta_path)
  merged_data <- raw_data_folder |> 
    get_raw_csv_data(synch_time = "2024-01-01 00:00:00") |> 
    merge_meta_with_data(meta = meta)
  saveRDS(merged_data, file.path(temp_folder, "study_data.rds"))
  saveRDS(meta, file.path(temp_folder, "metadata.rds"))
  
  run_app(
    data_folder = temp_folder,
    onStart = \(){onStop(\(){
      unlink(temp_folder, recursive = TRUE); 
      Sys.setenv("GOLEM_CONFIG_ACTIVE" = old_golem_config)
    })}
  )
} 

load_and_run_app()
