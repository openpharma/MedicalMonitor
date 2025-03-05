pkg_name <- "clinsight"
library(pkg_name, character.only = TRUE)

load_and_run_app <- function(){
  # Note 1: withr call here does not work since the tempfile gets deleted during app initialization. 
  # different option below, using onStop() within the run_app() function to clean up.
  # Note 2: Creating a temp folder with a random name within the main tempdir 
  # ensures that we can safely unlink the folder in interactive mode, without 
  # deleting temporary files from (for example) renv. Inspired by 
  # withr::with_tempdir().
  temp_folder <- tempfile(tmpdir = tempdir())
  dir.create(temp_folder)
  old_golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE")
  Sys.setenv("GOLEM_CONFIG_ACTIVE" = "test")
  # Mimic data loading in production version: 
  saveRDS(clinsightful_data, file.path(temp_folder, "study_data.rds"))
  saveRDS(metadata, file.path(temp_folder, "metadata.rds"))  
  file.copy("../testdb.sqlite", file.path(temp_folder, "user_db.sqlite"))
  
  run_app(
    data_folder = temp_folder,
    onStart = \(){onStop(\(){
      unlink(temp_folder, recursive = TRUE); 
      Sys.setenv("GOLEM_CONFIG_ACTIVE" = old_golem_config)
      })}
  )
} 

load_and_run_app()
