#' Create custom ClinSight metadata
#'
#' Small helper function to open a new metadata Excel file in a temporary
#' folder. It will attempt to open it. 
#' @param path Character string with destination path.
#' @param template_path Path to the metadata template.
#'
#' @return A ClinSight metadata (.xlsx) file.
#' @export
#' 
create_clinsight_metadata <- function(
    path = ".",
    template_path = app_sys("metadata.xlsx")
){
  meta_path <- file.path(path, "clinsight_metadata.xlsx")
  if(file.exists(meta_path)){
    stop("The file '", meta_path, "' already exists.",
         " Delete or rename this file and try again.")
  }
  file.copy(template_path, meta_path)
  message(
    "Creating a customizable ClinSight metadata file in the following location:\n", 
    meta_path, ".\n",
    "To use this file, save it in a different location, customize it, and ", 
    "use the functions `get_metadata()` and 'merge_meta_with_data()' to create ", 
    "metadata and study_data compatible with ClinSight."
  )
  file.show(meta_path)
}


#' Create custom ClinSight configuration file
#'
#' Small helper function to create a custom ClinSight configuration file in a
#' temporary folder. It will attempt to open it.
#'
#' @param path Character string with destination path.
#' @param template_path Path to the config template.
#'
#' @return A .yml configuration file.
#' @export 
#'
create_clinsight_config <- function(
    path = ".",
    template_path = app_sys("golem-config.yml")
){
  config_path <- file.path(path, "clinsight_config.yml")
  if(file.exists(config_path)){
    stop("The file '", config_path, "' already exists.",
         " Delete or rename this file and try again.")
  }
  file.copy(template_path, config_path, overwrite = FALSE)
  message(
    "Creating a customizable ClinSight config file in the following location:\n", 
    config_path, ".\n",
    "To use this config file, save it in a custom location and use \n",
    "    'Sys.setenv('CONFIG_PATH' = '", config_path, "')'\n", 
    "to specify the config file location so that ClinSight can find it."
  )
  file.show(config_path)
}
