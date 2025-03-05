#' Run the Shiny Application
#' 
#' @param data_folder Character string. The folder in which all data resides is
#'   usually set in the config.yml file. However, this can be overwritten if a
#'   path is set in this argument. Useful for testing purposes.
#' @param credentials_pwd Character string with the credentials' database
#'   password.
#' @param ... arguments to pass to golem_opts. See `?golem::get_golem_options`
#'   for more details.
#' @inheritParams shiny::shinyApp
#' 
#' 
#'
#' @export
#' 
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    data_folder = NULL,
    credentials_pwd = Sys.getenv("DB_SECRET"),
    ...
) {
  
  data <- get_golem_config("study_data")
  meta <- get_golem_config("meta_data")
  user_db <- get_golem_config("user_db")
  use_shinymanager <- isTRUE(get_golem_config("user_identification") == "shinymanager")
  credentials_db <- get_golem_config("credentials_db")
  
  if(!is.null(data_folder)){
    if(!dir.exists(data_folder)) dir.create(data_folder) 
    if(!dir.exists(data_folder)){
      stop("Folder path '", data_folder, "' specified but cannot be created\n")
    }
    if(is.character(data)) data <- file.path(data_folder, data)
    if(is.character(meta)) meta <- file.path(data_folder, meta)
    user_db <-  file.path(data_folder, user_db)
    credentials_db <- file.path(data_folder, credentials_db)
  }
  
  ## Verify study data
  if(is.character(data)){
    if(!file.exists(data)) stop(paste0("Cannot find '", data, "'."))
    if(tolower(tools::file_ext(data)) != "rds"){
      stop("Invalid data format. Expecting a file .rds format")
    }
    data <- readRDS(data)
  } 
  stopifnot("Expecting study data to be in data frame format." = is.data.frame(data))
  
  ## Verify metadata
  if(is.character(meta)){
    if(!file.exists(meta)) stop(paste0("Cannot find metadata ('", meta, "')."))
    if(tolower(tools::file_ext(meta)) != "rds") {
      stop("Only metadata files of type '.rds' are allowed.")
    }
    meta <- readRDS(meta)
  }
  stopifnot("Expecting metadata to be in a list format" = inherits(meta, "list"))
  
  ## Verify user database
  stopifnot("user_db should be a character vector with a file path" = 
              is.character(user_db))
  if(!file.exists(user_db)){
    warning("No user database found. New database will be created")
    db_create(get_review_data(data), db_path = user_db)
  } else{
    stopifnot("user_db version is not up to date" =
                identical(db_version, db_get_version(user_db)))
    # Skip if not needed for faster testing:
    if(isTRUE(get_golem_config("app_prod"))){
      db_update(get_review_data(data), db_path = user_db) 
    }
  }
  
  ## Verify credentials database, if applicable
  if(use_shinymanager){
    rlang::check_installed(
      "shinymanager", 
      reason = "to use it for authentication management in ClinSight."
      )
    stopifnot("credentials_db should be a character vector with a file path" = 
                is.character(credentials_db))
    stopifnot("No valid credentials database pwd provided" = is.character(credentials_pwd))
    if(nchar(credentials_pwd) == 0 ) stop("credentials_pwd cannot be blank when using shinymanager")
    initialize_credentials(
      credentials_db = credentials_db,
      credentials_pwd = credentials_pwd
    )
    shinymanager::set_labels(
      language = "en",
      "Please authenticate" = "Login to continue"
    )
    options("shinymanager.pwd_validity" = 90) 
    options("shinymanager.pwd_failure_limit" = 5)
  }
  
  with_golem_options(
    app = shinyApp(
      ui =  if(use_shinymanager) authenticate_ui() else app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      meta = meta,
      data = data,
      user_db = user_db,
      credentials_db = credentials_db,
      credentials_pwd = credentials_pwd,
      ...
    )
  )
}
