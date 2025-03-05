
#' Build manual custom
#'
#' This is a tweaked function of devtools::build_manual() that will throw
#' informative errors when it fails to create a PDF manual. The change was
#' needed because of  an issue of devtools::build_manual() under windows (see
#' [here](https://github.com/r-lib/devtools/issues/2478#issue-1410219572)).
#'
#' @param pkg
#' @param path
#'
#' @noRd
build_manual_custom <- function (pkg = ".", path = NULL){
  pkg <- as.package(pkg)
  path <- path %||% path_dir(pkg$path)
  name <- paste0(pkg$package, "_", pkg$version, ".pdf", collapse = " ")
  tryCatch(msg <- callr::rcmd(
    "Rd2pdf", 
    cmdargs = c(
      "--force", 
      paste0("--output=", path, "/", name), 
      pkg$path
    ), 
    fail_on_status = TRUE, 
    stderr = "2>&1", 
    spinner = FALSE), 
    error = function(e) { print(e); cli::cli_abort("Failed to build manual")}
    )
  cat(msg$stdout)
  invisible(msg)
}

#library(devtools)
#library(fs)
#library(rlang)

#Sys.setenv(PATH=paste(Sys.getenv("PATH"),"C:/Users/lsamson/AppData/Roaming/TinyTeX/bin/windows/",sep=";"))
#build_manual_custom()
