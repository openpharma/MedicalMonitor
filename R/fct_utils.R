#' Time stamp
#' 
#' Simple wrapper around Sys.time to get a time stamp in 
#' unified timezone and format.
#'
#' @param form Format to be used for the time stamp. 
#' See also [base::strptime()] for the possible formats. 
#' @param timezone Timezone to use.
#'
#' @return A character string with time stamp in it.
#' @export
#'
#' @examples
#' time_stamp()
time_stamp <- function(
    form = "%Y-%m-%d %H:%M:%S %Z", 
    timezone = "UTC"
) {
  stopifnot(c(is.character(form), is.character(timezone)))
  stopifnot("timezone must be a valid R timezone. Check base::OlsonNames() for valid timezones." = 
              timezone %in% OlsonNames())
  ts <- format(Sys.time(), form, tz = timezone)
  # propagate errors from internal base R as.Date() function to check for valid 
  # function options:
  check_valid_date_format <- try(as.Date(ts), silent = TRUE)
  if(inherits(check_valid_date_format, "try-error")){
    stop(check_valid_date_format)
  }
  ts
}

#' Extract latest update time
#' 
#' Small helper function to extract the latest date-time from a data frame or 
#' a list of data frames.
#'
#' @param data data to look for the latest date time. 
#' Can be a data frame or a list of data frames.
#' @param col_name character vector with the column name in which 
#' to search for the date object. 
#'
#' @return A date-time object of length 1.
#' @export
#'
#' @examples 
#' # example data:
#' dfs <- lapply(1:10, \(x){data.frame(
#'   "edit_date_time" = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), 12)
#'   )})
#' get_max_time(dfs, "edit_date_time")
#'  
get_max_time <- function(
    data, 
    col_name = "edit_date_time"
){
  stopifnot(inherits(data, c("list", "data.frame")))
  stopifnot(is.character(col_name))
  stopifnot(length(col_name) == 1)
  
  if(is.data.frame(data)) { 
    dates <- as.character(data[[col_name]])
  } else if(is.list(data)){ 
    dates <- unlist(lapply(data, \(x){as.character(x[[col_name]])}), use.names = FALSE)
  } 
  if(length(dates) == 0 ) stop("No dates found in data. Does the column ", 
                               col_name, " exist?")
  dates <- na.omit(as.POSIXct(dates, tz = "UTC"))
  if(length(dates) == 0) stop(
    "No non-missing dates found. ", 
    "Is the date-time provided in the right format?"
  )
  max(dates, na.rm = TRUE)
}

#' Expand columns
#' 
#' Function to expand list columns into additional rows.
#'
#' @param data A data frame to use.
#' @param columns Character vector containing the names of the columns that will be used for the expansion of the data frame.
#' @param separator 
#' Character vector, containing the separator that is used to distinguish 
#' different elements in the list columns. 
#' @param unite_with 
#' Character string. Name of 
#' @param remove_cols A logical, indicating if the columns need to be removed 
#' from the data set after expansion. Will be ignored if `unite_with` is `NULL`, 
#' to ensure that the information in `columns` is not lost. Can be a vector of logicals if multiple `columns` are used for expansion. 
#'
#' @return A data frame that usually will be longer than the original one.
#' @export
#' 
#' @examples
#'  df <- head(iris, n = 6) |> 
#'   dplyr::mutate(
#'     expansion_1 = list("feature.1, feature.2"), 
#'     expansion_2 = list("group.1, group.2")
#'     )
#'     
#'  # expand the data frame with the values in columns "expansion_1" and "expansion_2":
#'   expand_columns(df, c("expansion_1", "expansion_2"))
#' 
#' # You can also unite the values in the expansion columns with any column in the data frame, 
#' # and remove column "expansion_1" but keep column "expansion_2" afterwards:
#' expand_columns(df, c("expansion_1", "expansion_2"),  
#'   unite_with = "Species", remove_cols = c(TRUE, FALSE))
#' 
expand_columns <- function(
    data, 
    columns,
    separator = ",",
    unite_with = NULL,
    remove_cols = FALSE
){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(columns))
  stopifnot(is.character(separator))
  stopifnot(is.null(unite_with) || is.character(unite_with))
  stopifnot(is.logical(remove_cols))
  
  expand.names <- names(data)[names(data) %in% columns]
  df <-  data |> 
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(columns), 
        function(x){
          ifelse(
            is.na(x), 
            NA,
            strsplit(gsub("[[:space:]]", "", x), split = separator)
          )
        }
      )
    )  
  for(i in expand.names){
    df <- tidyr::unnest(df, dplyr::all_of(i)) 
    if(!is.null(unite_with)){
      remove_i <- ifelse(length(remove_cols) == 1, remove_cols, 
                         remove_cols[match(i, expand.names)])
      df <- tidyr::unite(
        df, 
        col = {{unite_with}}, 
        dplyr::all_of(c(unite_with, i)), 
        sep = "_",
        remove = remove_i, 
        na.rm = TRUE
      )
    }
  }
  df
}

#' Capitalize words
#' 
#' Convert sentence to upper case to capital words
#'  
#' Adapted from the base R toupper() function help page.
#'
#' @param x Character string.
#'
#' @return A character string.
#' @export
#'
#' @examples 
#' title_case("this will be converted to upper case")
#' # [1] "This Will Be Converted To Upper Case"
#' 
title_case <- function(x) {
  stopifnot(is.character(x))
  out <- character(length = length(x))
  s <- strsplit(x, " ")
  lapply(s, \(x){
    if(all(is.na(x))) return(NA_character_)
    paste0(
      toupper(substring(x, 1, 1)), 
      tolower(substring(x, 2)),
      collapse = " "
    ) }) |> 
    unlist()
}


#' Create Unique ID
#' 
#' Creates a unique Identifier. Adapted from `crosstalk:::createUniqueId()` since
#' it is not an exported function. This id needs to be refreshed at certain times 
#' to ensure that the crosstalk widgets stay connected (see also 
#'  [here](https://github.com/rstudio/crosstalk/issues/132#issue-1393291183)).
#'
#' @param bytes An integer. Controls the size of the unique id.
#'
#' @return A unique character vector.
#' @export
#'
#' @examples
#' create_unique_id(5)
create_unique_id <- function (
    bytes = 5
) {
  stopifnot(is.numeric(bytes))
  paste(
    format(as.hexmode(sample(256, round(bytes, digits = 0), 
                             replace = TRUE) - 1), width = 2), 
    collapse = ""
  )
}

#' Simplify String
#' 
#' Converts readable names to clean ids that are easier to handle in R. 
#' Replaces all punctuation and spaces for an underscore, and transforms all 
#' uppercase letters to lowercase.
#'
#' @param x a character vector or a factor. Factors will be silently converted 
#' to character strings.
#'
#' @return character vector of the same length as the input vector.
#' @export
#'
#' @examples simplify_string(c(" Dirty_name. to Clean.#$", "#$another   complicated. Name"))
simplify_string <- function(x){
  if(is.factor(x)) x <- as.character(x)
  stopifnot(is.vector(x))
  stopifnot("character" %in% class(x))
  x <- tolower(trimws(gsub("[[:punct:]]+", " ", x)))
  gsub(" +", "_", x)
}


#' Order string
#' 
#' Small helper function to define the proper order per individual/site, to 
#' arrange the sites/individuals properly within the application.
#' Arranges first on all string letters, and then on the numbers.
#'
#' @param string Character string to be used. 
#'
#' @return An ordered string.
#' @export
#'
#' @examples
#' order_string(rownames(mtcars))
#' 
order_string <- function(
    string = appdata$`CBC regular`$subject_id
){
  stopifnot(is.vector(string))
  if(!is.character(string)) warning("Non-character vector used as input. 
                                    The input will be converted to character.")
  string <- unique(as.character(string))
  df <- data.frame(string = string)
  df$string_letters <- regmatches(string, gregexpr("[[:alpha:]]", string)) |> 
    sapply(paste0, collapse = "")
  df$string_numbers <- regmatches(string, gregexpr("[[:digit:]]", string)) |> 
    sapply(paste0, collapse = "") |> as.numeric()
  df <- dplyr::arrange(df, .data[["string_letters"]], .data[["string_numbers"]])
  df$string
}


#' Get unique vars
#'
#' Get all unique values from variables that are available within a data frame
#' or a list of dataframes.
#'
#' @param data Either a list of data frames or a data frame.
#' @param var Column names to extract the unique variables from
#'
#' @return A data frame with unique values.
#' @export
#'
#' @examples
#'  list_with_data <- list(
#'   data.frame(
#'     "Sites" = c("site_1", "site_3", "site_4"),
#'     "ID" = c("subj_1", "subj_5", "subj_8")
#'     ),
#'  data.frame(
#'     "Sites" = c("site_1", "site_3", "site_9"),
#'     "ID" = c("subj_7", "subj_9", NA_character_)
#'     )
#'  )
#'
#'  get_unique_vars(list_with_data, c("Sites", "ID"))
#' 
get_unique_vars <- function(data, var
){
  stopifnot(is.list(data), is.character(var))
  var <- var[nchar(var)>0]; stopifnot("No variable names provided" = length(var)>0)
  if(is.data.frame(data)) return({unique(add_missing_columns(data, var)[var])})
  lapply(data, \(x){
    for(i in var){
      x[[i]] <- if(i %in% names(x)) as.character(x[[i]]) else NA_character_
    }
    x[var]
  }) |> 
    dplyr::bind_rows() |> 
    unique()
}


#' Custom row bind
#' 
#' Small wrapper around the function `dplyr::bind_rows()`. 
#' Converts first a custom amount of variables in a list of data frames to 
#' character, and then combines the list of data frames into one data frame.
#'
#' @param data A list of data frames.
#' @param convert A character vector containing the column names of the columns 
#' that need to be converted.
#'
#' @return A data frame object.
#' @export
#' 
#' @examples
#'  data_list <- list(
#'    data.frame(
#'       "ID" = c("subj_1", "subj_5", "subj_8"),
#'       "values" = c("1", "4", "7")
#'    ),
#'    data.frame(
#'       "ID" = c("subj_7", "subj_9", NA_character_),
#'       "values" = c(10, 12, 5)
#'    )
#'  )
#' bind_rows_custom(data_list, "values")
bind_rows_custom <- function(
    data, 
    convert = "item_value"
){
  stopifnot("input should be a list with data frames" = inherits(data, "list"))
  lapply(data, \(x){
    x <- add_missing_columns(x, convert)
    for(i in convert){
      x[[i]] <- as.character(x[[i]])
    }
    x
  }) |> 
    dplyr::bind_rows()
}


#' Collapse Factor levels
#'
#' Small helper function to collapse factors using a Base R syntax
#'
#' @param x a character string or factor
#' @param new_levels a named list of levels that need to be relabeled. They
#'   should be character values. If no names are provided, the first element
#'   will be chosen as the factor label.
#' @param warn_non_existent_levels A logical, whether a warning should be given
#'   if levels are not existent.
#'
#' @return a factor with newly calculated levels
#' @export
#'
#' @examples
#' collapse_fct_levels(
#' x = LETTERS,
#' new_levels = list("A-C" = c("A", "B", "C"))
#' )
collapse_fct_levels <- function(
    x = appdata$ECG$status,
    new_levels = list("old" = "old", "new" = c("new", "updated")),
    warn_non_existent_levels = FALSE
){
  stopifnot(inherits(x, c("character", "factor")))
  stopifnot(is.list(new_levels))
  stopifnot(
    "all elements in new_levels should be of class character." = 
      all(sapply(new_levels, is.character))
  )
  if(!all(is.na(x)) && all(x == "")) {warning("empty string provided")}
  x <- factor(x)
  all_lvls <- unlist(new_levels, use.names = FALSE)
  non_existing_lvls <- all_lvls[!all_lvls %in% levels(x)]
  
  if(warn_non_existent_levels && length(non_existing_lvls) != 0){
    warning("the following levels are currently missing in the vector: ", 
            non_existing_lvls)
  }
  non_defined <- levels(x)[!levels(x) %in% all_lvls]
  if(!length(non_defined) == 0){
    names(non_defined) <- non_defined
    new_levels <- append(new_levels, as.list(non_defined))
  }
  names(new_levels) <- unlist(lapply(seq_along(new_levels), \(x){
    name_x <- names(new_levels)[x] 
    if(name_x == "") name_x <- new_levels[[x]][1]
    name_x
  }))
  levels(x) <- new_levels
  x
}



#' Verify date class
#'
#' Small helper function to verify whether an object is of class Date or
#' POSIXct.
#'
#' @param x An atomic object (a vector).
#'
#' @return a logical of length one.
#' @export
#'
#' @examples is_date(as.Date("2023-09-03"))
is_date <- function(x) {
  stopifnot(is.atomic(x))
  inherits(x, c("Date", "POSIXt"))
}

#' Convert non-numeric columns to character
#'
#' @param data A data frame with columns that need to be converted.
#'
#' @return A data frame with all non-numeric columns converted to character.
#' @export
#'
#' @examples
#' df <- cols_to_char(iris)
#' class(df$Species)
cols_to_char <- function(data){
  stopifnot(is.data.frame(data))
  lapply(data, \(x){ 
    if(is.numeric(x)) x else as.character(x)
  }) |> 
    as.data.frame()
}

#' Convert date columns to character
#' 
#' Helpful for when a date object needs to be printed or stored in a SQL 
#' database, since SQL databases do not (always) support date-time classes 
#' used in R. 
#'
#' @param data A data frame.
#'
#' @return A data frame with all columns with the class `Date` or `POSIXct` 
#' converted to character.
#' @export
#'
#' @examples
#' df <- iris
#' df$dates <- Sys.Date() + sample(1:1000, size = nrow(iris))
#' converted_df <- date_cols_to_char(df)
#' # the date variable is now converted, while all other columns are unchanged:
#' sapply(converted_df, class)
#' 
date_cols_to_char <- function(data){
  stopifnot(is.data.frame(data))
  list_data <- lapply(data, \(x){ 
    if(is_date(x)) as.character(x) else x 
  })  
  dplyr::bind_rows(list_data)
}


#' Clean dates
#'
#' This helper function replaces a pattern of "NK" in months and in days in a
#' date, with the first day or month. The function expects that the pattern of
#' the date is 'YYYY-MM-DD'.
#'
#' @param x A character or date vector. if the input is already a date vector,
#'   the input will be returned unchanged.
#' @param unknown_pattern Pattern to look up in the date, standard "NK". Note
#'   that the pattern will gain a prefix of "-", to avoid that years with the
#'   tag `unknown_pattern` tab will be replaced. Dates containing years with the
#'   `unknown_pattern` tag will become missing.
#' @param unknown_replacement Replacement of the unknown pattern. Standard 01
#'   (first day/month).
#'
#' @return A `date`-class vector.
#' @export
#'
#' @examples
#' clean_dates(c("2023-08-NK", "2023-NK-NK", "NK-08-01"))
clean_dates <- function(
    x,
    unknown_pattern = "NK",
    unknown_replacement = "01"
){
  stopifnot(is.character(x) | is_date(x))
  if(is_date(x)) return(x)
  gsub(
    paste0("-", unknown_pattern[1]), 
    paste0("-", unknown_replacement[1]), 
    x
  ) |> 
    as.Date()
}

#' Select from vector
#'
#' Small helper function to select values from a character vector, based on
#' regular expressions.
#'
#' @param x A character vector.
#' @param include Optional. If provided, will only include vector elements that
#'   match this regular expression.
#' @param exclude Optional. If provided, will exclude vector elements that match
#'   this regular expression.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' vector_select(names(iris), include = "Sepal", exclude = "Width")
#' 
vector_select <- function(
    x,
    include = NULL,
    exclude = NULL
){
  if(is.null(x)) return(character(0))
  stopifnot(is.character(x))
  stopifnot(is.null(include) || is.character(include))
  stopifnot(is.null(exclude) || is.character(exclude))
  if(!is.null(include)){
    x <- x[grepl(paste(include, collapse = "|"), x)]
  }
  if(!is.null(exclude)){
    x <- x[!grepl(paste(exclude, collapse = "|"), x)]
  }
  x
}


#' Get package test results
#'
#' Creates package results and wraps it in a list, together with a timestamp at
#' which the results were created and the current sessionInfo.
#'
#' @param package Character vector with the poackage name.
#' @param reporter a `testthat` file reporter. defaults to
#'   `testthat::ListReporter`.
#' @param outfile If provided, the results will be saved in a .rds file on the
#' @param run_as_not_cran Logical. If TRUE, will provide a temporary
#'   environmental variable NOT_CRAN='true' to ensure that tests are not
#'   skipped.
#' @return A list with the outcome. If `outfile` is provided, the result will
#'   also be saved in the location of `outfile`.
#' @export
#' 
get_test_results <- function(
    package = "clinsight", 
    reporter = testthat::ListReporter,
    outfile = NULL,
    run_as_not_cran = TRUE
){
  stopifnot(is.null(outfile) || tools::file_ext(outfile) == "rds")
  if(!is.null(outfile)){
    if(!dir.exists(dirname(outfile))){
      cat("Outfile directory not found. Creating new directory\n")
      dir.create(dirname(outfile))
    }
    if(file.exists(outfile)) {
      warning(outfile, " exists already and will be overwritten.")
    }
  }
  stopifnot(dir.exists(dirname(outfile)))
  
  if(run_as_not_cran){
    withr::local_envvar(NOT_CRAN = "true")
  }
  
  test_results_raw <- testthat::test_package(
    package = package, 
    reporter = reporter,
    stop_on_failure = FALSE,
    stop_on_warning = FALSE
  )
  cat("\n\n----------------------------\n\nFinished unit testing. Results: \n")
  test_results <- format_test_results(test_results_raw)
  
  if(is.null(outfile)) return(test_results)
  saveRDS(test_results, outfile)
  if(file.exists(outfile)){
    cat("Output created in ", outfile, "\n") 
  }
  cat("\n----------------------------\n\n")
}

#' Format testthat results
#'
#' Helper function to reshape original testthat output into a list format  with
#' the testthat tests output, and some metadata to make it easier to
#' interpretate the results.
#'
#' @param results An object of class `testthat_results`, created with one of the
#'   `testthat::test_*` functions.
#'
#' @return A list with test results and metadata containing information such as
#'   a summary of the tests and their outcome, a timestamp, and the results of
#'   utils::sessionInfo() of the environment in which the tests ran.
#' @noRd
#' 
format_test_results <- function(
    results
){
  if(isFALSE(inherits(results, "testthat_results"))){
    stop("Expecting an object of class 'testthat_results'")
  }
  test_results <- list(
    "results" = results,
    "time"    = time_stamp(form = "%Y-%m-%d %H:%M:%S %Z", timezone = "UTC"),
    "session" = utils::sessionInfo(),
    "sum_results" = "",
    "test_outcome" = ""
  )
  # Summary is nice to have but should not give a fatal error: 
  tryCatch({
    # TODO: maybe import testthat:::as.data.frame.testthat_results?
    test_df <- as.data.frame(test_results$results)
    sum_results <- sapply(test_df[c("failed", "skipped", "error", "warning", "passed")], sum)
    print(sum_results)
    test_results[["sum_results"]] <- sum_results
    test_outcome <- ifelse(isTRUE(all_tests_passed(test_results)), "pass", "fail")
    test_results[["test_outcome"]] <-  test_outcome
  },
  error = function(x) "Could not summarize results. Verify results manually"
  )
  tryCatch({
    if(identical(test_outcome, "pass")) {
      cat("All tests passed successfully\n")
    } else{
      warning("Not all tests passed successfully. Verify the outcome.")
      failed_tests <- with(test_df, file[failed != 0])
      if(length(failed_tests) != 0){
        cat(
          "There was a failure in the following tests: \n", 
          paste0(failed_tests, collapse = "\n"), 
          "\n",
          sep = ""
        )
        cat("Failure messages: \n\n")
        res <- unlist(with(test_df, result[file %in% failed_tests]), recursive = FALSE) 
        lapply(res, \(x){if(expectation_type(x, "failure")) x[]}) |> 
          unlist() |> 
          cat(sep = "\n\n") 
      }
    }
  },
  error = function(x) "Could not print problematic results. Verify results manually"
  )
  test_results
}

#' All tests passed
#'
#' Helper function to determine whether all tests passed. Designed to be used
#' together with get_test_results.
#'
#' @param results A list with test results. Should contain `var`
#' @param var A character vector with the name of the list element that contains
#'   the summary results.
#' @param include_skipped Boolean. Whether to check if no tests were skipped.
#'
#' @return A boolean.
#' @noRd
#' 
all_tests_passed <- function(results, var = "sum_results", include_skipped = TRUE){
  stopifnot("results needs to be a list" = inherits(results, "list"))
  if(!var %in% names(results)){
    warning(paste0(var, " is missing from the results. Cannot determine if all tests passed"))
    return(NA)
  }
  res <- results[[var]]
  cols_to_verify <- c("failed", "error", "warning")
  if(include_skipped) cols_to_verify <- c(cols_to_verify, "skipped")
  (sum(res[cols_to_verify]) == 0 ) && res[["passed"]] != 0
}

#' Verify expectation type
#'
#' Helper function to verify expectation type.
#'
#' @param exp The expection to verify
#' @param type Character string with the expected type
#'
#' @return A boolean
#' @noRd
expectation_type <- function(
    exp, 
    type = c("failure", "error", "skip", "warning", "success")
) {
  stopifnot(testthat::is.expectation(exp))
  type <- match.arg(type)
  identical(gsub("^expectation_", "", class(exp)[[1]]), type)
}


#' Custom config path
#'
#' Note: this is a temporary solution, to circumvent the issue described here:
#' https://github.com/ThinkR-open/golem/issues/1178#issue-2513219365. It ensures
#' that a flexible path to the config file can be set by the user. Works with
#' golem version 0.5.1.
#'
#' @return A path to the active config file to use for the application.
#' @noRd
#' 
custom_config_path <- function(
){
  Sys.getenv("CONFIG_PATH", app_sys("golem-config.yml")) 
}

dblclick_to_form <- function(bttn_ns) {
  DT::JS(
    "table.on('dblclick', 'tbody tr', function(t) {",
    # This processing assumes `server = TRUE`
    "var current_index = table.row(this).index();",
    "var current_rows = table.ajax.json().DT_rows_current;",
    "table.shinyMethods.selectRows(current_rows[current_index]);",
    "document.getElementById(", deparse(NS(bttn_ns, "go_to_form")), ").click();",
    "})"
  )}

#' Decode a base64 encoded string.
#'
#' Used to decode base64-encoded HTTP headers. Encoding these headers
#' ensures that they can contain complex names with special characters.
#'
#' The function will warn if the decoded output string is no valid UTF-8. This
#' might occur if the input was not base64 encoded.
#'
#' @param x A base64 encoded character string.
#'
#' @return A decoded character string.
#' @noRd
#'
#' @examples
#' encoded_username <- base64enc::base64encode(charToRaw("Ťěšťůšěř 的"))
#' decode_base64(encoded_username)
#' 
decode_base64 <- function(
    x
){
  stopifnot("base64enc is not installed" = rlang::is_installed("base64enc"))
  if(is.null(x)) return(x)
  stopifnot(is.character(x))
  decoded <- base64enc::base64decode(x)
  valid_UTF8 <- base64enc::checkUTF8(decoded, quiet = TRUE)
  
  if(isFALSE(valid_UTF8)){
    warning(
      "decoded string does not contain valid UTF-8. ",
      "Is the input really base64 encoded?"
    )
    # using deparse creates visible quotes around the string but doing so will
    # prevent the app from breaking due to incorrect UTF-8 encoding:
    return({deparse1(rawToChar(decoded))})
  }
  rawToChar(decoded)
}

split_review_data <- function(db_path, forms) {
  all_review_data <- db_get_table(db_path = db_path, db_table = "all_review_data")

  if (missing(forms)) {
    split(all_review_data, all_review_data$item_group)
  } else {
    split(all_review_data, factor(all_review_data$item_group, forms))
  }
}
