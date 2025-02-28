describe("get_form_table() works", {
  it(
    "creates a form table with not yet reviewed data marked bold and with the 
    o_reviewed list stored in it,in which only the rows for the active_subject 
    are set to disabled=FALSE", 
    {
      appdata <- get_appdata(clinsightful_data)
      table_names <- setNames(nm = names(appdata))
      form_items <- get_meta_vars(appdata)$items
      rev_data <- lapply(table_names, \(x){
        appdata[[x]] |> 
          get_review_data() |> 
          dplyr::mutate(
            id = dplyr::row_number(),
            # force data of BEL_04_772 as being not reviewed
            # Using two ids since there is no id that is available in all forms
            reviewed = ifelse(subject_id %in% c("BEL_04_772", "NLD_06_755"), "No", "Yes"),
            status = ifelse(reviewed == "No", "new", "old")
          )  
      })
      
      output <- lapply(table_names, \(x){
        get_form_table(
          form_data = appdata[[x]], 
          form_review_data = rev_data[[x]], 
          form = x,
          form_items = form_items[[x]],
          active_subject = "BEL_04_772", 
          is_reviewed = TRUE
        )
      })
      
      lapply(table_names, \(x){
        expect_true(is.data.frame(output[[!!x]]))
        expect(
          "o_reviewed" %in% names(output[[x]]), 
          paste0("`o_reviewed` is an expected column for data in form `", x, "`.")
        )
        expect_true(is.list(output[[!!x]][["o_reviewed"]]))
        enabled_rows <- lapply(output[[x]][["o_reviewed"]], \(i) isFALSE(i$disabled)) |> unlist()
        if(any(enabled_rows)){
          expect_equal(unique(output[[!!x]][enabled_rows,]$subject_id), "BEL_04_772")
        } else{
          expect_true(!"BEL_04_772" %in% unique(output[[!!x]]$subject_id))
        }
      }) |> 
        invisible()
      
      ## Verify columns for each table
      standard_names <- c("o_reviewed", idx_cols, "event_repeat", "event_date")
      review_tables <- table_names[table_names != "General"]
      unreviewed_items <- lapply(review_tables, \(x){
        df_x <- output[[x]]
        col_to_check <- names(df_x)[!names(df_x) %in% standard_names][1]
        df_x[df_x$subject_id %in% c("BEL_04_772", "NLD_06_755"),][[col_to_check]]
      })
      lapply(review_tables, \(x) {
        expect_true(all(grepl("<b>", unreviewed_items[[!!x]])))
      }) |> 
        invisible()
    })
  
  it(
    "creates a form table similar to the on created with create_table when 
    ignoring changes for AE/SAE tables made with adjust_ae_form_table and 
    ignoring the bold tags for not yet reviewed data", 
    {
      appdata <- get_appdata(clinsightful_data)
      table_names <- setNames(nm = names(appdata))
      form_items <- get_meta_vars(appdata)$items
      rev_data <- lapply(table_names, \(x){
        appdata[[x]] |> 
          get_review_data() |> 
          dplyr::mutate(
            id = dplyr::row_number(),
            # force data of BEL_04_772 as being not reviewed
            # Using two ids since there is no id that is available in all forms
            reviewed = ifelse(subject_id %in% c("BEL_04_772", "NLD_06_755"), "No", "Yes"),
            status = ifelse(reviewed == "No", "new", "old")
          )  
      })
      ##### ignore AE/SAE column adjustments in this test:
      testthat::local_mocked_bindings(
        adjust_ae_form_table = function(data, is_SAE){data}
      )
      output <- lapply(table_names, \(x){
        get_form_table(
          form_data = appdata[[x]], 
          form_review_data = rev_data[[x]], 
          form = x,
          form_items = form_items[[x]],
          active_subject = "BEL_04_772", 
          is_reviewed = TRUE
        )
      })
      
      output_tables <- lapply(table_names, \(x){
        output[[x]] |> 
          dplyr::select(-o_reviewed) |> 
          dplyr::mutate(
            dplyr::across(
              # remove bold tags. status_label is the exception here since it 
              # is expected to have html tags, including the tag for a bold font.
              dplyr::where(is.character) & !dplyr::any_of("status_label"), 
              \(x) gsub("\\**<\\/*b>", "", x)
            )
          )
      })
      original_tables <- lapply(table_names, \(x){
        create_table(appdata[[x]], expected_columns = names(form_items[[x]])) 
      })
      lapply(table_names, \(x){
        expect_equal(output_tables[[!!x]], original_tables[[!!x]])
      })
    }
  )
  it("errors with unexpected input", {
    appdata <- subset(clinsightful_data, item_group == "Adverse events") |> 
      get_appdata()
    form_items <- get_meta_vars(appdata)$items[["Adverse events"]]
    rev_data <- appdata[["Adverse events"]] |> 
      get_review_data() |> 
      dplyr::mutate(
        id = dplyr::row_number(),
        # force data of BEL_04_772 as being not reviewed
        # Using two ids since there is no id that is available in all forms
        reviewed = ifelse(subject_id %in% c("BEL_04_772", "NLD_06_755"), "No", "Yes"),
        status = ifelse(reviewed == "No", "new", "old")
      )
    args <- list(
      form_data = appdata[["Adverse events"]], 
      form_review_data = rev_data, 
      form = "Adverse events",
      form_items = form_items,
      active_subject = "BEL_04_772", 
      is_reviewed = TRUE
    )
    change_args <- function(x){modifyList(x = args, val = x)}
    expect_error(
      do.call("get_form_table", change_args(list(form_data = "incorrect")))
    )
    expect_error(
      do.call("get_form_table", change_args(list(form_review_data = "incorrect")))
    )
    expect_error(
      do.call("get_form_table", change_args(list(form = data.frame())))
    )
    expect_error(
      do.call("get_form_table", change_args(list(form_items = data.frame())))
    )
    expect_error(
      do.call("get_form_table", change_args(list(active_subject = data.frame())))
    )
    expect_error(
      do.call("get_form_table", change_args(list(is_reviewed = "incorrect")))
    )
    incorrect_data <- args
    incorrect_data[["form_data"]] <- mtcars
    expect_error(
      do.call("get_form_table",  incorrect_data),
      paste0("the following columns are missing: subject_id, event_name, ",
             "item_group, form_repeat, item_name, edit_date_time, event_date, item_value")
    )
    incorrect_data <- args
    incorrect_data["active_subject"] <- list(NULL)
    expect_warning(
      do.call("get_form_table", incorrect_data),
      "No active subject selected"
    )
  })
})

describe("adjust_ae_form_table() works as expected", {
  it("creates an AE table with only Adverse Events, no SAEs, and the columns starting with prefix SAE removed", {
    appdata <- clinsightful_data[clinsightful_data$item_group == "Adverse events", ] |> 
      get_appdata()
    form_items <- get_meta_vars(appdata)$items[["Adverse events"]]
    ae_table <- create_table(
      appdata[["Adverse events"]], 
      expected_columns = names(form_items)
      )
    output <- adjust_ae_form_table(ae_table, is_SAE = FALSE)
    
    expect_equal(unique(output$`Serious Adverse Event`), "No")
    expected_output <- with(ae_table, ae_table[grepl("No", `Serious Adverse Event`),])
    expected_output <- expected_output[, !grepl("^SAE ", names(expected_output))]
    expect_equal(output, expected_output)
  })
  it("creates an SAE table with expected SAE columns, and the SAE prefix removed from the column names", {
    appdata <- clinsightful_data[clinsightful_data$item_group == "Adverse events", ] |> 
      get_appdata()
    form_items <- get_meta_vars(appdata)$items[["Adverse events"]]
    ae_table <- create_table(
      appdata[["Adverse events"]], 
      expected_columns = names(form_items)
    )
    SAE_cols <- c("subject_id","form_repeat", "Name", "AESI",  
                  "Start date", "End date", "CTCAE severity", 
                  "Treatment related", "Treatment action", "Other action", 
                  "Category","Awareness date", "Date of death", 
                  "Death reason")
    
    output <- adjust_ae_form_table(ae_table, is_SAE = TRUE)
  
    expect_true(is.data.frame(output))
    expect_equal(names(output)[order(names(output))], SAE_cols[order(SAE_cols)])
  })
})
