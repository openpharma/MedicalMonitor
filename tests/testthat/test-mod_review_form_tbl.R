describe(
  "mod_review_form_tbl. Feature 1 | Load application module in isolation.", 
  {
    testargs <- list(
      form = "Adverse events",
      form_data = reactiveVal(),
      form_review_data = reactiveVal(),
      form_items = "",
      active_subject = reactiveVal("DEU_02_482"),
      table_names = NULL,
      timeline_data = reactiveVal()
    ) 
    
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_common_forms_ui(id = "test", form = "Adverse events")
      golem::expect_shinytag(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_study_forms_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    
    it("Can load the module server, with functioning internal parameters.", {
      testServer(mod_common_forms_server, args = testargs , {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  }
)

describe(
  "mod_review_form_tbl. Feature 2 | View form tables. As a user, I want to 
  be able to view a table belonging to a form, in the correct format and with 
  data highlighted as expected.", 
  {
    ## Old scenario from mod_common_forms():
    ##
    # it(
    #   "Scenario 1 - View Adverse events table. Given a list of appdata in [filtered_tables],
    #       and the active subject_id set to the existing subject with ID 'DEU_02_482',
    #       and a data frame with current review_data in [review_data],
    #       and the active form set to 'Adverse events',
    #       and the subject having an adverse event 'Allergic Reaction' that has not yet been reviewed,
    #       I expect that the active data set [data_active] is a data frame object, 
    #       and that this data frame contains only data of subject DEU_02_482, 
    #       and that the output table is a valid JSON object,
    #       and that the Adverse event 'Allergic Reaction' exists in the active table for subject DEU_02_482,
    #       and that the name of the Adverse event 'Allergic Reaction' is shown in bold.", 
    #   {
    #     testServer(mod_common_forms_server, args = testargs, {
    #       ns <- session$ns
    #       session$setInputs(show_all_data = FALSE)
    #       
    #       ## continue here
    #       browser()
    #       expect_true(FALSE)
    #       expect_true(inherits(output[["review_form_tbl-table"]], "json"))
    #     })
    #   }
    # )
    # 
    # it(
    #   "Scenario 3 - View Medication.  
    #       Given a list of appdata in [filtered_tables],
    #       and the active subject_id set to the existing subject with ID 'NLD_06_755',
    #       and the active form set to 'Medication',
    #       and the subject having the medication 'Pantoprazole' that has not yet been reviewed,
    #       I expect that the active data set [data_active] is a data frame object, 
    #       and that this data frame contains only data of subject NLD_06_755, 
    #       and that the medication 'Pantoprazole' exists in the active table,
    #       and that the medication name is shown in bold, 
    #       and that the output table is a valid JSON object",
    #   {
    #     testargs <- list(
    #       r = reactiveValues(
    #         filtered_data = appdata,
    #         review_data = rev_data, 
    #         filtered_tables = apptables,
    #         subject_id = "NLD_06_755"
    #       ),
    #       form = "Medication",
    #       form_items = with(metadata$common_forms, item_name[item_group == "Medication"])
    #     )  
    #     
    #     testServer(mod_common_forms_server, args = testargs, {
    #       ns <- session$ns
    #       session$setInputs(show_all_data = FALSE)
    #       expect("o_reviewed" %in% names(common_form_data()), "`o_reviewed` is an expected column for form data")
    #       enabled_rows <- lapply(common_form_data()[["o_reviewed"]], \(x) isFALSE(x$disabled)) |> unlist()
    #       expect_equal(unique(common_form_data()[enabled_rows,"subject_id",drop = TRUE]), "NLD_06_755")
    #       expect_true(is.data.frame(common_form_data()))
    #       expect_true(inherits(output[["review_form_tbl-table"]], "json"))
    #       pantoprazole_med <- with(
    #         common_form_data(), 
    #         Name[subject_id == "NLD_06_755" & 
    #                grepl("pantoprazole", tolower(Name)) & 
    #                grepl("Adverse event", Indication)]
    #       )
    #       expect_true(length(pantoprazole_med) != 0 )
    #       expect_true(grepl("<b>", pantoprazole_med))
    #     })
    #   }
    # )
    # ## 
    # ### old mod_study_form test:
    # it(
    #   "Scenario 4 - Given subject id NLD_05_561,
    #       and the input value [show_all] is set to 'TRUE',
    #       I expect that a table with review data of everyone will be shown,
    #       and that a valid JSON output table will be created", {
    #         testServer(mod_study_forms_server, args = testargs, {
    #           ns <- session$ns
    #           r$subject_id = "NLD_05_561"
    #           session$setInputs(
    #             filter = c("pulse", "BMI"),
    #             show_all = TRUE
    #           )
    #           expect_true(is.data.frame(study_form_data()))
    #           enabled_rows <- lapply(study_form_data()[["o_reviewed"]], \(x) isFALSE(x$disabled)) |> unlist()
    #           expect_equal(nrow(study_form_data()), 68)
    #           
    #           table_ids <- unique(study_form_data()$subject_id)
    #           table_ids <- table_ids[order(table_ids)]
    #           expected_ids <- unique(r$review_data$subject_id)
    #           expected_ids <- expected_ids[order(expected_ids)]
    #           expect_equal(table_ids, expected_ids)
    #           
    #           expect_true(inherits(output[["review_form_tbl-table"]], "json"))
    #         })
    #       })
  }
)