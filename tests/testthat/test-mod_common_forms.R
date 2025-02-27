describe(
  "mod_common_forms. Feature 1 | Load application module in isolation.", 
  {
    testargs <- list(
      r = reactiveValues(
        filtered_data = "",
        review_data = data.frame(), 
        filtered_tables = list(),
        subject_id = "DEU_02_482"
      ),
      form = "Adverse events",
      form_items = "" 
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
  "mod_common_forms. Feature 2 | View common forms. As a user, I want to be able 
      to view common forms in the application, which includes at least the 
      adverse events forms (with timeline view), but can also include forms 
      such as (concomitant) medication, and medical history.", 
  {
    set.seed(2023)
    appdata <- clinsightful_data |> 
      dplyr::filter(
        subject_id %in% c("DEU_02_482", "NLD_06_755"),
        item_group %in% c("Adverse events",  "Medical History", "Medication")
      ) |> 
      get_appdata()
    appvars <- get_meta_vars(appdata)
    apptables <- lapply(setNames(names(appdata), names(appdata)), \(x){
      create_table(appdata[[x]], expected_columns = names(appvars$items[[x]]))
    })
    rev_data <- get_review_data(bind_rows_custom(appdata)) |> 
      dplyr::mutate(
        id = dplyr::row_number(),
        reviewed = sample(c("Yes", "No"), dplyr::n(), replace = TRUE),
        status = sample(c("new", "old", "updated"), dplyr::n(), replace = TRUE)
      )
    form_items <- with(metadata$common_forms, item_name[item_group == "Adverse events"])
    names(form_items) <- form_items
    testargs <- list(
      r = reactiveValues(
        filtered_data = appdata,
        review_data = rev_data, 
        filtered_tables = apptables,
        subject_id = "DEU_02_482"
      ),
      form = "Adverse events",
      form_items = form_items
    ) 
    it(
      "Scenario 1 - View Adverse events table. Given a list of appdata in [filtered_tables],
          and the active subject_id set to the existing subject with ID 'DEU_02_482',
          and a data frame with current review_data in [review_data],
          and the active form set to 'Adverse events',
          and the subject having an adverse event 'Allergic Reaction' that has not yet been reviewed,
          I expect that the active data set [data_active] is a data frame object, 
          and that this data frame contains only data of subject DEU_02_482, 
          and that the output table is a valid JSON object,
          and that the Adverse event 'Allergic Reaction' exists in the active table for subject DEU_02_482,
          and that the name of the Adverse event 'Allergic Reaction' is shown in bold.", 
      {
        testServer(mod_common_forms_server, args = testargs, {
          ns <- session$ns
          session$setInputs(show_all_data = FALSE)
          expect("o_reviewed" %in% names(common_form_data()), "`o_reviewed` is an expected column for form data")
          enabled_rows <- lapply(common_form_data()[["o_reviewed"]], \(x) isFALSE(x$disabled)) |> unlist()
          expect_equal(unique(common_form_data()[enabled_rows,"subject_id",drop = TRUE]), "DEU_02_482")
          expect_true(is.data.frame(common_form_data()))

          expect_true(inherits(output[["review_form_tbl-table"]], "json"))
          allergic_ae <- with(
            common_form_data(), 
            Name[subject_id == "DEU_02_482" & grepl("allergic reaction", tolower(Name))]
            )
          expect_true(length(allergic_ae) != 0 )
          expect_true(grepl("<b>", allergic_ae))
        })
      }
    )
    it(
      "Scenario 2 - View timeline. Given a list of appdata in [filtered_tables],
          and a data frame with current review_data in [review_data],
          and the active subject_id set to the existing subject with ID 'DEU_02_482',
          and the selected [form] is 'Adverse events',
          I expect that a timeline (a valid JSON object) is shown in the 
          timeline output", 
      {
        testServer(mod_common_forms_server, args = testargs, {
          ns <- session$ns
          session$setInputs(show_all_data = FALSE)
          expect_true(inherits(output[["timeline_fig-timeline"]], "json"))
          # session$setInputs(x = 1)
          # session$flushReact()
          # expect_true(input == 1)
          # - Testing output
          # expect_true(inherits(output, "html"))
        })
      }
    )
    it(
      "Scenario 3 - View Medication.  
          Given a list of appdata in [filtered_tables],
          and the active subject_id set to the existing subject with ID 'NLD_06_755',
          and the active form set to 'Medication',
          and the subject having the medication 'Pantoprazole' that has not yet been reviewed,
          I expect that the active data set [data_active] is a data frame object, 
          and that this data frame contains only data of subject NLD_06_755, 
          and that the medication 'Pantoprazole' exists in the active table,
          and that the output table is a valid JSON object",
      {
        testargs <- list(
          r = reactiveValues(
            filtered_data = appdata,
            review_data = rev_data, 
            filtered_tables = apptables,
            subject_id = "NLD_06_755"
          ),
          form = "Medication",
          form_items = with(metadata$common_forms, item_name[item_group == "Medication"])
        )  
        
        testServer(mod_common_forms_server, args = testargs, {
          ns <- session$ns
          session$setInputs(show_all_data = FALSE)
          expect("o_reviewed" %in% names(common_form_data()), "`o_reviewed` is an expected column for form data")
          enabled_rows <- lapply(common_form_data()[["o_reviewed"]], \(x) isFALSE(x$disabled)) |> unlist()
          expect_equal(unique(common_form_data()[enabled_rows,"subject_id",drop = TRUE]), "NLD_06_755")
          expect_true(is.data.frame(common_form_data()))
          expect_true(inherits(output[["review_form_tbl-table"]], "json"))
          pantoprazole_med <- with(
            common_form_data(), 
            Name[subject_id == "NLD_06_755" & 
                   grepl("pantoprazole", tolower(Name)) & 
                   grepl("Adverse event", Indication)]
          )
          expect_true(length(pantoprazole_med) != 0 )
        })
      }
    )
    
  }
)

