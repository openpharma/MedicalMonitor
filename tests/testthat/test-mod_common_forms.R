describe(
  "mod_common_forms. Feature 1 | Load application module in isolation.", 
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
  "mod_common_forms. Feature 2 | View common forms. As a user, I want to be able 
      to view common forms in the application such as adverse events and medical 
      history, which which can include a flexible JSON table (created with 
      mod_review_form_tbl) and a timeline (created with mod_timeline).", 
  {
    set.seed(2023)
    appdata <- clinsightful_data |> 
      dplyr::filter(
        subject_id %in% c("DEU_02_482", "NLD_06_755"),
        item_group %in% c("Adverse events")
      ) |> 
      get_appdata()
    appvars <- get_meta_vars(appdata)
    apptables <- lapply(setNames(names(appdata), names(appdata)), \(x){
      create_table(appdata[[x]], expected_columns = names(appvars$items[[x]]))
    })
    rev_data <- get_review_data(appdata[["Adverse events"]]) |> 
      dplyr::mutate(
        id = dplyr::row_number(),
        reviewed = sample(c("Yes", "No"), dplyr::n(), replace = TRUE),
        status = sample(c("new", "old", "updated"), dplyr::n(), replace = TRUE)
      )
    form_items <- appvars$items[["Adverse events"]]
    timeline_data <- get_timeline_data(appdata, apptables)
    testargs <- list(
      form = "Adverse events",
      form_data = reactiveVal(appdata[["Adverse events"]]),
      form_review_data = reactiveVal(rev_data),
      form_items = form_items,
      active_subject = reactiveVal("DEU_02_482"),
      table_names = NULL,
      timeline_data = reactiveVal(timeline_data)
    ) 
    it(
      "Scenario 1 - View Adverse events and SAE tables. Given the form [Adverse events],
          and the active subject_id set to the existing subject with ID 'DEU_02_482',
          and a data frame with current review_data in [review_data],
          I expect that there is a valid adverse event table (a JSON object) 
          and that there is a valie SAE table (a JSON object).", 
      {
        testServer(mod_common_forms_server, args = testargs, {
          ns <- session$ns
          session$setInputs(show_all_data = FALSE)
          expect_true(inherits(output[["review_form_SAE_tbl-table"]], "json"))
          expect_true(inherits(output[["review_form_tbl-table"]], "json"))
        })
      }
    )
    it(
      "Scenario 2 - View timeline. Given the form [Adverse events] with  
          form-specific data and review data,
          and the active subject_id set to the existing subject with ID 'DEU_02_482',
          I expect that a timeline (a valid JSON object) is shown in the 
          timeline output", 
      {
        testServer(mod_common_forms_server, args = testargs, {
          ns <- session$ns
          session$setInputs(show_all_data = FALSE)
          expect_true(inherits(output[["timeline_fig-timeline"]], "json"))
        })
      }
    )
    it(
      "Scenario 3 - View Medication. Given a list of appdata in [filtered_tables],
          and a data frame with current review_data in [review_data],
          and the active subject_id set to the existing subject with ID 'DEU_02_482',
          and the selected [form] is 'Adverse events',
          I expect that a timeline (a valid JSON object) is shown in the 
          timeline output", 
      {
        appdata <- subset(clinsightful_data, item_group %in% c("Medication")) |> 
          get_appdata()
        rev_data <- get_review_data(appdata[["Medication"]]) |> 
          dplyr::mutate(id = dplyr::row_number(), reviewed = "No", status = "new")
        form_items <- get_meta_vars(appdata)$items[["Medication"]]
        timeline_data <- timeline_data[0,]
        
        testargs <- list(
          form = "Medication",
          form_data = reactiveVal(appdata[["Medication"]]),
          form_review_data = reactiveVal(rev_data),
          form_items = form_items,
          active_subject = reactiveVal("DEU_02_482"),
          table_names = NULL,
          timeline_data = reactiveVal(timeline_data)
        )  
        
        testServer(mod_common_forms_server, args = testargs, {
          ns <- session$ns
          session$setInputs(show_all_data = FALSE)
          expect_error(output[["timeline_fig-timeline"]])
          expect_error(output[["review_form_SAE_tbl-table"]])
          expect_true(inherits(output[["review_form_tbl-table"]], "json"))
        })
      }
    )
  }
)

