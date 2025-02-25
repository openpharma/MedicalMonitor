describe(
  "mod_timeline. Feature 1 | Load application module in isolation.",
  {
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_timeline_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_timeline_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    
    it("Can load the module server, with functioning internal parameters.", {
      testargs <- list(
        form_review_data = reactiveVal(),
        timeline_data = reactiveVal(),
        active_subject = reactiveVal("BEL_04_133")
      ) 
      testServer(mod_timeline_server, args = testargs , {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  }
)

describe(
  "mod_timeline. Feature 2 | View interactive timeline. 
  As a user, I want to be able to view an interactive timeline 
  that displays study events, such as visits and study drug administration, 
  together with data related to patient monitoring stuch as adverse events. ", 
  {
    set.seed(2023)
    appdata <- get_appdata(clinsightful_data)
    rev_data <- get_review_data(appdata[["Adverse events"]]) |> 
      dplyr::mutate(
        reviewed = sample(c("Yes", "No"), dplyr::n(), replace = TRUE),
        status = sample(c("new", "old", "updated"), dplyr::n(), replace = TRUE)
      )
    appvars <- get_meta_vars(appdata)
    apptables <- lapply(setNames(names(appdata), names(appdata)), \(x){
      create_table(appdata[[x]], expected_columns = names(appvars$items[[x]]))
    })
    timeline_data <- get_timeline_data(appdata, apptables)
    
    testargs <- list(
      form_review_data = reactiveVal(rev_data),
      timeline_data = reactiveVal(timeline_data),
      active_subject = reactiveVal("BEL_04_133")
    ) 
    it("Scenario 1 - Given a Form 'Adverse events', I expect 
       two internal dataframes (timeline_data_active() and timeline_data()) 
       and a JSON timeline object timeline as output", {
         testServer(mod_timeline_server, args = testargs, {
           ns <- session$ns
           expect_true(is.data.frame(timeline_data_active()))
           expect_equal(nrow(timeline_data_active()), 10)
           expect_true(is.data.frame(timeline_data()))
           expect_equal(nrow(timeline_data()), 203)
           expect_true(inherits(output$timeline, "json"))
         })
       })
  }
)

describe(
  "mod_timeline. Feature 3 | Customize treatment labels.
  As a user, I want to be able to customize the timeline label.", 
  {
    set.seed(2025)
    appdata <- get_appdata(clinsightful_data)
    rev_data <- get_review_data(appdata[["Adverse events"]]) |> 
      dplyr::mutate(
        reviewed = sample(c("Yes", "No"), dplyr::n(), replace = TRUE),
        status = sample(c("new", "old", "updated"), dplyr::n(), replace = TRUE)
      )
    appvars <- get_meta_vars(appdata)
    apptables <- lapply(setNames(names(appdata), names(appdata)), \(x){
      create_table(appdata[[x]], expected_columns = names(appvars$items[[x]]))
    })
    timeline_data <- get_timeline_data(appdata, apptables)
    
    testargs <- list(
      form_review_data = reactiveVal(rev_data),
      timeline_data = reactiveVal(timeline_data),
      active_subject = reactiveVal("BEL_04_133")
    ) 
    it("Scenario 1 - Standard label. Given a Form 'Adverse events', 
          and the treatment_label is not set explicitly, 
          I expect the standard treatment label '💊 Tₓ' in the timeline JSON output.", {
            testServer(mod_timeline_server, args = testargs, {
              ns <- session$ns
              expect_true(inherits(output$timeline, "json"))
              expect_true(grepl("💊 Tₓ", output$timeline))
            })
          })
    it(
      "Scenario 2 - Customized label. Given a Form 'Adverse events',
      and the treatment_label set to 'custom_treatment_label',
      I expect the  [custom_treatment_label] in the timeline JSON output.", 
      {
        timeline_data <- get_timeline_data(
          appdata, 
          apptables, 
          treatment_label = "custom_treatment_label"
        )
        
        testargs <- list(
          form_review_data = reactiveVal(rev_data),
          timeline_data = reactiveVal(timeline_data),
          active_subject = reactiveVal("BEL_04_133")
        ) 
        testServer(mod_timeline_server, args = testargs, {
          ns <- session$ns
          expect_true(is.data.frame(timeline_data_active()))
          expect_true(inherits(output$timeline, "json"))
          expect_true(grepl("custom_treatment_label", output$timeline))
        })
      }
    )
  }
)

