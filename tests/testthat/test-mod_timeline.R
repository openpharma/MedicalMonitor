describe(
  "mod_timeline. Feature 1 | Load application module in isolation.",
  {
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_timeline_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_study_forms_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    
    it("Can load the module server, with functioning internal parameters.", {
      
      testargs <- list(
        r = reactiveValues(
          filtered_data = list(),
          review_data = data.frame(), 
          filtered_tables = list("Adverse events" =  data.frame()),
          subject_id = "BEL_04_133"
        ),
        form = "Adverse events"
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
    AE_table <- create_table(appdata[["Adverse events"]])
    testargs <- list(
      r = reactiveValues(
        filtered_data = appdata,
        review_data = rev_data, 
        filtered_tables = list("Adverse events" =  AE_table),
        subject_id = "BEL_04_133"
      ),
      form = "Adverse events"
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
    it("Scenario 2 - Given a Form other than 'Adverse events', I expect 
       two empty internal dataframes (timeline_data_active() and timeline_data()) 
       and an empty timeline object as output", {
         testargs <- list(
           r = reactiveValues(
             filtered_data = appdata,
             review_data = rev_data, 
             filtered_tables = list("Adverse events" =  AE_table),
             subject_id = "BEL_04_133"
           ),
           form = "Other form"
         ) 
         testServer(mod_timeline_server, args = testargs, {
           ns <- session$ns
           expect_error(timeline_data_active())
           expect_error(timeline_data())
           expect_error(output$timeline)
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
    AE_table <- create_table(appdata[["Adverse events"]])
    testargs <- list(
      r = reactiveValues(
        filtered_data = appdata,
        review_data = rev_data, 
        filtered_tables = list("Adverse events" =  AE_table),
        subject_id = "BEL_04_133"
      ),
      form = "Adverse events", 
      treatment_label = NULL
    ) 
    it("Scenario 1 - Standard label. Given a Form 'Adverse events', 
          and the treatment_label is missing, 
          I expect the standard treatment label '💊 Tₓ' in the timeline JSON output.", {
         testServer(mod_timeline_server, args = testargs, {
           ns <- session$ns
           expect_true(inherits(output$timeline, "json"))
           expect_true(grepl("💊 Tₓ", output$timeline))
         })
       })
    it("Scenario 2 - Customized label. Given a Form other than 'Adverse events', 
          and the treatment_label set to 'custom_treatment_label',
          I expect the  [custom_treatment_label] in the timeline JSON output.", {
            testargs <- list(
              r = reactiveValues(
                filtered_data = appdata,
                review_data = rev_data, 
                filtered_tables = list("Adverse events" =  AE_table),
                subject_id = "BEL_04_133"
              ),
              form = "Adverse events", 
              treatment_label = "custom_treatment_label"
            ) 
         testServer(mod_timeline_server, args = testargs, {
           ns <- session$ns
           expect_true(is.data.frame(timeline_data_active()))
           expect_true(inherits(output$timeline, "json"))
           expect_true(grepl("custom_treatment_label", output$timeline))
         })
       })
  }
)

