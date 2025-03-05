describe(
  "mod_header_widgets. Feature 1 | Load application module in isolation.", 
  {
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_header_widgets_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_header_widgets_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    it("Can load the module server, with functioning internal parameters.", {
      testargs <- list(
        r = reactiveValues(
          filtered_data = list(
            "AEs" = data.frame(
              "subject_id" = "Subj01", 
              "event_name" = "Screening",
              "event_label" = "V0", 
              "item_name" = "Other"
            )
          )
        ), 
        rev_data = reactiveValues(), 
        navinfo = reactiveValues()
      ) 
      testServer(mod_header_widgets_server, args = testargs, {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  }
)

describe(
  "mod_header_widgets. Feature 2 | Get overview statistics of selected patient. 
      As a user, I want to be able to get an overview
      of the selected, active patient's data in the header. The information should 
      show the patient ID, the number of adverse events (including a color code 
      showing if new/updated adverse events are available), the number of forms that 
      need a review, and a timeline figure showing the number of visits that the 
      patient performed.", 
  {
    it(
      "Scenario 1 - Subject 1, with Adverse Events and a Serious Adverse Event. 
        Given data sets [rev_data] and [nav_info] and 
        [r$filtered_tables$`Adverse events`] set with test data, 
        and the active subject ID [r$subject_id] set to ['Subj01'] , 
        and the active subject having two adverse events and one severe adverse event,
        and the data frame [rev_data$summary()] containing data of 'Subj01' with 
        the column 'reviewed' containing at least one 'No',
        I expect [AEvals_active] to be a data frame with the adverse events of ['Subj01'],
        and [SAEvalue.individual] to be the value one,
        and [AEvalue.individual] to be the value two,
        and [all_AEs_reviewed] to be the value 'FALSE',
        and the output [ae_box] to contain a html element,
        and the ouput [visit_figure] to contain a plot object.", 
      {
        AE_table <- data.frame(
          "subject_id" = "Subj01", 
          "form_repeat" = 1:3, 
          `Serious Adverse Event` = c("No", "Yes", "No"), 
          check.names = FALSE
        )
        AE_figure_data <- data.frame(
          "subject_id" = "Subj01", 
          "event_name" = "Screening",
          "event_label" = factor("V0"), 
          "item_name" = "Other"
        )
        
        testargs <- list(
          r = reactiveValues(
            filtered_data = list("Adverse events" = AE_figure_data),
            filtered_tables = list("Adverse events" = AE_table)
          ), 
          rev_data = reactiveValues(
            summary = reactive({
              data.frame(
                "subject_id" = "Subj01",
                "Form" = "Adverse events",
                reviewed = c("No", "Yes")
              )
            })
          ), 
          navinfo = reactiveValues()
        ) 
        
        testServer(mod_header_widgets_server, args = testargs, {
          ns <- session$ns
          r$subject_id = "Subj01"
          session$flushReact()
          expect_equal(AEvals_active(), AE_table)
          expect_equal(SAEvalue.individual(), 1)
          expect_equal(AEvalue.individual(), 2)
          expect_false(all_AEs_reviewed())
          expect_true(inherits(output$ae_box$html, "html"))
          expect_equal(output[["visit_figure"]]$alt, "Plot object")
        })
        
      }
    )
    it(
      "Scenario 2 - Subject 2, no AEs recorded. Given data sets [rev_data] and [nav_info] and 
        [r$filtered_data$`Adverse events`] set with test data, 
        and the active subject ID [r$subject_id] set to ['Subj02'], 
        and the active subject having no adverse event data available, 
        and the data frame [rev_data$summary()] containing no data of ['Subj02'],
        I expect SAEvalue.individual() to be zero,
        and AEvalue.individual() to be zero,
        and the AEvals_active() table to be a data frame with zero rows,
        and all_AEs_reviewed() to being set to 'TRUE',
        and output$ae_box to contain a html element,
        and ouput$visit_figure to contain a plot object.", 
      {
        AE_table <- data.frame(
          "subject_id" = "Subj01", 
          "form_repeat" = 1, 
          "Serious Adverse Event" = "No", 
          check.names = FALSE
        )
        AE_figure_data <- data.frame(
          "subject_id" = "Subj01", 
          "event_name" = "Screening",
          "event_label" = factor("V0"), 
          "item_name" = "Other"
        )
        testargs <- list(
          r = reactiveValues(
            filtered_data = list("Adverse events" = AE_figure_data),
            filtered_tables = list("Adverse events" = AE_table)
          ), 
          rev_data = reactiveValues(
            summary = reactive({
              data.frame(
                "subject_id" = "Subj01",
                "Form" = "Adverse events",
                reviewed = c("No", "Yes")
              )
            })
          ), 
          navinfo = reactiveValues()
        ) 
        
        testServer(mod_header_widgets_server, args = testargs, {
          ns <- session$ns
          r$subject_id = "Subj02"
          session$flushReact()
          expect_equal(AEvals_active(), AE_table[0,])
          expect_equal(SAEvalue.individual(), 0)
          expect_equal(AEvalue.individual(), 0)
          expect_true(all_AEs_reviewed())
          expect_true(inherits(output$ae_box$html, "html"))
          expect_equal(output[["visit_figure"]]$alt, "Plot object")
        })
        
      }
    )
  }
)



