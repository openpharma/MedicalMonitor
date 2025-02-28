library(shinytest2)

describe(
  paste0("Feature 4 | Load data from CSV files, merge with custom metadata, and start the application.", 
         "As a user, I want to be able ", 
         "to load data from the raw csv file output from the EDC system and use a ", 
         "different metadata file to customize the application."), {
    it(
      paste0("Scenario 1 - Load raw data with custom metadata. ", 
      "Given raw CSV data exported from the EDC system from subject [9600-002], ", 
      "and with metadata in which total visits is restricted to V0-V10, ",
      "and with only the study_forms 'Response' and 'Vitals adjusted' in the metadata",
      "and with, at both Screening and Visit 2 the [Systolic blood pressure] being 99, ", 
      "[Diastolic Blood Pressure] 77, [Pulse] 77, [Resp] 9, and [Temperature] 37.5, ", 
      "and (only at screening) [Weight] 70kg, ", 
      "and that I browse to the 'Common events' tab",
      "I expect that I see the timeline of subject [9600-002] with study visits in it",
      "and when I browser to the 'Study data' tab",
      "I expect that I see the Vital signs page of subject [9600-002]', ", 
      "and that the compact header timeline shows visits V0-V10 ",
      "and that I see a figure with the data displayed, ",
      "and that I see a table with the data displayed after clicking on the table view, ", 
      "and that the data for the figure and table in the app is the same as ", 
      "the raw data in the CSV files."), 
      {
        options("shiny.testmode" = TRUE)
        app <- AppDriver$new(
          app_dir = testthat::test_path("fixtures", "testapp-raw"),
          name = "app-feature-4",
          timeout = 20000,
          width = 1619,
          height = 955
        )
        withr::defer(app$stop())
        
        app$set_inputs(main_tabs = "Common events")
        timeline_json <- app$get_value(output = "cf_adverse_events-timeline_fig-timeline")
        expect_true(inherits(timeline_json, "json"))
        expect_true(grepl('"subject_id":"9600-002"', timeline_json))
        expect_true(grepl("Screening", timeline_json))
        expect_true(grepl("Visit 1", timeline_json))
        expect_true(grepl("Visit 2", timeline_json))
        
        app$set_inputs(main_tabs = "Study data")
        app$wait_for_idle(1100)

        # Expect labels V0-V10 in compact header visit timeline (as defined in custom metadata file):
        fig_vals <- app$get_value(output = "header_widgets_1-visit_figure")
        expect_equal(
          fig_vals$coordmap$panels[[1]]$domain$discrete_limits$x,
          paste0("V", 0:10)
        )
        
        # Snapshot JSON table output and verify that vital signs are shown:
        app$expect_values(
          input = "sf_vitals_adjusted-switch_view",
          output = c("sf_vitals_adjusted-figure", 
                     "navigate_participants_1-subject_info", 
                     "main_sidebar_1-navigate_forms_1-form_name")
        )
        app$set_inputs("sf_vitals_adjusted-switch_view" = "table")
        app$wait_for_idle()
        app$expect_values(
          input = "sf_vitals_adjusted-switch_view",
          output = c("sf_vitals_adjusted-figure", 
                     "navigate_participants_1-subject_info", 
                     "main_sidebar_1-navigate_forms_1-form_name")
          )
        
        # Get a snapshot of the raw data of the figure:
        fig_data <- app$get_value(export = "sf_vitals_adjusted-fig_data") |> 
          dplyr::select(subject_id, event_name, event_date, item_group, item_name, item_value,  
                        item_unit, lower_lim, upper_lim) |> 
          dplyr::arrange(item_name)
        expect_snapshot(fig_data)
        # Get a snapshot of the raw data in the table:
        table_data  <- app$get_value(export = "sf_vitals_adjusted-review_form_tbl-table_data")
        expect_snapshot(print(table_data, width = Inf))
      }
    )
  }
)
