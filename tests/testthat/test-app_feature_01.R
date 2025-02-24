library(shinytest2)

describe(
  "Feature 1 | Explore data. As a user, I want to be able to explore data in both
    common forms and study-specific forms of multiple participants in the application,
    so that I can appropriately assess whether there is a risk for the patient's safety.",
  {
    it("Scenario 1 - Data visualization.
          Given a fixed random test data set,
          with all data being marked as 'new'/not yet reviewed,
          and being logged in as test user,
          I expect that I can see the start page,
          and that, by double clicking on clicking on 'BEL_04_772' within 
          the start page table, with and without sorting,
          I can see the adverse event page of the selected patient with all 
          data shown in bold (indicating the need for review),
          and that, by clicking on [Study data],
          I can see the Vital signs page of the selected patient, 
          with all data shown in large dots (indicating the need for review),
          and that, after clicking on table view,
          the same vital signs data will be shown in a table, 
          with all data in bold (indicating the need for review),
          and that, after clicking on the header widget with participant info,
          and setting the patient to 'NLD_06_893' and clicking 'apply',
          I can see the vital signs page of the newly selected patient.",
       {
         app <- AppDriver$new(
           app_dir = test_path("fixtures/testapp"),
           name = "app-feature-1",
           timeout = 25000,
           width = 1619,
           height = 955
         )
         withr::defer(app$stop())
         app$wait_for_idle(8000)
         # focus on the output, since input has some values that are 
         # inconsistently created by DT::datatable (for example, 
         # xxx_table_rows_current)
         # also, remove ggplot figure since it is recommended to remove these 
         # from snaps, since they cannot be serialized to JSON and produce a 
         # flaky hash https://rstudio.github.io/shinytest2/articles/robust.html
         output_names <- names(app$get_values(output = TRUE)$output)
         app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
         
         app$run_js('$("#start_page_1-overview_table td").filter(function() {return $(this).text() == "BEL_04_772"}).closest("tr").trigger("dblclick")')
         app$wait_for_idle()
         expect_equal(app$get_value(input = "main_tabs"), "Common events")
         output_names <- names(app$get_values(output = TRUE)$output)
         app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
         
         app$set_inputs(main_tabs = "Start")
         app$run_js('$("#start_page_1-overview_table th").filter(function() {return $(this).text() == "Status"}).closest("th").trigger("click")')
         app$run_js('$("#start_page_1-overview_table td").filter(function() {return $(this).text() == "BEL_04_772"}).closest("tr").trigger("dblclick")')
         app$wait_for_idle()
         expect_equal(app$get_value(input = "main_tabs"), "Common events")
         expect_equal(app$get_text("#navigate_participants_1-subject_info .value-box-title"), "BEL_04_772")
         
         app$set_inputs(main_tabs = "Study data")
         app$wait_for_idle()
         output_names <- names(app$get_values(output = TRUE)$output)
         app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
         
         app$set_inputs("sf_vital_signs-switch_view" = "table")
         output_names <- names(app$get_values(output = TRUE)$output)
         app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
         
         app$run_js("$('#navigate_participants_1-subject_info').click()")
         app$wait_for_idle(1500)
         app$set_inputs("navigate_participants_1-participant_selection" = "NLD_06_893")
         app$wait_for_idle(1000)
         app$click("navigate_participants_1-subj_apply")
         app$wait_for_idle()
         output_names <- names(app$get_values(output = TRUE)$output)
         app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
         
         ## Note (LSA): after clicking on 'graph' view again at this moment, an 
         ## error will be shown instead of the expected figures. 
         ## ('Error: incorrect height and width'). So far, this error could not 
         ## be isolated and only occurred when running the script above 
         ## non-interactively. Therefore, no further action is taken.
         #app$view()
         #app$set_inputs("sf_vital_signs-switch_view" = "graph")
       }
    )
  }
)


