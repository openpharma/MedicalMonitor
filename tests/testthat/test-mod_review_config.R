describe(
  "mod_report. Feature 1 | Load application module in isolation.", {
    appdata <- get_appdata(clinsightful_data)
    vars <- get_meta_vars(appdata, metadata)
    testargs <- list(
      r = reactiveValues(
        subject_id = "DEU_02_866",
        user_name = "test user",
        user_roles = "Medical Monitor",
        user_role = "Medical Monitor"
      ),
      app_data = appdata,
      app_tables = list("tab1" = data.frame(subject_id = vars$subject_id)),
      sites = vars$Sites,
      subject_ids = "DEU_02_866"
    )
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_review_config_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_review_config_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    
    it("Can load the module server, with functioning internal parameters.", {
      testServer(mod_review_config_server, args = testargs, {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  }
)

describe(
  "mod_review_config. Feature 2 | Select user configuration. 
    As a user, I want to be able to select my user 
    configuration before I start to perform a review. I want to be able to 
    configure the regions and the sites that I will review. After selecting these, 
    the data should be filtered so that only data from the filtered regions/sites 
    will be shown.", 
  {
    appdata <- get_appdata(clinsightful_data)
    vars <- get_meta_vars(appdata, metadata)
    apptables <- list("tab1" = data.frame(subject_id = vars$subject_id))
    
    testargs <- list(
      r = reactiveValues(subject_id = "DEU_02_866",
                         user_name = "test user",
                         user_roles = "Medical Monitor",
                         user_role = "Medical Monitor",
                         filtered_data = appdata, 
                         filtered_tables = apptables, 
                         filtered_subjects = vars$subject_id),
      app_data = appdata,
      app_tables = apptables,
      sites = vars$Sites,
      subject_ids = vars$subject_id
    )
    
    it(
      "Scenario 1 - Warn for missing sites. Given a test data set with random data, 
        and and a site name was provided that is not available in the test data set, 
        I expect that a warning will be given with the text 'Not all sites are 
        found in the appdata'.", 
      {
        testargs$sites <- testargs$sites |> 
          rbind(data.frame("site_code" = "Site Unknown", "region" = "NLD"))
        expect_warning(
          testServer(mod_review_config_server, args = testargs, {
          }), 
          "Not all sites are found in the appdata."
        )
      }
    )
    
    
    it("Scenario 2 - Filters data and subject ids as expected. 
        Given a test data set with random data containing the regions 'BEL', 'NLD', and 'DEU',
          I expect that the regions are initially set to 'BEL', 'NLD', and 'DEU', 
          and given that I select the region 'NLD' and press the [Save] button,
          I expect that the app regions are now set to 'NLD',
          and that the data frame is filtered to contain only the SubjectIds 
          from region 'NLD'.", {
            testServer(mod_review_config_server, args = testargs, {
              ns <- session$ns
              modvars <- reactiveValues()
              regions_filtered_initial <- get_unique_vars(r$filtered_data, "region")$region
              expect_equal(regions_filtered_initial, c("BEL", "DEU", "NLD"))
              
              subjects_filtered_initial <-   order_string(
                get_unique_vars(r$filtered_data, "subject_id")$subject_id
              )
              
              expect_equal(subjects_filtered_initial, subject_ids)
              
              session$setInputs(
                active_role = r$user_role,
                site_selection = with(sites, site_code[region == "NLD"]),
                region_selection = "NLD",
                save_review_config = 1
              )
              expect_equal(
                get_unique_vars(r$filtered_data, "region")$region, c("NLD")
              )
              subjects <- get_unique_vars(r$filtered_data, "subject_id")$subject_id
              expect_true(all(grepl("^NLD_", subjects)))
            })
          })
    it("Scenario 3 - Warns if only sites are selected that are not in the app data set. 
       Given a test data set with random data,
       and region is set to 'NLD',
       and site selection is set only to the non-existent 'Site x',
       and the button [Save] is pressed,
       I expect that the filtering will be aborted,
       and that a warning will be shown. 
       ", {
         testServer(mod_review_config_server, args = testargs, {
           ns <- session$ns
           session$setInputs(region_selection = "NLD")
           session$setInputs(site_selection = "Site x")
           expect_no_error(suppressWarnings(
             session$setInputs(save_review_config = 2)))
           expect_warning(
             session$setInputs(save_review_config = 3)
           )
         })
       })
    it(
      "Scenario 4 - Apply review configuration. 
        Given a test data set containing regions 'NLD', 'DEU', and 'BEL', 
        and sites 'DEU01' and 'DEU02' belonging to region 'DEU',
        and clicking on [settings],
        I expect to see the modal to select regions and sites to review,
        and given that I deselect all regions and click on [Save],
        I expect that I will get the message 'You must select at least one site/region to review.',
        and that the data within the app will not be updated with the empty selection,
        and given that I select region 'DEU',
        I expect that only the sites 'DEU01' and 'DEU02' will be selected,
        and given that I click on [Save],
        I expect that a confirmation will be shown with the text 'Review configuration applied successfully',
        and that the data within the app only contains data of 'DEU01' and 'DEU02', 
        and I expect that the selected configuration is shown correctly when 
        opening the configuration panel again.", 
      {
        test_ui <- function(request){
          tagList(
            shinyjs::useShinyjs(),
            bslib::page(
              bslib::card(
                mod_review_config_ui("test")
              )
            )
          )
        }
        test_server <- function(input, output, session){
          r = reactiveValues(
            subject_id = "DEU_02_866", 
            user_name = "test user",
            user_roles = "Medical Monitor",
            user_role = "Medical Monitor",
            filtered_data = appdata, 
            filtered_tables = apptables, 
            filtered_subjects = vars$subject_id
          )
          
          mod_review_config_server(
            "test", r, app_data = appdata, 
            app_tables = apptables, sites = vars$Sites, subject_ids = vars$subject_id
          )
          exportTestValues(filtered_data = r$filtered_data)
        }
        test_app <- shinyApp(test_ui, test_server, options = list("test.mode" = TRUE))
        app <- shinytest2::AppDriver$new(
          app_dir = test_app, 
          name = "mod_review_config",
          width = 1619, 
          height = 955
        )
        withr::defer(app$stop())
        app$click("test-config_review")
        app$expect_values(input = TRUE, output = TRUE)
        app$set_inputs(`test-region_selection` = "")
        app$expect_values(input = TRUE, output = TRUE)
        app$click("test-save_review_config")
        filtered_data <- app$get_value(export = "filtered_data")
        all_sites <- lapply(filtered_data, \(x){x[["site_code"]]}) |> 
          unlist() |> 
          unique()
        expect_equal(
          all_sites[order(all_sites)], 
          sort(vars$Sites$site_code)
        )
        app$set_inputs(`test-region_selection` = "DEU")
        app$expect_values(input = TRUE, output = TRUE)
        app$click("test-save_review_config")
        app$expect_values(input = TRUE, output = TRUE)
        filtered_data <- app$get_value(export = "filtered_data")
        all_sites <- lapply(filtered_data, \(x){x[["site_code"]]}) |> 
          unlist() |> 
          unique()
        expect_equal(
          all_sites[order(all_sites)], 
          c("DEU01", "DEU02")
        )
        
        app$wait_for_js("$('#shiny-modal').modal('hide');")
        app$click("test-config_review")
        app$wait_for_idle(800)
        
        input_vals <- app$get_values(input = TRUE)$input
        expect_equal(input_vals$`test-active_role`, "Medical Monitor")
        expect_equal(input_vals$`test-region_selection`, "DEU")
        expect_equal(input_vals$`test-site_selection`, c("DEU01", "DEU02"))
      }
    )
  }
)

describe(
  "mod_review_config. Feature 3 | Allow to change roles if multiple are assigned. 
    As a user, I want to be able to change my role, 
    if there are multiple roles allocated.", 
  {
    it("Scenario 1 - Change user role. 
        Given a user named 'test user' with the user_role set to 'Administrator', 
          and the available roles set to 'Administrator' and 'Medical Monitor', 
          and a test data set with random data, 
          and after setting the 'active_role' to 'Medical Monitor' 
          and clicking 'save changes',
          I expect that the user role is changed to 'Medical Monitor'.", {
            appdata <- get_appdata(clinsightful_data)
            vars <- get_meta_vars(appdata, metadata)
            apptables <- list("tab1" = data.frame(subject_id = vars$subject_id))
            
            testargs <- list(
              r = reactiveValues(subject_id = "DEU_02_866",
                                 user_name = "test user",
                                 user_roles = c("Administrator", 
                                                "Medical Monitor"),
                                 user_role = "Administrator",
                                 filtered_data = appdata, 
                                 filtered_tables = apptables, 
                                 filtered_subjects = vars$subject_id),
              app_data = appdata,
              app_tables = apptables,
              sites = vars$Sites,
              subject_ids = vars$subject_id
            )
            
            testServer(mod_review_config_server, args = testargs, {
              ns <- session$ns
              modvars <- reactiveValues()
              
              expect_equal(r$user_role, "Administrator")
              expect_equal(r$user_roles, c("Administrator", "Medical Monitor"))
              
              session$setInputs(
                active_role = "Medical Monitor",
                site_selection = vars$Sites$site_code,
                region_selection = vars$subject_id,
                save_review_config = 1
              )
              expect_equal(r$user_role, "Medical Monitor")
            })
          })
  }
)

