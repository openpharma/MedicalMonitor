library(shinytest2)

describe("mod_main_sidebar. Feature 1 | Load application module in isolation.", {
    testargs <- list(
      r = reactiveValues(create_query = 0),
      navinfo = reactiveValues(),
      app_data = list("Form1" = data.frame("site_code" = "", "edit_date_time" = "2023-01-01")), # used by mod_review_config()
      app_tables = list(),
      app_vars = list(
        all_forms = data.frame(),
        Sites = data.frame(),
        subject_id = "",
        form_level_data = data.frame("item_group" = "", "review_required" = TRUE)
      ),
      db_path = "",
      forms_to_review = reactiveVal(),
      available_data = data.frame()
    )
    
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_main_sidebar_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_main_sidebar_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    
    it("Can load the module server, with functioning internal parameters.", {
      testServer(mod_main_sidebar_server, args = testargs, {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  })

describe(
  "mod_main_sidebar. Feature 2 | View sidebar containing controls of application. 
    As a user, I want to be able to view the main sidebar that 
    contains buttons to save review actions, including comments, to the selected worksheet. 
    In addition, I want to be able to navigate to the next/previous form by 
    clicking on the next or previous button. The sidebar should also contain a 
    button to configure review, and a button to open a modal in which I can raise 
    a query for the selected active form and participant.", 
  {
    it(
      "Scenario 1 - Open 'create query' modal. I expect that I can open a modal 
       for creating a query after clicking the 'create query' button", 
      {
        appdata <- get_appdata(clinsightful_data)
        rev_data <- get_review_data(appdata[["Adverse events"]]) |> 
          dplyr::mutate(
            reviewed = "No",
            status = "new",
            comment = "",
            id = dplyr::row_number()
          )
        vars <- get_meta_vars(appdata, metadata)
        apptables <- lapply(
          setNames(names(appdata), names(appdata)), \(x){
            create_table(appdata[[x]], expected_columns = names(vars$items[[x]]))
          })
        all_forms <- data.frame(
          main_tab = c("Common events", "Study data"), 
          form = c("Adverse events", "Vital signs")
        )
        available_data <- get_available_data(appdata, apptables, all_forms = all_forms)
        
        test_ui <- function(request){
          bslib::page_navbar(sidebar = mod_main_sidebar_ui("test"))
        }
        test_server <- function(input, output, session){
          mod_main_sidebar_server(
            id = "test", 
            r = reactiveValues(
              create_query = 0, 
              review_data = rev_data, 
              subject_id = "NLD_06_755",
              user_name = "Admin",
              user_role = "Medical Monitor"
            ),
            navinfo = reactiveValues(active_form = "Adverse events", active_tab = "Common events"),
            app_data = list("Form1" = data.frame("site_code" = "")), # used by mod_review_config()
            app_tables = list(),
            app_vars = list(
              all_forms = data.frame(),
              Sites = data.frame(),
              subject_id = "",
              form_level_data = data.frame("item_group" = "Adverse events", "review_required" = TRUE)
            ),
            db_path = "",
            forms_to_review = reactiveVal(),
            available_data = available_data
          )
        }
        test_app <- shinyApp(test_ui, test_server)
        app <- shinytest2::AppDriver$new(
          app_dir = test_app, 
          name = "test-mod_main_sidebar", 
          timeout = 5000
        )
        withr::defer(app$stop())
        app$get_values()
        app$set_window_size(width = 1619, height = 955)
        app$click("test-write_query-create_query")
        app$expect_values()
      }
    )
    
    it(
      "Scenario 2 - Hide review panel. Given test a data frame with random data,
          and active_tab set to 'Start' (not 'Common events' or 'Study data'),
          I expect that the review panel will be hidden.", 
      {
        test_ui <- function(request){
          bslib::page_navbar(sidebar = mod_main_sidebar_ui("test"))
        }
        test_server <- function(input, output, session){
          mod_main_sidebar_server(
            id = "test", 
            r = reactiveValues(
              create_query = 0, 
              review_data = data.frame(
                "id" = 1L,
                "subject_id" = "NLD_06_755", 
                "event_name" = "Any visit",
                "item_group" = "Adverse events",
                "form_repeat" = 1L,
                "item_name" = "AE item",
                "edit_date_time" = "2023-01-01",
                "reviewed" = "",
                "comment" = "",
                "status" = ""
              ), 
              subject_id = "NLD_06_755",
              user_name = "Admin",
              user_role = "Medical Monitor"
            ),
            navinfo = reactiveValues(active_form = "Adverse events", active_tab = "Start"),
            app_data = list("Form1" = data.frame("site_code" = "")), # used by mod_review_config()
            app_tables = list(),
            app_vars = list(
              all_forms = data.frame(),
              Sites = data.frame(),
              subject_id = "",
              form_level_data = data.frame("item_group" = "Adverse events", "review_required" = TRUE)
            ),
            db_path = "",
            forms_to_review = reactiveVal(),
            available_data = data.frame()
          )
        }
        test_app <- shinyApp(test_ui, test_server)
        app <- shinytest2::AppDriver$new(
          app_dir = test_app, 
          name = "test-mod_main_sidebar-scen2", 
          timeout = 5000,
          width = 1619, 
          height = 955
        )
        withr::defer(app$stop())
        app$expect_values()
      }
    )
  }
)


