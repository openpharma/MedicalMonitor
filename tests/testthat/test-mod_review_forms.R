describe(
  "mod_review_forms. Feature 1 | Load application module in isolation.", 
  {
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_review_forms_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_review_forms_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    it("Can load the module server, with functioning internal parameters.", {
      testargs <- list(
        r = reactiveValues(
          user_name = "test_name",
          user_role = "Medical Monitor",
          subject_id = "885",
          review_data = reactiveValues()
        ),
        active_form = reactiveVal("Adverse events"),
        active_tab = reactiveVal("Common forms"),
        review_required_data = data.frame(
          "item_group" = "Adverse events", 
          "review_required" = TRUE
          ),
        db_path = ""
      )
      testServer(
        mod_review_forms_server, args = testargs, {
          ns <- session$ns
          expect_true(inherits(ns, "function"))
          expect_true(grepl(id, ns("")))
          expect_true(grepl("test", ns("test")))
        })
    })
  }
)

describe(
  paste0("mod_review_forms. Feature 2 | Save review of a form. ",
         "As a user, I want to be able to save ",
         "a review of a form in the database. After saving the review, all items of ", 
         "that form that are not yet reviewed should get a tag that the value was ",
         "reviewed."), 
  {
    it(
      paste0("Scenario 1 - Save a review. Given test review data with at ",
             "least an 'Adverse event' form with patient '885',",
             "and [user_name] set to 'test_name' and [user_role] to 'Medical Monitor'",
             "and [active_patient] set to '885', ",
             "and [active_form] set to 'Adverse events', ",
             "and [active_tab] set to 'Common forms', ",
             "and [form_reviewed] set to FALSE, ",
             "I expect that I can save a new review properly, ",
             "with the result saved in the application being the same as ", 
             "the one saved in the database, ",
             "and no review error occurring"),
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        
        testargs <- list(
          r = reactiveValues(
            user_name = "test_name",
            user_role = "Medical Monitor",
            subject_id = "885",
            review_data = do.call(reactiveValues, split_review_data(temp_path))
          ),
          active_form = reactiveVal("Adverse events"),
          active_tab = reactiveVal("Common forms"),
          review_required_data = data.frame(
            "item_group" = "Adverse events", 
            "review_required" = TRUE
          ),
          db_path = temp_path
        )
        testServer(
          mod_review_forms_server, args = testargs, {
            ns <- session$ns
            
            session$userData$review_records <- reactiveValues()
            session$userData$update_checkboxes <- reactiveValues()
            
            ## patient has two rows: AF and Cystitis. AF is already reviewed by someone else: 
            expect_equal(
              data.frame(
                item_name = c("Atrial Fibrillation", "Cystitis"),
                status = c("old", "new")
              ),
              db_temp_connect(db_path, {
                DBI::dbGetQuery(
                  con, 
                  paste0("SELECT item_name, status FROM all_review_data ",
                         "WHERE subject_id = '885'")
                )
              })
            )
            
            session$setInputs(form_reviewed = FALSE) # Needs to be initialized to work
            session$setInputs(form_reviewed = TRUE, save_review = 1)
            db_reviewdata <- split_review_data(db_path)
            db_reviewlogdata <- db_get_table(db_path, "all_review_data_log")
            
            expect_false(review_save_error())
            # app data should be equal to DB data
            expect_equal(reactiveValuesToList(r$review_data), db_reviewdata)
            # review table should only have one row in the DB containing the new reviewed = "Yes"
            # for the item 'Cystitis'
            expect_equal(
              with(db_reviewdata[["Adverse events"]], reviewed[subject_id == "885" & item_name == "Cystitis"]), 
              "Yes"
              )
            # log table should only have one row in the DB containing the old reviewed = "No"
            r_id <- with(db_reviewdata[["Adverse events"]], id[subject_id == "885" & item_name == "Cystitis"])
            expect_equal(with(db_reviewlogdata, reviewed[review_id == r_id]), c("No") )
            expect_snapshot({
              reactiveValuesToList(r$review_data) |> 
              lapply(\(x){dplyr::select(x, -timestamp)})
            })
            Sys.sleep(2) # because the timestamp only records seconds, 
            # we should add delay here to prevent that the exact same timestamp is 
            # created in the next step. The timestamp is needed for uniquely selecting the latest entry.
            # It would still work since it defaults to select the last row of the database, 
            # but it might give a warning. 
            
            # It saves a review a second time properly if the review state is changed, 
            # only showing the latest review data in the app but storing all review
            # data, including the previous one, in the database
            # ! if add_comment != logical (1 for example), the test below will fail.
            # two rows with the same editdateetime will be written to the database.
            session$setInputs(
              form_reviewed = FALSE, 
              add_comment = TRUE, 
              review_comment = "test review",
              save_review = 2
              )
            
            updated_rows_db <- db_get_review(
              db_path, subject_id = "885", item_group = "Adverse events"
            )
            
            expect_equal(updated_rows_db$comment, c("test review", "test review"))
            expect_equal(updated_rows_db$reviewed, c("No", "No"))
            
            db_reviewdata <- db_temp_connect(db_path, {
              dplyr::tbl(con, "all_review_data") |> 
                dplyr::collect()
            })
            db_reviewlogdata <- db_temp_connect(db_path, {
              dplyr::tbl(con, "all_review_data_log") |> 
                dplyr::collect()
            })
            
            expect_false(review_save_error())
            expect_equal(with(db_reviewdata, comment[subject_id == "885"]), c("test review", "test review"))
            expect_equal(with(db_reviewdata, reviewed[subject_id == "885"]), c("No", "No"))
            r_id <- with(db_reviewdata, id[subject_id == "885"])
            expect_equal(with(db_reviewlogdata, comment[review_id %in% r_id]), c("", "test comment", ""))
            expect_equal(with(db_reviewlogdata, reviewed[review_id %in% r_id]), c("No", "Yes", "Yes"))
            expect_equal(reactiveValuesToList(r$review_data), split_review_data(db_path))
            expect_equal(do.call(rbind, reactiveValuesToList(r$review_data)), db_reviewdata, ignore_attr = TRUE)
            expect_snapshot({
              reactiveValuesToList(r$review_data) |> 
                lapply(\(x){dplyr::select(x, -timestamp)})
            })
          })
      }
    )
    it(
      "Scenario 2 - Save a review. Given a data frame
      and a database with review data with [reviewed] status set to 'new' (not reviewed yet),
        and [user_name] set to 'test_name' and [user_role] to 'Medical Monitor',
        and [subject_id]  set to '885',
        and [active_form] set to 'Adverse events',
        and first (1) No input is given, then (2) the [form_reviewed] tick box 
        is ticked and the comment field enabled,
        and then (3) the [save_review] button is clicked,
        I expect that, after each action, the save review button and the option 
        to add a comment will be (1) disabled, (2) enabled, and (3) disabled,
        and that, after the aforementioned input actions, the review status of the active form and
        active subject in the database is set to 'old' (reviewed),
        and the reviewer is set to 'test_name'.",
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        test_ui <- function(request){
          tagList(
            shinyjs::useShinyjs(),
            bslib::page_navbar(sidebar = bslib::sidebar(mod_review_forms_ui("test")))
          )
        }
        test_server <- function(input, output, session){
          session$userData$review_records <- reactiveValues()
          session$userData$update_checkboxes <- reactiveValues()
          
          mod_review_forms_server(
            id = "test",
            r = reactiveValues(
              user_name = "test_name",
              user_role = "Medical Monitor",
              subject_id = "885",
              review_data = do.call(reactiveValues, split_review_data(temp_path))
            ),
            active_form = reactiveVal("Adverse events"),
            active_tab = reactiveVal("Common events"),
            review_required_data = data.frame(
              "item_group" = "Adverse events", 
              "review_required" = TRUE
            ),
            db_path = temp_path
          )
        }
        test_app <- shinyApp(test_ui, test_server)
        app <- shinytest2::AppDriver$new(
          app_dir = test_app,
          name = "test-mod_review_forms",
          timeout = 8000,
          width = 1619,
          height = 955
        )
        withr::defer(app$stop())
        app$wait_for_idle(2500)
        # save button and comment option should not be available:
        app$expect_values()
        expect_true(app$get_js("document.getElementById('test-save_review').disabled;"))
        expect_true(app$get_js("document.getElementById('test-add_comment').disabled;"))
        expect_true(app$get_js("document.getElementById('test-review_comment').disabled;"))
        
        app$click("test-form_reviewed")
        app$click("test-add_comment")
        # now the save button and comment option is available:
        app$expect_values()
        expect_false(app$get_js("document.getElementById('test-save_review').disabled;"))
        expect_false(app$get_js("document.getElementById('test-add_comment').disabled;"))
        expect_false(app$get_js("document.getElementById('test-review_comment').disabled;"))
        
        app$click("test-save_review")
        app$wait_for_idle()
        # save button and comment option should not be available anymore:
        app$expect_values()
        expect_true(app$get_js("document.getElementById('test-save_review').disabled;"))
        expect_true(app$get_js("document.getElementById('test-add_comment').disabled;"))
        expect_true(app$get_js("document.getElementById('test-review_comment').disabled;"))
        
        # review status and reviewer is saved as expected
        saved_review_row <- db_get_review(temp_path, subject_id = "885", item_group = "Adverse events")
        expect_equal(saved_review_row$status, c("old", "old"))
        expect_equal(saved_review_row$reviewer, c("Reviewer 1", "test_name (Medical Monitor)"))
      }
    )
  }
)

describe(
  "mod_review_forms. Feature 3 | Disable review buttons if review not applicable 
    or not allowed, and give feedback why. 
    As a user, I want to get feedback on whether a 
    review is needed or not for an active form. The review controls should only be 
    enabled when there is data to review.", 
  {
    it(
      "Scenario 1 - Review needed. Given test review data with at least an 
        'Adverse event' form with patient '885',
        and [active_patient] set to '885',
        and [active_form] set to 'Adverse events',
        and [active_tab] set to 'Common forms',
        and [form_reviewed] set to FALSE,
        I expect that the data frame [active_review_data] contains two rows with 
        data of participant '885',
        and with the [item_group] set to 'Adverse events',
        and that a message will be displayed containing the text 'Requires review'", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        testargs <- list(
          r = reactiveValues(
            user_name = "test_name",
            user_role = "Medical Monitor",
            subject_id = "885",
            review_data = do.call(reactiveValues, split_review_data(temp_path))
          ),
          active_form = reactiveVal("Adverse events"),
          active_tab = reactiveVal("Common forms"),
          review_required_data = data.frame(
            "item_group" = "Adverse events", 
            "review_required" = TRUE
          ),
          db_path = temp_path
        )
        
        testServer(mod_review_forms_server, args = testargs, {
          ns <- session$ns
          session$setInputs(form_reviewed = FALSE)
          expect_equal(
            review_data_active(),
            subset(r$review_data[["Adverse events"]], subject_id == "885")
          )
          expect_equal(review_data_active()$item_group, c("Adverse events", "Adverse events"))
          expect_equal(nrow(review_data_active()), 2)
          expect_error(output[["save_review_error"]], "Requires review")
        })
      }
    )
    it(
      "Scenario 2 - Saving with unchanged review status. Given the same 
      conditions as in Scenario 1, and setting comment to 'test comment',
      and attempting to save a review 'save review',
      I expect that [enable_save_review()] is set to 'FALSE',
      and that the review database remains unchanged.",
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        old_review_table <- db_temp_connect(temp_path, {
          DBI::dbGetQuery(con, "SELECT * FROM all_review_data")
        })
        testargs <- list(
          r = reactiveValues(
            user_name = "test_name",
            user_role = "Medical Monitor",
            subject_id = "885",
            review_data = do.call(reactiveValues, split_review_data(temp_path))
          ),
          active_form = reactiveVal("Adverse events"),
          active_tab = reactiveVal("Common forms"),
          review_required_data = data.frame(
            "item_group" = "Adverse events", 
            "review_required" = TRUE
          ),
          db_path = temp_path
        )
        
        testServer(mod_review_forms_server, args = testargs, {
          ns <- session$ns
          session$setInputs(form_reviewed = FALSE, add_comment = "test comment")
          expect_false(enable_save_review())
          session$setInputs(save_review = 1)
          new_review_table <- db_temp_connect(db_path, {
            DBI::dbGetQuery(con, "SELECT * FROM all_review_data")
          })
          expect_error(output[["save_review_error"]], "Requires review")
          expect_equal(old_review_table, new_review_table)
        })
      }
    )
    it(
      "Scenario 3 - No data to review. Given [active_form] set to a 
        form of which no data is available named [no_data_form],
        and that I try to save a review by setting [save_review] to 1,
        I expect that a warning message will be displayed with the text [Nothing to review],
        and that no new information is saved to the database,
        and that no new information is saved to the app data frame.",
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        testargs <- list(
          r = reactiveValues(
            user_name = "test_name",
            user_role = "Medical Monitor",
            subject_id = "885",
            review_data = do.call(reactiveValues, split_review_data(temp_path))
          ),
          active_form = reactiveVal("Adverse events"),
          active_tab = reactiveVal("Common forms"),
          review_required_data = data.frame(
            "item_group" = c("Adverse events", "no_data_form"),
            "review_required" = TRUE
          ),
          db_path = temp_path
        )
        testServer(
          mod_review_forms_server, args = testargs, {
            ns <- session$ns
            active_form("no_data_form")
            data_before_saving <- reactiveValuesToList(r$review_data)
            db_before_saving <- split_review_data(db_path)
            expect_equal(db_before_saving, data_before_saving)
            
            session$setInputs(save_review = 1)
            expect_error(output[["save_review_error"]], "Nothing to review")
            expect_equal(reactiveValuesToList(r$review_data), data_before_saving)
            db_after_saving <- split_review_data(db_path)
            expect_equal(db_after_saving, data_before_saving)
          })
      }
    )
  }
)
describe(
  "mod_review_forms. Feature 4 | Only allow to save review with valid user name 
    and role. 
    As a user, I want that saving data is only 
    possible with a valid user name", 
  {
    it(
      "Scenario 1 - Trying to save data without user name. Given 
        a data frame and a database with review data with [reviewed] status set 
        to 'new' (not reviewed yet), 
        and [subject_id]  set to '885', 
        and [active_form] set to 'Adverse events', 
        and no [user_name] available, 
        and setting the value [form_review] is to 'TRUE' and clicking on the 
        [save_review] button, 
        I expect that an error message will be displayed, 
        and that the review data of the selected subject and form is unaltered.", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        test_ui <- function(request){
          tagList(
            shinyjs::useShinyjs(),
            bslib::page_navbar(sidebar = bslib::sidebar(mod_review_forms_ui("test")))
          )
        }
        test_server <- function(input, output, session){
          mod_review_forms_server(
            id = "test", 
            r = reactiveValues(
              user_name = NULL,
              user_role = "Medical Monitor",
              subject_id = "885", 
              review_data = do.call(reactiveValues, split_review_data(temp_path))
            ),
            active_form = reactiveVal("Adverse events"),
            active_tab = reactiveVal("Common events"),
            review_required_data = data.frame(
              "item_group" = "Adverse events", 
              "review_required" = TRUE
            ),
            db_path = temp_path
          )
        }
        test_app <- shinyApp(test_ui, test_server, options = list("test.mode" = TRUE))
        app <- shinytest2::AppDriver$new(
          app_dir = test_app,
          name = "test-mod_review_forms_no_user",
          timeout = 8000,
          width = 1619,
          height = 955
        )
        withr::defer(app$stop())
        db_before_saving <- db_get_table(temp_path)
        
        app$wait_for_idle(2500)
        app$click("test-form_reviewed")
        app$click("test-save_review")  
        
        app$wait_for_idle()
        app$expect_values()
        
        # review status and reviewer is saved as expected
        db_after_saving <- db_get_table(temp_path)
        expect_equal(db_after_saving, db_before_saving)
      }
    )   
    
  }
)

describe(
  "Feature 5 | Ensure data in memory remains in synch with the database. 
    As a user, I want that data in memory remains the same as the one 
    in the database, even if an error occurs when saving data to the database", 
  {
    it(
      "Scenario 1 - Database save function not working. 
      Given test review data,
      and the function 'db_save_review' being mocked (temporarily replaced) with a
      function that does not write to the database,
      and [user_name] set to '885',
      and [active_form] to 'Adverse events',
      and [active_tab] to 'Common forms',
      and [form_reviewed] to 'TRUE',
      and [save_review] to '1',
      I expect that [save_review_error] is TRUE,
      and that the new review data is not saved in memory,
      and that the rows of [review_data] in the database of subject 885 are 
      still marked as not reviewed", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        rev_data <- do.call(reactiveValues, split_review_data(temp_path))
        local_mocked_bindings(
          db_save_review = function(...) "no data saved in database"
        )
        testargs <- list(
          r = reactiveValues(
            user_name = "test_name",
            user_role = "Medical Monitor",
            subject_id = "885",
            review_data = rev_data
          ),
          active_form = reactiveVal("Adverse events"),
          active_tab = reactiveVal("Common forms"),
          review_required_data = data.frame(
            "item_group" = "Adverse events", 
            "review_required" = TRUE
          ),
          db_path = temp_path
        )
        testServer(
          mod_review_forms_server, args = testargs, {
            ns <- session$ns
            
            session$userData$review_records <- reactiveValues()
            session$userData$update_checkboxes <- reactiveValues()
            
            session$setInputs(form_reviewed = NULL)
            db_before_saving <- db_get_table(db_path)
            session$setInputs(form_reviewed = TRUE, save_review = 1)
            db_after_saving <- db_get_table(db_path)

            expect_true(review_save_error())
            expect_equal(
              reactiveValuesToList(r$review_data), 
              reactiveValuesToList(rev_data)
              )
            expect_equal(db_after_saving, db_before_saving)
          })
      }
    )
  }
)

describe(
  "mod_review_forms. Feature 6 | Restrict right to review forms to roles 
    specified in the app configuration. 
    As an admin, I want to be able to restrict the 
    right to review forms to the roles specified in the config file.", 
  {
    it(
      "Scenario 1 - Role without review privileges. 
        Given the [user_name] 'test_user',
        and the unprivileged user_role 'restricted_role',
        I expect that all the review options are disabled,
        and that in the review_error output a message is shown that review is 
        not allowed for the user's active role.
      ", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        test_ui <- function(request){
          tagList(
            shinyjs::useShinyjs(),
            bslib::page_navbar(sidebar = bslib::sidebar(mod_review_forms_ui("test")))
          )
        }
        test_server <- function(input, output, session){
          mod_review_forms_server(
            id = "test",
            r = reactiveValues(
              user_name = "test_name",
              user_role = "restricted_role",
              subject_id = "885",
              review_data = do.call(reactiveValues, split_review_data(temp_path))
            ),
            active_form = reactiveVal("Adverse events"),
            active_tab = reactiveVal("Common events"),
            review_required_data = data.frame(
              "item_group" = "Adverse events", 
              "review_required" = TRUE
            ),
            db_path = temp_path
          )
        }
        test_app <- shinyApp(test_ui, test_server)
        app <- shinytest2::AppDriver$new(
          app_dir = test_app,
          name = "test-mod_review_forms",
          timeout = 8000,
          width = 1619,
          height = 955
        )
        withr::defer(app$stop())
        app$wait_for_idle(2500)
        # save button and comment option should not be available:
        expect_true(app$get_js("document.getElementById('test-save_review').disabled;"))
        expect_true(app$get_js("document.getElementById('test-add_comment').disabled;"))
        expect_true(app$get_js("document.getElementById('test-review_comment').disabled;"))
        expect_true(app$get_js("document.getElementById('test-form_reviewed').disabled;"))
        
        # correct error message is shown in the output:
        expect_equal(
          app$get_value(output = "test-save_review_error")$message, 
          "Review not allowed for a 'restricted_role'."
        )
      }
    )
  }
)
