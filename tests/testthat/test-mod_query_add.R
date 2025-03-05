describe(
  "mod_query_add. Feature 1 | Load application module in isolation.", 
  {
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_query_add_ui(id = "test")
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
          query_data = data.frame(),
          user_name = "",
          subject_id = ""
        ),
        active_form = reactiveVal(),
        db_path = "",
        available_data = data.frame()
      ) 
      testServer(mod_query_add_server, args = testargs , {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  }
)

describe(
  "mod_query_add. Feature 2 | Create and save a query. As a user, I want to be able 
  to create a query and save it to the remote database and to the internal data frame. 
  Together with the query text, I want to be able to save other mandatory information, 
  such as query type, the name of the reviewer, a timestamp, the ID of the participant, and 
  the form at which the query is applicable. Optional other information that I 
  want to be able to save with the query is the name of the item in question.", 
  {
    query_df <- data.frame(
      "subject_id"    = c("ID1"),
      "type"          = c("Normal", "Major"),
      "event_label"   = c("Visit 1"),
      "item_group"    = c("Vital signs", "Adverse events"),
      "item"          = c("Pulse", "Sepsis"),
      "timestamp"     = c("2023-01-01 01:01:01 UTC", "2023-11-02 01:01:01 UTC"),
      "query_id"      = c("ID1-unique_id", "ID2-unique_id"),
      "n"             = c(1),     
      "reviewer"      = c("Test author (Medical Monitor)", "Author3 (Medical MOnitor)"),
      "query"         = c("Query text test.", "Scoring correct? Please verify"),
      "resolved"      = c("No"),
      "resolved_date" = NA_character_,
      "edit_reason"   = NA_character_
    )
    
    it(
      "Scenario 1 - The currently active form information is correct. Given 
        [subject_id] set to 'ID1', 
        and [active_form] consecutively set to 'Vital signs' and 'Adverse events', 
        I expect that the data frame [selected_data] shows a data frame with 
        available data of [subject_id] 'ID1' and [active_form] but filtered on 
        the [subject_id] and [active_form()]", 
      {
        testargs <- list(
          r = reactiveValues(
            query_data = query_df,
            user_name = "Admin test",
            subject_id = "ID1"
          ),
          active_form = reactiveVal(),
          db_path = "",
          available_data = data.frame(
            subject_id = c("ID1", "ID1", "ID2"),
            item_name = c("item1", "Sepsis", "Oxycodon"), 
            item_group = c("Vital signs", "Adverse events", "Medication"),
            event_name  = c("Visit 1", "Any visit", "Any visit")
          )
        ) 
        
        testServer(mod_query_add_server, args = testargs, {
          ns <- session$ns
          r$subject_id <- "ID1"
          active_form("Vital signs")
          session$setInputs(create_query = 1)
          expect_equal(
            selected_data(), 
            with(available_data, available_data[subject_id == "ID1" & item_group == "Vital signs", ])
          )
          active_form("Adverse events")
          session$setInputs(create_query = 2)
          expect_equal(
            selected_data(), 
            with(available_data, available_data[subject_id == "ID1" & item_group == "Adverse events", ])
          )
        })
      }
    )
    it(
      "Scenario 2 - A query can be saved successfully. 
        Given [subject_id] is set to 'ID2', 
        and [active_form()] is set to 'Medication',
        and [query_text] is 'Add a new test query',
        and [query_select_visit] is set to 'Visit 1',
        and [query_select_item] is set to 'Oxycodon',
        and [query_major] is set to 'FALSE',
        and [query_add_input] is set to 1 (indicating a button click),
        I expect that one new query is saved in [query_data] for patent 'ID1',
          and that the 'item_group' of the query is 'Medication',
          and 'item' is 'Oxycodon', 
          and 'reviewer' is 'User 1' with the role 'Medical Monitor',
          and 'query' is 'Add a new test query',
          and 'resolved' is 'No',
          and the remote database contains the same query as in [query_data].", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite") 
        con <- get_db_connection(temp_path)
        DBI::dbWriteTable(con, "query_data", query_df)
        
        testargs <- list(
          r = reactiveValues(
            query_data = query_df,
            user_name = "User 1",
            user_role = "Medical Monitor",
            subject_id = "ID2"
          ),
          active_form = reactiveVal("Medication"),
          db_path = temp_path,
          available_data = data.frame(
            subject_id = c("ID1", "ID1", "ID2"),
            item_name = c("item1", "Sepsis", "Oxycodon"), 
            item_group = c("Vital signs", "Adverse events", "Medication"),
            event_name  = c("Visit 1", "Any visit", "Any visit")
          )
        ) 
        
        testServer(mod_query_add_server, args = testargs, {
          ns <- session$ns
          session$setInputs(
            create_query = 1,
            query_text = "Add a new test query",
            query_select_visit = "Visit 1",
            query_select_item = "Oxycodon",
            query_add_input = 1,
            query_major = FALSE
          )
          new_query <- dplyr::filter(r$query_data, subject_id == "ID2")
          expect_equal(nrow(new_query), 1)
          expect_equal(new_query$item_group, "Medication")
          expect_equal(new_query$item, "Oxycodon")
          expect_equal(new_query$reviewer, "User 1 (Medical Monitor)")
          expect_equal(new_query$query, "Add a new test query")
          expect_equal(new_query$resolved, "No")
          expect_equal(
            dplyr::as_tibble(r$query_data),
            dplyr::collect(dplyr::tbl(con, "query_data"))
          )
        })
      }
    )
    
    it(
      "Scenario 3 - Save a major query. 
        Given [subject_id] is set to 'ID3', 
        and [active_form()] is set to 'Adverse events',
        and [query_text] is 'Major query text.',
        and [query_select_visit] is set to 'Visit 1',
        and [query_select_item] is set to 'Pneumothorax',
        and [query_major] is set to 'TRUE',
        and [query_add_input] is set to 1 (indicating a button click),
        I expect that one new query is saved in [query_data] for patent 'ID3',
          and that the 'item_group' of the query is 'Adverse events',
          and 'item' is 'Pneumothorax', 
          and 'reviewer' is 'User 1' with the role 'Medical Monitor',
          and 'query' is 'Major query text.',
          and 'resolved' is 'No',
          and the remote database contains the same query as in [query_data].", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite") 
        con <- get_db_connection(temp_path)
        DBI::dbWriteTable(con, "query_data", query_df)
        
        testargs <- list(
          r = reactiveValues(
            query_data = query_df,
            user_name = "User 1",
            user_role = "Medical Monitor",
            subject_id = "ID3"
          ),
          active_form = reactiveVal("Adverse events"),
          db_path = temp_path,
          available_data = data.frame(
            subject_id = c("ID1", "ID3", "ID2"),
            item_name = c("item1", "Pneumothorax", "Oxycodon"), 
            item_group = c("Vital signs", "Adverse events", "Medication"),
            event_name  = c("Visit 1", "Any visit", "Any visit")
          )
        ) 
        
        testServer(mod_query_add_server, args = testargs, {
          ns <- session$ns
          session$setInputs(
            create_query = 1,
            query_text = "Major query text.",
            query_select_visit = "Any visit",
            query_select_item = "Pneumothorax",
            query_add_input = 1,
            query_major = TRUE
          )
          new_query <- with(r$query_data, r$query_data[
            subject_id == "ID3" & item == "Pneumothorax", ])
          expect_equal(nrow(new_query), 1)
          expect_equal(new_query$item_group, "Adverse events")
          expect_equal(new_query$item, "Pneumothorax")
          expect_equal(new_query$reviewer, "User 1 (Medical Monitor)")
          expect_equal(new_query$query, "Major query text.")
          expect_equal(new_query$resolved, "No")
          expect_equal(
            dplyr::as_tibble(r$query_data),
            dplyr::collect(dplyr::tbl(con, "query_data"))
          )
        })
      }
    )
  }
)

describe(
  "mod_query_add. Feature 3 | Error if query entry in app does not match database. 
    As a user, I want to be able to see an error
    message if the latest query entry does not match the one in the database
    after saving a new entry.",
  {
    it(
    "Scenario 1 - Database save function not working. 
      Given a test data base, 
      and the function 'db_save' being mocked (temporarily replaced) with a
      function that does not write to the database,
      and [user_name] set to 'test user',
      and [active_form] to 'Adverse events',
      and [subject_id] to '885',
      and [query_text] set to 'Test query',
      and [query_select_visit] to 'Any visit',
      and [create_query] to 1,
      and [query_add_input] to 1,
      I expect that [save_review_error] is TRUE,
      and that the new query is not saved in memory,
      and that [query_data] remains the same as the input test query data.", 
    {
      temp_path <- withr::local_tempfile(fileext = ".sqlite")
      file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path)
      query_df <- readRDS(test_path("fixtures", "query_testdata.rds"))
      db_temp_connect(temp_path, DBI::dbWriteTable(con, "query_data", query_df))

      local_mocked_bindings(
        db_save = function(...) "nothing written to database"
      )
      testargs <- list(
        r = reactiveValues(
          user_name = "test user",
          user_role = "Medical Monitor",
          subject_id = 885,
          review_data = db_get_table(temp_path)
        ),
        active_form = reactiveVal("Adverse events"),
        db_path = temp_path,
        available_data = data.frame(subject_id = "885",
                                    item_group = "Adverse events")
      )
      testServer(mod_query_add_server, args = testargs, {
        ns <- session$ns
        session$setInputs(
          create_query = 1,
          query_text = "Test query",
          query_select_visit = "Any visit",
          query_add_input = 1,
          query_major = FALSE
        )
        new_query <- dplyr::filter(r$query_data, subject_id == "885", 
                                   query == "test query")
        expect_true(query_save_error())
        expect_equal(nrow(new_query), 0)
        expect_equal(r$query_data, dplyr::as_tibble(query_df))
      })
    })
  }
)

describe(
  "mod_query_add. Feature 4 | Only allow to add query with valid user name and role. 
    As a user, I want that saving a query is only 
    possible with a valid user name and role", 
  {
    it(
      "Scenario 1 - Trying to save a query without user name. Given 
        a data frame and a database with review data with [reviewed] status set 
        to 'new' (not reviewed yet), 
        and no [user_name] available,
        and [subject_id]  set to '885', 
        and [active_form] set to 'Adverse events', 
        and no [user_role] set to 'Medical Monitor', 
        and setting the value [form_review] is to 'TRUE' and clicking on the 
        [save_review] button, 
        I expect that a query_error will be shown, 
        and that no [query_data] in the application and in the database 
        is unchanged.", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path)
        query_df <- readRDS(test_path("fixtures", "query_testdata.rds"))
        db_temp_connect(temp_path, DBI::dbWriteTable(con, "query_data", query_df))
        
        testargs <- list(
          r = reactiveValues(
            user_name = "",
            user_role = "Medical Monitor",
            subject_id = 885,
            review_data = db_get_table(temp_path)
          ),
          active_form = reactiveVal("Adverse events"),
          db_path = temp_path,
          available_data = data.frame(subject_id = "885",
                                      item_group = "Adverse events")
        )
        testServer(mod_query_add_server, args = testargs, {
          ns <- session$ns
          session$setInputs(
            create_query = 1,
            query_text = "Test query",
            query_select_visit = "Any visit",
            query_add_input = 1,
            query_major = FALSE
          )
          expect_error(
            output$query_error, 
            "User name missing. Cannot save query anonymously"
          )
          
          expect_true(is.null(r$query_data))
          expect_equal(
            db_temp_connect(
              db_path, 
              DBI::dbGetQuery(con, "SELECT * FROM query_data")
            ),
            query_df
          )
        })
      }
    )
    it(
      "Scenario 2 - Trying to save a query without user role. Given 
        the same conditions as in scenario 1, 
        but user_name now set to 'test user',
        and no user role available,
        and trying to save a review,
        I expect that a query_error will be shown, 
        and that no [query_data] in the application and in the database 
        is unchanged.", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path)
        query_df <- readRDS(test_path("fixtures", "query_testdata.rds"))
        db_temp_connect(temp_path, DBI::dbWriteTable(con, "query_data", query_df))
        
        testargs <- list(
          r = reactiveValues(
            user_name = "test user",
            user_role = "",
            subject_id = 885,
            review_data = db_get_table(temp_path)
          ),
          active_form = reactiveVal("Adverse events"),
          db_path = temp_path,
          available_data = data.frame(subject_id = "885",
                                      item_group = "Adverse events")
        )
        testServer(mod_query_add_server, args = testargs, {
          ns <- session$ns
          session$setInputs(
            create_query = 1,
            query_text = "Test query",
            query_select_visit = "Any visit",
            query_add_input = 1,
            query_major = FALSE
          )
          expect_error(
            output$query_error, 
            "User role missing. Cannot save query without user role"
          )
          expect_true(is.null(r$query_data))
          expect_equal(
            db_temp_connect(
              db_path, 
              DBI::dbGetQuery(con, "SELECT * FROM query_data")
            ),
            query_df
          )
        })
      }
    )
  }
)   
