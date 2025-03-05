#' App Server
#'
#' Main server of the Shiny application.
#'
#' Prepares the app data and connects all the Shiny modules. Main modules used
#' in the application are the `common_form` pages ([mod_common_forms_server()]),
#' `study form` pages ([mod_study_forms_server()]), the `sidebar`
#' ([mod_main_sidebar_server()]), the `start page` ([mod_start_page_server()]),
#' the `header widgets` ([mod_header_widgets_server()]), and the `query page`
#' ([mod_queries_server()])
#'
#' @param input,output,session Internal parameters for `shiny`.
#' @seealso [app_ui()], [run_app()]
#' 
app_server <- function(
    input, 
    output, 
    session
){
  meta <- golem::get_golem_options("meta")
  merged_data <- golem::get_golem_options("data")
  user_db <- golem::get_golem_options("user_db")
  credentials_db <- golem::get_golem_options("credentials_db")
  
  app_data <- get_appdata(merged_data, meta = meta)
  app_vars <- get_meta_vars(data = app_data, meta = meta)
  app_tables <- lapply(
    setNames(names(app_data), names(app_data)), \(x){
      create_table(app_data[[x]], expected_columns = names(app_vars$items[[x]]))
    })
  check_appdata(app_data, meta)
  
  session$userData$review_records <- reactiveValues()
  session$userData$update_checkboxes <- reactiveValues()
  
  res_auth <- authenticate_server(
    all_sites = app_vars$Sites$site_code, 
    credentials_db = credentials_db,
    credentials_pwd = golem::get_golem_options("credentials_pwd"),
    session = session
  )
  
  # For query item selector drop-down menus:
  available_data <- get_available_data(
    data = app_data,
    tables = app_tables,
    all_forms = app_vars$all_forms,
    form_repeat_name = with(
      meta[["table_names"]], 
      table_name[raw_name == "form_repeat"]
      ) |> 
      tryCatch(error = \(e) "N")
  )
  
  # For summary review data:
  static_overview_data <- get_static_overview_data(
    data = app_data,
    expected_general_columns = unique(
      with(meta$items_expanded, item_name[item_group == "General"])
    )
  )
  # think of using the pool package, but functions such as row_update are not yet supported.
  r <- reactiveValues(
    review_data       = do.call(reactiveValues, split_review_data(user_db, forms = app_vars$all_forms$form)),
    query_data        = collect_query_data(user_db),
    filtered_subjects = app_vars$subject_id,
    filtered_data     = app_data,
    filtered_tables   = app_tables,
    subject_id        = app_vars$subject_id[1]
  )
  
  user_error <- reactiveVal()
  observeEvent(res_auth, {
    if(identical(get_golem_config("user_identification"), "shinymanager")){
    req(res_auth[["user"]])
    }
    r$user_name <- res_auth[["name"]] %||% res_auth[["user"]] %||% ""
    r$user_roles  <- names(get_valid_roles(res_auth[["roles"]])) %||% ""
    r$user_role   <- r$user_roles[1]
    user_error(NULL)
    if(r$user_name == ""){
      user_error("No valid user name provided. ")
    } 
    if(r$user_role == ""){
      user_error(paste0(user_error(), "No valid user role provided. "))
    }
    if(!is.null(user_error())){
      user_error(
        paste0(
          user_error(), 
          "Functionality is limited. ",
          "Please contact the administrator to resolve this issue."
        )
      )
    }
  })
  
  forms_to_review_data <- app_vars$form_level_data[c("item_group", "review_required")] 
  
  observeEvent(user_error(), {
    showNotification(
      user_error(), 
      id = "user_error", 
      type = "error",  
      duration = NULL
    )
  })
  
  output$user_info <- renderText({
    req(r$user_name)
    r$user_name
  })
  rev_sites <- reactive({res_auth[["sites"]]})
  observeEvent(rev_sites(), {
    req(!all(rev_sites() %in% app_vars$Sites$site_code))
    r <- filter_data(r, rev_sites(), subject_ids = app_vars$subject_id,
                     appdata = app_data, apptables = app_tables)
  })
  
  navinfo <- reactiveValues(
    active_form       = app_vars$all_forms$form[1],
    active_tab        = "Start",
    trigger_page_change = 1
  )
  
  rev_data <- reactiveValues(
    summary = reactive({
      req(forms_to_review_data)
      r$review_data |>
        reactiveValuesToList() |> 
        do.call(what = rbind) |> 
        dplyr::left_join(forms_to_review_data, by = "item_group") |> 
        dplyr::filter(
          reviewed != "Yes",
          review_required,
          subject_id %in% r$filtered_subjects
        ) |>
        summarize_review_data() |>
        dplyr::select(subject_id, "Form" = item_group, "Event" = event_name,
                      "Edit date" = edit_date_time, status, reviewed)
    }),
    overview = reactive({
      static_overview_data |>
        dplyr::filter(subject_id %in% r$filtered_subjects) |>
        dplyr::mutate(
          needs_review = subject_id %in% unique(rev_data$summary()$subject_id)
        ) |> 
        dplyr::arrange(dplyr::desc(needs_review), subject_id)
    }),
    subject = app_vars$subject_id[1],
    open_modal = 0,
    show_all = FALSE
  )
  
  observeEvent(input$main_tabs, {
    golem::cat_dev("Changed main tab to", input$main_tabs, "\n")
    # This observeEvent should only be triggered by directly clicking on tabs.
    # If tabs are changed programmatically, all variables should already be set
    # correctly in the designated change tab modules.
    req(input$main_tabs != navinfo$active_tab)
    navinfo$active_tab <- input$main_tabs
    
    if(input$main_tabs == "Common events"){
      req(input$common_data_tabs)
      navinfo$active_form <- input$common_data_tabs
    } else if(input$main_tabs == "Study data"){
      req(input$study_data_tabs)
      navinfo$active_form <- input$study_data_tabs} else
        return(golem::cat_dev("\n"))
    cat("Changed 'active_form' to", navinfo$active_form, "\n\n")
  })
  # the two observeEvents below are needed to keep the active_form updated if
  # the event above is not triggered (by clicking on a different form in the same main_tab tabset).
  # the value will be used to update the current tab in the main sidebar.
  observeEvent(input$common_data_tabs, {
    navinfo$active_form <- input$common_data_tabs
    cat("Common tab switch --> 'active_form' updated to ", navinfo$active_form, "\n")
  })
  
  observeEvent(input$study_data_tabs, {
    if(input$main_tabs == "Study data"){
      navinfo$active_form <- input$study_data_tabs
      golem::cat_dev("Study tab switch --> 'active_form' updated to",
                     navinfo$active_form, "\n")
    }
  })
  
  observeEvent(navinfo$trigger_page_change, {
    req(input$common_data_tabs, input$study_data_tabs)
    # trigger_page_change prevents circular dependencies.
    # If navinfo$active_tab would change input$main_tab directly, it would
    # conflict with the observeEvent above where a change in input$main_tabs
    # also changes navinfo$active_tab.
    req(navinfo$trigger_page_change > 1)
    golem::cat_dev("trigger new selection in main server\n")
    bslib::nav_select(id = "main_tabs", selected = navinfo$active_tab)
    id_to_change <- switch(navinfo$active_tab,
                           "Common events" = "common_data_tabs",
                           "Study data"    = "study_data_tabs")
    bslib::nav_select(id = id_to_change, selected = navinfo$active_form)
  })
  
  timeline_data <- reactive({
    get_timeline_data(
      r$filtered_data, 
      r$filtered_tables, 
      treatment_label = meta$settings$treatment_label %||% "\U1F48A T\U2093"
    )
  })
  
  ###### Load common form tabs in UI and server:
  common_forms <- with(app_vars$all_forms, form[main_tab == "Common events"])
  lapply(common_forms, \(i){
    bslib::nav_insert(
      id = "common_data_tabs",
      mod_common_forms_ui(id = paste0("cf_", simplify_string(i)), form = i),
      select = (i == common_forms[1])
    )
  })
  lapply(common_forms, \(x){
    mod_common_forms_server(
      id = paste0("cf_", simplify_string(x)), 
      form = x,
      form_data = reactive(r$filtered_data[[x]]), 
      form_review_data = reactive(r$review_data[[x]]), 
      form_items = app_vars$items[[x]], 
      active_subject = reactive(r$subject_id),
      table_names = app_vars$table_names, 
      timeline_data = timeline_data
    ) 
  }) |>
    unlist(recursive = FALSE)
  
  ###### Load study form tabs in UI and server:
  study_forms <- with(app_vars$all_forms, form[main_tab == "Study data"])
  lapply(study_forms, \(i){
    bslib::nav_insert(
      id = "study_data_tabs",
      mod_study_forms_ui(paste0("sf_", simplify_string(i)), form = i,
                         form_items = app_vars$items[[i]]),
      select = (i == study_forms[1])
    )
  })
  lapply(study_forms, \(x){
    mod_study_forms_server(
      id = paste0("sf_", simplify_string(x)), 
      form = x,
      form_data = reactive(r$filtered_data[[x]]), 
      form_review_data = reactive(r$review_data[[x]]), 
      form_items = app_vars$items[[x]], 
      active_subject = reactive(r$subject_id),
      table_names = app_vars$table_names,
      item_info = app_vars$form_level_data[app_vars$form_level_data$item_group == x, ]
    ) 
  }) |>
    unlist(recursive = FALSE)
  
  mod_start_page_server("start_page_1", r, rev_data, navinfo, app_vars$all_forms,
                        app_vars$table_names)
  mod_header_widgets_server(
    id = "header_widgets_1", 
    r = r, 
    rev_data = rev_data, 
    navinfo = navinfo
  )
  
  
  # Only initiate the sidebar after successful login, because it contains a
  # modal that pops up if data is out of synch. Modals interfere with shinymanager.
  observeEvent(r$user_name, {
    if(isTRUE(get_golem_config("user_identification") == "shinymanager")){
      req(rlang::is_installed("shinymanager"))
      pwd_mngt <- shinymanager::read_db_decrypt(
        get_db_connection(credentials_db), 
        name = "pwd_mngt",
        passphrase = golem::get_golem_options("credentials_pwd")
      )
      req(with(pwd_mngt, must_change[user == res_auth[["user"]]]) == "FALSE") 
    }
    
    mod_main_sidebar_server(
      id = "main_sidebar_1",
      r = r,
      app_data = app_data,
      app_tables = app_tables,
      app_vars = app_vars,
      navinfo,
      forms_to_review = reactive({
        with(rev_data$summary(), Form[subject_id == r$subject_id])
      }),
      db_path = user_db,
      available_data = available_data
    )
  })
  
  mod_review_config_server(
    "review_config_1",
    r = r,
    app_data = app_data,
    app_tables = app_tables,
    sites = app_vars$Sites,
    subject_ids = app_vars$subject_id
  )
  
  mod_queries_server(
    "queries_1",
    r = r,
    navinfo = navinfo,
    all_forms = app_vars$all_forms,
    db_path = user_db,
    table_names = app_vars$table_names
  )
  mod_report_server("report_1", r = r, rev_data, db_path = user_db,
                    table_names = app_vars$table_names)
  
  mod_navigate_participants_server("navigate_participants_1", r)
  
  mod_navigate_review_server(
    "navigate_review_1",
    r,
    rev_data,
    navinfo,
    app_vars$all_forms,
    table_names = app_vars$table_names
  )
  shiny::exportTestValues(
    user_db = user_db,
    active_participant = r$subject_id,
    active_form = navinfo$active_form,
    user_error = user_error()
  )
}
