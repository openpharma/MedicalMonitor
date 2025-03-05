describe("clean_event_metadata() works", {
  it("cleans event metadata as expected", {
    df <- data.frame(
      event_id = c("SCR", "START", NA, "FU1", "FU2", "EXIT", NA),
      event_id_pattern = c(NA, NA, "^V[[:digit:]]+$", NA, NA, NA, "^UNV"),
      is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
      is_baseline_event = c(TRUE, rep(FALSE, times = 6)),
      event_name_custom = c(NA, NA, "Visit", NA, NA, NA, "UV"),
      event_label_custom = c(NA, NA, "Vis", NA, NA, NA, NA)
    )
    output <- clean_event_metadata(df)
    expect_equal(
      names(output), 
      c(names(df), "generate_labels", "meta_event_order", "add_visit_number", 
        "add_event_repeat_number")
    )
    expect_true(is.logical(output$add_visit_number))
    expect_true(is.logical(output$add_event_repeat_number))
    expect_true(is.logical(output$generate_labels))
    expect_true(is.logical(output$is_baseline_event))
    expect_equal(output$meta_event_order, 1:7)
  })
  it("works with only event_id and adds other required columns", {
    df <- data.frame(event_id = c("SCR", "START", "EXIT"))
    output <- dplyr::as_tibble(clean_event_metadata(df))
    expect_snapshot(print(output, width = Inf))
  })
  it("creates event_id_pattern based on event_id if event_id_pattern is missing", {
    df <- data.frame(event_id = c("SCR", "START", "EXIT"))
    output <- clean_event_metadata(df)
    expect_equal(output$event_id_pattern, paste0("^", df$event_id, "$"))
  })
  
  it("marks all visits as regular visit if the column is_regular_visit is missing", {
    df <- data.frame(event_id = c("SCR", "Vis1", "Vis2"))
    output <- clean_event_metadata(df)
    expect_true("is_regular_visit" %in% names(output))
    expect_equal(output$is_regular_visit, c(TRUE, TRUE, TRUE))
  })
  it("marks first regular visit as baseline if the column is_baseline_event is missing", {
    df <- data.frame(
      event_id = c("SCR", "Vis1", "Vis2"),
      is_regular_visit= c(FALSE, TRUE, TRUE)
    )
    output <- clean_event_metadata(df)
    expect_true("is_baseline_event" %in% names(output))
    expect_equal(output$is_baseline_event, c(FALSE, TRUE, FALSE))
  })
  it("errors if more than one baseline event is defined", {
    df <- data.frame(
      event_id = c("SCR", "Vis1", "Vis2"),
      is_regular_visit= TRUE,
      is_baseline_event = c(TRUE, TRUE, FALSE)
    )
    expect_error(
      clean_event_metadata(df),
      "Only one baseline event allowed"
    )
  })
  it("errors if both the columns event_id and event_id_pattern are missing", {
    df <- data.frame(
      is_regular_visit= TRUE,
      is_baseline_event = c(TRUE, FALSE)
    )
    expect_error(
      clean_event_metadata(df),
      "At least one of the columns 'event_id' or 'event_id_pattern' must be available"
    )
  })
  it("errors if values are missing in both event_id and event_id_patttern", {
    df <- data.frame(event_id = c("SCR", NA, "VIS1"), event_id_pattern = NA)
    expect_error(
      clean_event_metadata(df),
      "values in 'event_id' and 'event_id_pattern' cannot both be missing"
    )
    df <- data.frame(
      event_id = c("SCR", NA, NA),
      event_id_pattern = c(NA, NA, "^VIS")
    )
    expect_error(
      clean_event_metadata(df),
      "values in 'event_id' and 'event_id_pattern' cannot both be missing"
    )
  })
})

describe("add_timevars_to_data() works", {
  it("Adds time variables as expected", {
    event_data <- clean_event_metadata(data.frame(event_id = "SCR"))
    time_data <- data.frame(
      site_code = c("site1"),
      subject_id = c("subj1"),
      event_id = c("SCR", "event2", "event3"),
      event_date = c("2025-01-01", "2025-01-04", "2025-01-06"),
      edit_date_time = "2024-01-01"
    )
    output <- add_timevars_to_data(time_data, event_data)
    expected_output <- time_data |> 
      dplyr::mutate(
        event_date = as.Date(event_date),
        edit_date_time = as.POSIXct(edit_date_time, tz = "UTC"),
        day = event_date - as.Date("2025-01-01")
      )
    expect_equal(output, expected_output)
  })
  it("uses the oldest date as baseline if no baseline event is found in the data", {
    event_data <- data.frame(event_id = "SCR", is_baseline_event = TRUE)
    time_data <- data.frame(
      site_code = c("site1"),
      subject_id = c("subj1"),
      event_id = c("event2", "eventx", "event3"),
      event_date = c("2025-02-02", "2025-01-01", "2025-01-06"),
      edit_date_time = "2024-01-01"
    )
    output <- add_timevars_to_data(time_data, event_data)
    expected_output <- time_data |> 
      dplyr::mutate(
        event_date = as.Date(event_date),
        edit_date_time = as.POSIXct(edit_date_time, tz = "UTC"),
        day = event_date - as.Date("2025-01-01")
      )
    expect_equal(output, expected_output)
  })
  
  it("Warns and returns early if empty data frame is provided", {
    event_data <- data.frame(event_id = "SCR", is_baseline_event = TRUE)
    df <- data.frame(matrix(nrow = 0, ncol = length(required_col_names))) |> 
      lapply(as.character) |> as.data.frame()
    names(df) <- required_col_names
    expect_warning(
      add_timevars_to_data(df, event_data),
      "Empty data frame provided"
    )
  })
  it("errors if required columns are missing", {
    event_data <- data.frame(event_id = c("SCR", "Vis1", "Vis2")) |>
      clean_event_metadata()
    expect_error(
      add_timevars_to_data(mtcars, event_data),
      "The following columns are missing while they are required"
    )
  })
  it("errors with incorrect events input", {
    event_data <- data.frame(event_id = "SCR", is_baseline_event = TRUE)
    time_data <- data.frame(
      site_code = c("site1"),
      subject_id = c("subj1"),
      event_id = c("SCR", "event2", "event3"),
      event_date = c("2025-01-01", "2025-01-04", "2025-01-06"),
      edit_date_time = "2024-01-01"
    )
    expect_error(add_timevars_to_data(time_data, c("")))
    expect_error(add_timevars_to_data(time_data, data.frame()))
    expect_error(
      add_timevars_to_data(
        time_data, 
        data.frame(event_id = TRUE, is_baseline_event = TRUE)
      )
    )
    expect_error(
      add_timevars_to_data(
        time_data, 
        data.frame(event_id = "SCR", is_baseline_event = " incorrect")
      )
    )
    expect_error(
      add_timevars_to_data(
        time_data, 
        data.frame(event_id = "SCR", is_baseline_event = c(TRUE, TRUE))
      )
    )
    expect_error(
      add_timevars_to_data(
        time_data, 
        data.frame(event_id = "SCR", is_baseline_event = FALSE)
      )
    )
  })
  it("returns early if a day column already exists in the data", {
    event_data <- clean_event_metadata(data.frame(event_id = "SCR"))
    time_data <- data.frame(
      site_code = c("site1"),
      subject_id = c("subj1"),
      event_id = c("SCR", "event2", "event3"),
      event_date = as.Date(c("2025-01-01", "2025-01-04", "2025-01-06")),
      edit_date_time = as.POSIXct("2024-01-01", tz = "UTC")
    ) |> 
      dplyr::mutate(day = event_date - as.Date("2020-01-01"))
    expect_output(
      output <- add_timevars_to_data(time_data, event_data),
      "Using pre-existing day column"
      )
    expect_equal(time_data, output)
  })
})

describe("add_events_to_data() works", {
  
  it("adds events to the data using declared 'event_id' as expected",{
    df <- data.frame(
      subject_id = c(rep("S1", times = 7), rep("S1", times = 7)),
      event_id = rep(c("SCR", "START", "UNV1", "V1", "UNV2", "UNV3", "V2"), times = 2),
      day = rep(c(0, 0, 2, 4, 5, 7, 8), times = 2)
    )
    events <- data.frame(
      event_id = c("SCR", "START", "V1", "V2", "FU1", "FU2", "EXIT", "UNV1", "UNV2", "UNV3"),
      is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
      is_baseline_event = c(TRUE, rep(FALSE, times = 9))
    ) |> 
      clean_event_metadata()
    output <- add_events_to_data(df, events)
    selected_output <- output[c("event_id", "event_name", "event_label")] |> 
      dplyr::arrange(event_id, event_name, event_label) |> 
      dplyr::distinct()
    expected_output <- data.frame(
      event_id   = c("SCR", "START", "UNV1", "UNV2", "UNV3", "V1", "V2"),
      event_name = c("SCR", "START", "UNV1", "UNV2", "UNV3", "V1", "V2"),
      event_label= factor(
        c("SCR", NA, NA, NA, NA, "V1", "V2"), 
        levels = c("SCR", "V1",  "V2",  "FU1", "FU2")
      )
    )
    expect_equal(selected_output, expected_output)
  })
  it("If event_id is used, it keeps correct order if events on the same day or 
      the same event on multiple days occur.",{
        df <- dplyr::tribble(
          ~subject_id, ~event_id,  ~day, 
          "S1",            "SCR",    0, 
          "S1",            "SCR",    1,
          "S1",          "START",    1, 
          "S1",           "UNV1",    2, 
          "S1",             "V1",    4, 
          "S1",             "V2",    5
        )
        events <- data.frame(
          event_id = c("SCR", "START", "V1", "V2", "FU1", "FU2", "EXIT", "UNV1"),
          is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
        ) |> 
          clean_event_metadata()
        output <- add_events_to_data(df, events)
        selected_output <- output[c("event_id", "event_name", "event_label")] |> 
          dplyr::arrange(event_id, event_name, event_label) |> 
          dplyr::distinct()
        expected_output <- dplyr::tibble(
          event_id   = c("SCR", "START", "UNV1", "V1", "V2"),
          event_name = c("SCR", "START", "UNV1", "V1", "V2"),
          event_label= factor(
            c("SCR", NA, NA, "V1", "V2"), 
            levels = c("SCR", "V1",  "V2",  "FU1", "FU2")
          )
        )
        expect_equal(selected_output, expected_output)
      })
  it("If event_id_pattern is used, the pattern will be used to match event_ids,
     and the order of events that were found in the data will be determined by 
     date of appearance", {
       df <- dplyr::tribble(
         ~subject_id, ~event_id,  ~day, 
                "S1",      "SCR",    0, 
                "S1",    "START",    0, 
                "S1",      "UNV",    2, 
                "S1",       "V1",    4, 
                "S1",      "UNV",    5, 
                "S1",      "UNV",    7, 
                "S1",       "V2",    8, 
                "S2",      "SCR",    0, 
                "S2",    "START",    0, 
                "S2",      "UNV",    4, 
                "S2",       "V1",    3, 
                "S2",      "UNV",    7, 
                "S2",       "V2",    10
       )
       events <- data.frame(
         event_id = c("SCR", "START", NA, "FU1", "FU2", "EXIT", NA),
         event_id_pattern = c(NA, NA, "^V[[:digit:]]+$", NA, NA, NA, "^UNV"),
         is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
         event_name_custom = c(NA, NA, "Visit", NA, NA, NA, "UV"),
         event_label_custom = c(NA, NA, "Vis", NA, NA, NA, NA)
       ) |> 
         clean_event_metadata()
       output <- add_events_to_data(df, events)
       selected_output <- output[c("event_id", "event_name", "event_label")] |> 
         dplyr::arrange(event_id, event_name, event_label) |> 
         dplyr::distinct()
       expected_output <- dplyr::tibble(
         event_id = c("SCR", "START", "UNV", "UNV", "UNV", "V1", "V2"),
         event_name = c("SCR", "START", "UV 1", "UV 2", "UV 3", "Visit 1", "Visit 2"),
         event_label= factor(
           c("SCR", NA, NA, NA, NA, "Vis1", "Vis2"), 
           levels = c("SCR", "Vis1",  "Vis2",  "FU1", "FU2")
         )
       )
       expect_equal(selected_output, expected_output)
  })
  
  it("handles the visit numbers correctly if one subject has the same event 
     multiple times but on different days (for example, screening spread out 
     over two days)", {
       df <- dplyr::tribble(
         ~subject_id, ~event_id,  ~day, 
         "S1",            "SCR",    0, 
         "S1",            "SCR",    1,
         "S1",          "START",    1, 
         "S1",            "UNV",    2, 
         "S1",             "V1",    4, 
         "S1",            "UNV",    5, 
         "S1",            "UNV",    7, 
         "S1",             "V2",    8, 
         "S2",            "SCR",    0, 
         "S2",          "START",    0, 
         "S2",            "UNV",    4, 
         "S2",             "V1",    3, 
         "S2",            "UNV",    7, 
         "S2",             "V2",    10
       )
       # create an example events table as created with the function get_metadata:
       events <- data.frame(
         event_id = c("SCR", "START", NA, "FU1", "FU2", "EXIT", NA),
         event_id_pattern = c(NA, NA, "^V[[:digit:]]+$", NA, NA, NA, "^UNV"),
         is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
         event_name_custom = c(NA, NA, "Visit", NA, NA, NA, "UV"),
         event_label_custom = c(NA, NA, "Vis", NA, NA, NA, NA)
       ) |> 
         clean_event_metadata()
       output <- add_events_to_data(df, events)
       selected_output <- output[c("event_id", "event_name", "event_label")] |> 
         dplyr::arrange(event_id, event_name, event_label) |> 
         dplyr::distinct()
       expected_output <- dplyr::tibble(
         event_id = c("SCR", "START", "UNV", "UNV", "UNV", "V1", "V2"),
         event_name = c("SCR", "START", "UV 1", "UV 2", "UV 3", "Visit 1", "Visit 2"),
         event_label= factor(
           c("SCR", NA, NA, NA, NA, "Vis1", "Vis2"), 
           levels = c("SCR", "Vis1",  "Vis2",  "FU1", "FU2")
         )
       )
       expect_equal(selected_output, expected_output)
     })
  it("If event_id_pattern is used and two events occur on the same day, 
      it will select the least occurring order number per event.", {
    df <- dplyr::tribble(
      ~subject_id, ~event_id,  ~day, 
      "S1",      "SCR",    0, 
      "S1",      "SCR",    1,
      "S1",    "START",    1, 
      "S1",      "UNV",    2, 
      "S1",       "V1",    4, 
      "S1",       "V2",    4, 
      "S2",      "SCR",    0, 
      "S2",      "UNV",    4, 
      "S2",       "V1",    3, 
      "S2",      "UNV",    7, 
      "S2",       "V2",    10
    )
    events <- data.frame(
      event_id = c("SCR", "START", NA, "FU1", "FU2", "EXIT", NA),
      event_id_pattern = c(NA, NA, "^V[[:digit:]]+$", NA, NA, NA, "^UNV"),
      is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
      event_name_custom = c(NA, NA, "Visit", NA, NA, NA, "UV"),
      event_label_custom = c(NA, NA, "Vis", NA, NA, NA, NA)
    ) |> 
      clean_event_metadata()
    expect_output(
      output <- add_events_to_data(df, events),
      "Event order is not unique based on event dates" 
    )
    selected_output <- output[c("event_id", "event_name", "event_label")] |> 
      dplyr::arrange(event_id, event_name, event_label) |> 
      dplyr::distinct()
    expected_output <- dplyr::tibble(
      event_id = c("SCR", "START", "UNV", "UNV", "V1", "V2"),
      event_name = c("SCR", "START", "UV 1", "UV 2", "Visit 1", "Visit 2"),
      event_label= factor(
        c("SCR", NA, NA, NA, "Vis1", "Vis2"), 
        levels = c("SCR", "Vis1",  "Vis2",  "FU1", "FU2")
      )
    )
    expect_equal(selected_output, expected_output)
  })
  it("does not error if baseline visit is missing", {
    df <- dplyr::tribble(
      ~subject_id, ~event_id,  ~day, 
      "S1",    "START",    1, 
      "S1",      "UNV",    2, 
      "S1",       "V1",    4, 
      "S1",       "V2",    5
    )
    events <- data.frame(
      event_id = c("SCR", "START", NA, "FU1", "FU2", "EXIT", NA),
      event_id_pattern = c(NA, NA, "^V[[:digit:]]+$", NA, NA, NA, "^UNV"),
      is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
      event_name_custom = c(NA, NA, "Visit", NA, NA, NA, "UV"),
      event_label_custom = c(NA, NA, "Vis", NA, NA, NA, NA)
    ) |> 
      clean_event_metadata()
    output <- add_events_to_data(df, events)
    selected_output <- output[c("event_id", "event_name", "event_label")] |> 
      dplyr::arrange(event_id, event_name, event_label) |> 
      dplyr::distinct()
    expected_output <- dplyr::tibble(
      event_id = c("START", "UNV", "V1", "V2"),
      event_name = c("START", "UV 1", "Visit 1", "Visit 2"),
      event_label= factor(
        c(NA, NA, "Vis1", "Vis2"), 
        levels = c("SCR", "Vis1",  "Vis2",  "FU1", "FU2")
      )
    )
    expect_equal(selected_output, expected_output)
  })
  it("If using event_id_pattern, adds correct visit number if baseline 
     visit is not the first visit", {
    df <- dplyr::tribble(
      ~subject_id, ~event_id,  ~day, 
      "S1",          "SCR",    -2, 
      "S1",        "VisBase",     4, 
      "S1",    "Visregular1",     5
    )
    events <- data.frame(
      event_id           = c("SCR", "VisBase", NA, "EXIT"),
      event_id_pattern   = c(NA, NA, "^Visregular", NA),
      is_baseline_event  = c(FALSE, TRUE, FALSE, FALSE),
      event_name_custom  = c(NA, "Baseline", "Visit", NA),
      event_label_custom = c(NA, NA, "Vis", NA)
    ) |> 
      clean_event_metadata()
    output <- add_events_to_data(df, events)
    selected_output <- output[c("event_id", "event_name", "event_label")] |> 
      dplyr::arrange(event_id, event_name, event_label) |> 
      dplyr::distinct()
    expected_output <- dplyr::tibble(
      event_id = c("SCR", "VisBase", "Visregular1"),
      event_name = c("SCR", "Baseline", "Visit 1"),
      event_label= factor(
        c("SCR", "VisBase", "Vis1"), 
        levels = c("SCR", "VisBase", "Vis1", "EXIT")
      )
    )
    expect_equal(selected_output, expected_output)
  })
  it("errors if required columns are missing", {
    expect_error(
      add_events_to_data(data.frame(), data.frame()),
      "The following columns are required but are missing"
    )
  })
  it("returns early if event_name and event_label already exist in the data, 
     irrespective of the events dataframe", {
    df <- data.frame(event_name = "Screening", event_label = "SCR")
    add_events_to_data(df, data.frame())
  })
  it("adds event_name_edc to the name in brackets if it exists and differs from the cusomized name", {
    df <- dplyr::tribble(
      ~subject_id, ~event_id,  ~day, ~event_name_edc,
      "S1",            "SCR",    0,  "SCR",
      "S1",            "SCR",    1,  "SCR",
      "S1",          "START",    1,  "Study start",
      "S1",           "UNV1",    2,  "Unscheduled visit",
      "S1",             "V1",    4,  "C1D1",
      "S1",             "V2",    5,  "C1D2"
    )
    events <- data.frame(
      event_id = c("SCR", "START", "V1", "V2", "FU1", "FU2", "EXIT", "UNV1"),
      is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
      event_name_custom = c(rep(NA, times = 7), "Unscheduled visit")
    ) |> 
      clean_event_metadata()
    output <- add_events_to_data(df, events)
    expect_equal(
      unique(output$event_name),
      c("SCR", "START (Study start)", "Unscheduled visit", "V1 (C1D1)", "V2 (C1D2)")
    )
  })
})

