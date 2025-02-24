describe(
  "Create_table.default() works and creates a table in wide format.", 
  {
    set.seed(2023)
    df <- data.frame(
      vals = sample(1:50, 10),
      var  = sample(c("item1", "item2", "item3"), size = 10, replace = TRUE),
      ID   = sample(c("ID1", "ID2"), size = 10, replace = TRUE),
      Site  = sample(c("Site1", "Site2"), size = 10, replace = TRUE)
    )
    it("Creates expected table output", {
      outcome <- create_table.default(
        df, 
        name_column = "var", 
        value_column = "vals",
        keep_vars = c("ID", "Site")
      )
      expect_true(is.data.frame(outcome))
      expect_equal(names(outcome), c("ID", "Site", "item1", "item3", "item2"))
      expect_snapshot(outcome)
    })
    it("produces the same output using the S3 method", {
      expect_equal(
        create_table(df, 
          name_column = "var", 
          value_column = "vals",
          keep_vars = c("ID", "Site")
        ),
        create_table.default(
          df, 
          name_column = "var", 
          value_column = "vals",
          keep_vars = c("ID", "Site")
        )
      )
    })
    it("Outputs an empty table if an empty table is provided", {
      outcome <- create_table.default(
        df[0, ], 
        name_column = "var", 
        value_column = "vals",
        keep_vars = c("ID", "Site")
      )
      expect_true(is.data.frame(outcome))
      expect_true(nrow(outcome) == 0)
    })
  }
)

describe("create_table.continuous() works.", {
  set.seed(2024)
  df <- data.frame(
    item_value = sample(c(1, 3, NA_integer_), size = 10, replace = TRUE),
    var  = sample(c("item1", "item2", "item3"), size = 10, replace = TRUE),
    ID   = sample(c("ID1", "ID2", "ID3"), size = 10, replace = TRUE),
    Site  = sample(c("Site1", "Site2"), size = 10, replace = TRUE),
    item_unit = sample(c("mL", NA_character_, "Hubble-barn", "Donkey power"), size = 10, replace = TRUE),
    reason_notdone = sample(c(NA_character_, "my fish was unwell", "zombie attack"), size = 10, replace = TRUE)
  )
  it("Produces the expected output", {
    output <- create_table.continuous(
      df, name_column = "var", value_column = "item_value", 
      unit_column = "item_unit", explanation_column = "reason_notdone", 
      keep_vars = c("ID", "Site")
      )
    expect_true(is.data.frame(output))
    expect_equal(names(output), c("ID", "Site", "item1", "item3", "item2"))
    expect_snapshot(output)
  })
  it("Produces the same output with the S3 method", {
    class(df) <- c("continuous", class(df))
    expect_equal(
      create_table.continuous(
        df, name_column = "var", value_column = "item_value", 
        unit_column = "item_unit", explanation_column = "reason_notdone", 
        keep_vars = c("ID", "Site")
      ),
      create_table(
        df, name_column = "var", value_column = "item_value", 
        unit_column = "item_unit", explanation_column = "reason_notdone", 
        keep_vars = c("ID", "Site")
      )
    )
  })
  it("Ignores explanation_column and unit_column if they are NULL", {
    output <- create_table.continuous(
      df, name_column = "var", value_column = "item_value", 
      unit_column = NULL, explanation_column = NULL, 
      keep_vars = c("ID", "Site")
    )
    expect_true(is.data.frame(output))
    expect_snapshot(output)
    # TODO: it still shows NA if an items is completely missing from a patient. 
    # Better would be to expand the column with all possible combinations available 
    # in the data. Not yet implemented due to time restrictions.
  })
  it("does not error with a zero-row data frame input", {
    expect_no_error(create_table(
      df[0,], name_column = "var", value_column = "item_value", 
      unit_column = "item_unit", explanation_column = "reason_notdone", 
      keep_vars = c("ID", "Site")
      ))
    output <- create_table(
      df[0,], name_column = "var", value_column = "item_value", 
      unit_column = "item_unit", explanation_column = "reason_notdone", 
      keep_vars = c("ID", "Site")
    )
    expect_equal(nrow(output), 0)
  })
})

describe(
  "create_table.general() works", 
  {
    appdata <- get_appdata(clinsightful_data)
    vars <- get_meta_vars(appdata, metadata)
    expected_cols <- names(vars$items$General)
    df <- appdata$General
    it("creates a table with S3 method", {
      expect_true(is.data.frame(create_table(df)))
      expect_equal(create_table(df), create_table.general(df))
    })
    
    it("creates expected table", {
      expect_snapshot(print(create_table(df, expected_columns = expected_cols), 
                            n = 25))
    })
    it("does not error with a zero-row data frame input", {
      expect_no_error(create_table(df[0,], expected_columns = expected_cols))
      output <- create_table(df[0,], expected_columns = expected_cols)
      expect_equal(nrow(output), 0)
    })
  }
)


describe(
  "create_table.adverse_events() works", 
  {
    appdata <- get_appdata(clinsightful_data)
    vars <- get_meta_vars(appdata, metadata)
    expected_cols <- names(vars$items$`Adverse events`)
    df <- appdata$`Adverse events`
    it("creates a table with S3 method for adverse events", {
      expect_true(is.data.frame(create_table(df)))
      expect_equal(create_table(df), create_table.adverse_events(df))
    })
    
    it("creates expected AE table", {
      expect_snapshot(print(create_table(df, expected_columns = expected_cols), n = 25))
    })
    
    it("does not error with a zero-row data frame input", {
      expect_no_error(create_table(df[0,], expected_columns = expected_cols))
      output <- create_table(df[0,], expected_columns = expected_cols)
      expect_equal(nrow(output), 0)
    })
  }
)

describe(
  "create_table.medication() works", 
  {
    appdata <- get_appdata(clinsightful_data)
    vars <- get_meta_vars(appdata, metadata)
    expected_cols <- names(vars$items$`Medication`)
    df <- appdata$Medication
    it("creates a table with S3 method for medication", {
      expect_true(is.data.frame(create_table(df)))
      expect_equal(create_table(df), create_table.medication(df))
    })
    
    it("creates expected medication table", {
      expect_snapshot(print(create_table(df, expected_columns = expected_cols), 
                            n = 25))
    })
    it("does not error with a zero-row data frame input", {
      expect_no_error(create_table(df[0,], expected_columns = expected_cols))
      output <- create_table(df[0,], expected_columns = expected_cols)
      expect_equal(nrow(output), 0)
    })
  }
)

describe(
  "create_table.medical_history() works", 
  {
    appdata <- get_appdata(clinsightful_data)
    vars <- get_meta_vars(appdata, metadata)
    expected_cols <- names(vars$items$`Medical History`)
    df <- appdata$`Medical History`
    it("creates a table with S3 method for medical history", {
      expect_true(is.data.frame(create_table(df)))
      expect_equal(create_table(df), create_table.medical_history(df))
    })
    
    it("creates expected medical history table", {
      expect_snapshot(print(create_table(df, expected_columns = expected_cols), 
                            n = 25))
    })
    it("does not error with a zero-row data frame input", {
      expect_no_error(create_table(df[0,], expected_columns = expected_cols))
      output <- create_table(df[0,], expected_columns = expected_cols)
      expect_equal(nrow(output), 0)
    })
  }
)

describe(
  "create_table.conc_procedures() works", 
  {
    appdata <- get_appdata(clinsightful_data)
    vars <- get_meta_vars(appdata, metadata)
    expected_cols <- names(vars$items$`Conc. Procedures`)
    df <- appdata$`Conc. Procedures`
    it("creates a table with S3 method", {
      expect_true(is.data.frame(create_table(df)))
      expect_equal(create_table(df), create_table.conc_procedures(df))
    })
    
    it("creates expected table", {
      expect_snapshot(print(create_table(df, expected_columns = expected_cols), 
                            n = 25))
    })
    it("does not error with a zero-row data frame input", {
      expect_no_error(create_table(df[0,], expected_columns = expected_cols))
      output <- create_table(df[0,], expected_columns = expected_cols)
      expect_equal(nrow(output), 0)
    })
  }
)

describe(
  "create_table.bm_cytology() works", 
  {
    # create data since BM cytology data is not in internal package data:
    df <- data.frame(
      site_code = "site1",
      subject_id = "885",
      event_repeat = 1,
      event_name = "Screening",
      event_date = "2023-08-07",
      item_group = "BM cytology",
      edit_date_time = "2023-08-24 00:10:00",
      item_name = c(
        "Bone marrow blasts",
        "BM smear assessment",
        "Auer Rods detectable?",
        "Ringed Sideroblasts detectable?"
      ),
      item_type = c(
        "other", 
        "categorical", 
        "dichotomous", 
        "dichotomous"
      ),
      item_value = c(
        "91", 
        "hypercellular", 
        "No", 
        "No"
      )
    )
    class(df) <- c("bm_cytology", class(df))
    expected_columns <- c(
      "Bone marrow blasts",
      "BM smear assessment",
      "Auer Rods detectable?",
      "Ringed Sideroblasts detectable?"
    )
    it("creates a table with S3 method", {
      expect_true(is.data.frame(create_table(df)))
      expect_equal(create_table(df), create_table.bm_cytology(df))
    })
    
    it("creates expected table", {
      expect_snapshot(print(create_table(df), 
                            n = 25))
    })
    it("does not error with a zero-row data frame input", {
      expect_no_error(create_table(df[0,], expected_columns = expected_columns))
      output <- create_table(df[0,], expected_columns = expected_columns)
      expect_equal(nrow(output), 0)
    })
  }
)

describe(
  "create_table.common_forms",
    {
      appdata <- get_appdata(clinsightful_data)
      vars <- get_meta_vars(appdata, metadata)
      expected_cols <- names(vars$items$`Medical History`)
      df <- appdata$`Medical History`
      # Remove medical_history class
      class(df) <- class(df)[-1]
      expect_equal(
        class(df),
        c("common_forms", "tbl_df", "tbl", "data.frame")
      )
      it("creates a table with S3 method for common forms", {
        expect_true(is.data.frame(create_table(df)))
        expect_equal(create_table(df), create_table.common_forms(df))
      })
      
      it("creates expected medical history table", {
        expect_snapshot(print(create_table(df, expected_columns = expected_cols), 
                              n = 25))
      })
      it("does not error with a zero-row data frame input", {
        expect_no_error(create_table(df[0,], expected_columns = expected_cols))
        output <- create_table(df[0,], expected_columns = expected_cols)
        expect_equal(nrow(output), 0)
      })
    }
)
