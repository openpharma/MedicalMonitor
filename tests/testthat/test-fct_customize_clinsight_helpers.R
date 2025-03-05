describe(
  "create_clinsight_metadata() creates a metadata file equal to the metadata template.", 
  {
    it("Creates the correct metadata file.", {
      local_mocked_bindings(file.show = \(...){""})
      temp_path  <- withr::local_tempdir()
      expect_message(
        create_clinsight_metadata(path = temp_path),
        "Creating a customizable ClinSight metadata file in the following location"
      )
      expect_equal(
        get_metadata(file.path(temp_path, "clinsight_metadata.xlsx")), 
        metadata
      )
    })
    it("Errors if the file already exists.", {
      local_mocked_bindings(file.show = \(...){})
      temp_path  <- withr::local_tempdir()
      temp_file_path <- file.path(temp_path, "clinsight_metadata.xlsx")
      file.create(temp_file_path)
      expect_error(
        create_clinsight_metadata(path = temp_path),
        "already exists. Delete or rename this file and try again"
      )
    })
  }
)

describe(
  "create_clinsight_config() creates a config file equal to the clinsight config template.", 
  {
    it("Creates the correct config file.", {
      local_mocked_bindings(file.show = \(...){""})
      temp_path  <- withr::local_tempdir()
      expect_message(
        create_clinsight_config(path = temp_path),
        "Creating a customizable ClinSight config file in the following location"
      )
      custom_config <- config::get(
        config = "test", 
        file = file.path(temp_path, "clinsight_config.yml")
      )
      attr(custom_config, "file") <- ""
      
      template_config <- config::get(
        config = "test", 
        file = app_sys("golem-config.yml")
        )
      attr(template_config, "file") <- ""
      expect_equal(custom_config, template_config)
    })
    it("Errors if the file already exists.", {
      local_mocked_bindings(file.show = \(...){})
      temp_path  <- withr::local_tempdir()
      temp_file_path <- file.path(temp_path, "clinsight_config.yml")
      file.create(temp_file_path)
      expect_error(
        create_clinsight_config(path = temp_path),
        "already exists. Delete or rename this file and try again"
      )
    })
  }
)