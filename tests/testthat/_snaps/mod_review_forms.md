# mod_review_forms. Feature 2 | Save review of a form. As a user, I want to be able to save a review of a form in the database. After saving the review, all items of that form that are not yet reviewed should get a tag that the value was reviewed.: Scenario 1 - Save a review. Given test review data with at least an 'Adverse event' form with patient '885',and [user_name] set to 'test_name' and [user_role] to 'Medical Monitor'and [active_patient] set to '885', and [active_form] set to 'Adverse events', and [active_tab] set to 'Common forms', and [form_reviewed] set to FALSE, I expect that I can save a new review properly, with the result saved in the application being the same as the one saved in the database, and no review error occurring

    Code
      dplyr::select(do.call(rbind, r$review_data), -timestamp)
    Output
                       id subject_id event_name     item_group form_repeat
      Adverse events.1  1        885  Any visit Adverse events           1
      Adverse events.2  2        885  Any visit Adverse events           2
      Vital signs.3     3        361    Visit 5    Vital signs           4
      Vital signs.4     4        361    Visit 6    Vital signs           5
                                     item_name event_date      edit_date_time
      Adverse events.1     Atrial Fibrillation 2023-08-15 2023-09-30 01:01:01
      Adverse events.2                Cystitis 2023-09-01 2023-10-01 01:01:01
      Vital signs.3    Systolic blood pressure 2023-07-01 2023-08-30 01:01:01
      Vital signs.4    Systolic blood pressure 2023-08-01 2023-08-02 01:01:01
                       reviewed         comment                    reviewer status
      Adverse events.1      Yes    test comment                  Reviewer 1    old
      Adverse events.2      Yes                 test_name (Medical Monitor)    old
      Vital signs.3         Yes another comment                  Reviewer 2    old
      Vital signs.4          No                                                new

---

    Code
      dplyr::select(do.call(rbind, r$review_data), -timestamp)
    Output
                       id subject_id event_name     item_group form_repeat
      Adverse events.1  1        885  Any visit Adverse events           1
      Adverse events.2  2        885  Any visit Adverse events           2
      Vital signs.3     3        361    Visit 5    Vital signs           4
      Vital signs.4     4        361    Visit 6    Vital signs           5
                                     item_name event_date      edit_date_time
      Adverse events.1     Atrial Fibrillation 2023-08-15 2023-09-30 01:01:01
      Adverse events.2                Cystitis 2023-09-01 2023-10-01 01:01:01
      Vital signs.3    Systolic blood pressure 2023-07-01 2023-08-30 01:01:01
      Vital signs.4    Systolic blood pressure 2023-08-01 2023-08-02 01:01:01
                       reviewed         comment                    reviewer status
      Adverse events.1       No     test review test_name (Medical Monitor)    new
      Adverse events.2       No     test review test_name (Medical Monitor)    new
      Vital signs.3         Yes another comment                  Reviewer 2    old
      Vital signs.4          No                                                new

