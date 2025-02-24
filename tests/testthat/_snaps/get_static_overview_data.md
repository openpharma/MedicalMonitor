# get_static_overview_data() works. : creates the expected output

    Code
      get_static_overview_data(data = appdata, expected_general_columns = metadata$
        general$item_name)
    Output
      # A tibble: 25 x 6
         subject_id status                WHO.classification Age   Sex    event_name  
         <chr>      <chr>                 <chr>              <chr> <chr>  <chr>       
       1 BEL_04_133 Enrolled              Syndrome K         88    Male   Screening, ~
       2 BEL_04_772 Enrolled              Syndrome O         78    Male   Screening, ~
       3 BEL_07_193 Enrolled              Syndrome D         26    Female Screening   
       4 BEL_07_431 Unknown               <NA>               42    Male   Screening   
       5 BEL_07_497 Withdrawal by subject <NA>               50    Female Screening   
       6 BEL_07_645 Enrolled              Syndrome J         46    Male   Screening, ~
       7 BEL_08_45  Death                 Syndrome V         64    Male   Screening   
       8 BEL_08_736 Enrolled              Syndrome A         45    Female Screening, ~
       9 BEL_08_885 Enrolled              Syndrome S         82    Male   Screening, ~
      10 BEL_09_361 Enrolled              Syndrome G         38    Male   Screening, ~
      # i 15 more rows

