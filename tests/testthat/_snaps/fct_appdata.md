# get_raw_csv_data works: Produces the expected output.

    Code
      df
    Output
      # A tibble: 3,267 x 26
         SiteSeq SiteCode SubjectSeq SubjectId EventSeq EventId   EventName  EventDate
         <chr>   <chr>    <chr>      <chr>     <chr>    <chr>     <chr>      <chr>    
       1 2       9700     1          9700-001  1        COMMON_AE Adverse E~ 2022-11-~
       2 2       9700     1          9700-001  1        COMMON_AE Adverse E~ 2022-11-~
       3 2       9700     1          9700-001  1        COMMON_AE Adverse E~ 2022-11-~
       4 2       9700     1          9700-001  1        COMMON_AE Adverse E~ 2022-11-~
       5 2       9700     1          9700-001  1        COMMON_AE Adverse E~ 2022-11-~
       6 2       9700     1          9700-001  1        COMMON_AE Adverse E~ 2022-11-~
       7 2       9700     1          9700-001  1        COMMON_AE Adverse E~ 2022-11-~
       8 2       9700     1          9700-001  1        COMMON_AE Adverse E~ 2022-11-~
       9 2       9700     1          9700-001  1        COMMON_AE Adverse E~ 2022-11-~
      10 2       9700     1          9700-001  1        COMMON_AE Adverse E~ 2022-11-~
      # i 3,257 more rows
      # i 18 more variables: ActivityId <chr>, ActivityName <chr>, FormId <chr>,
      #   FormName <chr>, FormSeq <chr>, SubjectFormSeq <chr>,
      #   OriginSubjectFormSeq <chr>, SourceSubjectFormSeq <chr>, ItemGroupId <chr>,
      #   ItemGroupSeq <chr>, ItemId <chr>, ItemExportLabel <chr>,
      #   DesignVersion <chr>, ItemValue <chr>, ItemCode <chr>, EditDateTime <chr>,
      #   EditBy <chr>, EditReason <chr>

# merge_meta_with_data. Feature 1 | Merge raw data with metadata. As a user, I want to be able to merge raw data with metadata. Furthermore, I want to be able to fix suffixes and rename the limits and significance values to the standard names used in the app.: Scenario 1 - Given a data frame with raw data,I expect that the output will be the same as recorded in a snapshot.

    Code
      df[c(1, 1000, 2000, 3000, 4000, 5000), ]
    Output
      # A tibble: 6 x 22
        site_code subject_id event_id event_date event_repeat form_id form_repeat
        <chr>     <chr>      <chr>    <date>            <int> <chr>         <int>
      1 9600      9600-001   SCR      2022-11-09            1 DM                1
      2 <NA>      <NA>       <NA>     NA                   NA <NA>             NA
      3 <NA>      <NA>       <NA>     NA                   NA <NA>             NA
      4 <NA>      <NA>       <NA>     NA                   NA <NA>             NA
      5 <NA>      <NA>       <NA>     NA                   NA <NA>             NA
      6 <NA>      <NA>       <NA>     NA                   NA <NA>             NA
      # i 15 more variables: edit_date_time <dttm>, day <drtn>, event_name <chr>,
      #   event_label <fct>, form_type <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_unit <chr>, lower_lim <dbl>, upper_lim <dbl>,
      #   item_value <chr>, significance <chr>, reason_notdone <chr>, region <chr>

---

    Code
      df
    Output
      # A tibble: 543 x 22
         site_code subject_id event_id event_date event_repeat form_id form_repeat
         <chr>     <chr>      <chr>    <date>            <int> <chr>         <int>
       1 9600      9600-001   SCR      2022-11-09            1 DM                1
       2 9600      9600-001   SCR      2022-11-09            1 DM                1
       3 9600      9600-002   SCR      2022-01-01            1 DM                1
       4 9600      9600-002   SCR      2022-01-01            1 DM                1
       5 9600      9600-002   SCR      2022-01-01            1 DM                1
       6 9600      9600-002   SCR      2022-01-01            1 STE               1
       7 9600      9600-002   SCR      2022-01-01            1 STE               1
       8 9600      9600-002   SCR      2022-01-01            1 VS                1
       9 9600      9600-002   SCR      2022-01-01            1 VS                1
      10 9600      9600-002   SCR      2022-01-01            1 VS                1
      # i 533 more rows
      # i 15 more variables: edit_date_time <dttm>, day <drtn>, event_name <chr>,
      #   event_label <fct>, form_type <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_unit <chr>, lower_lim <dbl>, upper_lim <dbl>,
      #   item_value <chr>, significance <chr>, reason_notdone <chr>, region <chr>

# get_appdata works: produces the expected output

    Code
      get_appdata(clinsightful_data, metadata)
    Output
      $`Adverse events`
      # A tibble: 799 x 22
         site_code subject_id event_id  event_date event_repeat form_id form_repeat
         <chr>     <chr>      <chr>     <date>            <int> <chr>         <int>
       1 BEL04     BEL_04_772 COMMON_AE 2023-08-31            1 AE                1
       2 BEL04     BEL_04_772 COMMON_AE 2023-08-31            1 AE                1
       3 BEL04     BEL_04_772 COMMON_AE 2023-08-31            1 AE                1
       4 BEL04     BEL_04_772 COMMON_AE 2023-08-31            1 AE                1
       5 BEL04     BEL_04_772 COMMON_AE 2023-08-31            1 AE                1
       6 BEL04     BEL_04_772 COMMON_AE 2023-08-31            1 AE                1
       7 BEL04     BEL_04_772 COMMON_AE 2023-08-31            1 AE                1
       8 BEL04     BEL_04_772 COMMON_AE 2023-08-31            1 AE                1
       9 BEL04     BEL_04_772 COMMON_AE 2023-08-31            2 AE                2
      10 BEL04     BEL_04_772 COMMON_AE 2023-08-31            2 AE                2
      # i 789 more rows
      # i 15 more variables: edit_date_time <dttm>, day <drtn>, event_name <chr>,
      #   event_label <fct>, form_type <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_unit <chr>, lower_lim <dbl>, upper_lim <dbl>,
      #   item_value <chr>, significance <chr>, reason_notdone <chr>, region <chr>
      
      $`CBC regular`
      # A tibble: 381 x 25
         site_code subject_id event_id event_date event_repeat form_id form_repeat
         <chr>     <chr>      <chr>    <date>            <int> <chr>         <int>
       1 BEL04     BEL_04_133 SCR      2023-06-05            1 LBHM              1
       2 BEL04     BEL_04_133 SCR      2023-06-05            1 LBHM              1
       3 BEL04     BEL_04_133 SCR      2023-06-05            1 LBHM              1
       4 BEL04     BEL_04_133 SCR      2023-06-05            1 LBHM              1
       5 BEL04     BEL_04_133 SCR      2023-06-05            1 LBHM              1
       6 BEL04     BEL_04_133 SCR      2023-06-05            1 LBHM              1
       7 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBHM              2
       8 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBHM              2
       9 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBHM              2
      10 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBHM              2
      # i 371 more rows
      # i 18 more variables: edit_date_time <dttm>, day <drtn>, event_name <chr>,
      #   event_label <fct>, form_type <chr>, item_name <fct>, item_type <chr>,
      #   item_group <chr>, item_unit <chr>, lower_lim <dbl>, upper_lim <dbl>,
      #   item_value <dbl>, significance <fct>, reason_notdone <chr>, region <chr>,
      #   value_scaled <dbl>, out_of_lim <fct>, text_label <chr>
      
      $`Conc. Procedures`
      # A tibble: 28 x 22
         site_code subject_id event_id  event_date event_repeat form_id form_repeat
         <chr>     <chr>      <chr>     <date>            <int> <chr>         <int>
       1 BEL07     BEL_07_645 COMMON_PR 2023-09-14            1 CP                1
       2 BEL07     BEL_07_645 COMMON_PR 2023-09-14            1 CP                1
       3 BEL07     BEL_07_645 COMMON_PR 2023-09-14            1 CP                1
       4 BEL07     BEL_07_645 COMMON_PR 2023-09-14            1 CP                1
       5 BEL07     BEL_07_645 COMMON_PR 2023-09-14            1 CP                1
       6 BEL07     BEL_07_645 COMMON_PR 2023-09-14            1 CP                1
       7 BEL07     BEL_07_645 COMMON_PR 2023-09-14            2 CP                2
       8 BEL07     BEL_07_645 COMMON_PR 2023-09-14            2 CP                2
       9 BEL07     BEL_07_645 COMMON_PR 2023-09-14            2 CP                2
      10 BEL07     BEL_07_645 COMMON_PR 2023-09-14            2 CP                2
      # i 18 more rows
      # i 15 more variables: edit_date_time <dttm>, day <drtn>, event_name <chr>,
      #   event_label <fct>, form_type <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_unit <chr>, lower_lim <dbl>, upper_lim <dbl>,
      #   item_value <chr>, significance <chr>, reason_notdone <chr>, region <chr>
      
      $Electrolytes
      # A tibble: 275 x 25
         site_code subject_id event_id event_date event_repeat form_id form_repeat
         <chr>     <chr>      <chr>    <date>            <int> <chr>         <int>
       1 BEL04     BEL_04_133 SCR      2023-06-05            1 LBSER             1
       2 BEL04     BEL_04_133 SCR      2023-06-05            1 LBSER             1
       3 BEL04     BEL_04_133 SCR      2023-06-05            1 LBSER             1
       4 BEL04     BEL_04_133 SCR      2023-06-05            1 LBSER             1
       5 BEL04     BEL_04_133 SCR      2023-06-05            1 LBSER             1
       6 BEL04     BEL_04_133 SCR      2023-06-05            1 LBSER             1
       7 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBSER             2
       8 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBSER             2
       9 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBSER             2
      10 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBSER             2
      # i 265 more rows
      # i 18 more variables: edit_date_time <dttm>, day <drtn>, event_name <chr>,
      #   event_label <fct>, form_type <chr>, item_name <fct>, item_type <chr>,
      #   item_group <chr>, item_unit <chr>, lower_lim <dbl>, upper_lim <dbl>,
      #   item_value <dbl>, significance <fct>, reason_notdone <chr>, region <chr>,
      #   value_scaled <dbl>, out_of_lim <fct>, text_label <chr>
      
      $General
      # A tibble: 261 x 22
         site_code subject_id event_id event_date event_repeat form_id form_repeat
         <chr>     <chr>      <chr>    <date>            <int> <chr>         <int>
       1 BEL04     BEL_04_133 SCR      2023-06-05            1 DM                1
       2 BEL04     BEL_04_133 SCR      2023-06-05            1 DM                1
       3 BEL04     BEL_04_133 SCR      2023-06-05            1 DM                1
       4 BEL04     BEL_04_133 SCR      2023-06-05            1 DM                1
       5 BEL04     BEL_04_133 SCR      2023-06-05            1 DM                1
       6 BEL04     BEL_04_133 SCR      2023-06-05            1 ECOG              1
       7 BEL04     BEL_04_133 SCR      2023-06-05            1 STE               1
       8 BEL04     BEL_04_133 SCR      2023-06-05            1 STE               1
       9 BEL04     BEL_04_133 SCR      2023-06-05            1 WHO               1
      10 BEL04     BEL_04_133 VIS1     2023-07-05            1 EX                1
      # i 251 more rows
      # i 15 more variables: edit_date_time <dttm>, day <drtn>, event_name <chr>,
      #   event_label <fct>, form_type <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_unit <chr>, lower_lim <dbl>, upper_lim <dbl>,
      #   item_value <chr>, significance <chr>, reason_notdone <chr>, region <chr>
      
      $`Liver function`
      # A tibble: 180 x 25
         site_code subject_id event_id event_date event_repeat form_id form_repeat
         <chr>     <chr>      <chr>    <date>            <int> <chr>         <int>
       1 BEL04     BEL_04_133 SCR      2023-06-05            1 LBSER             1
       2 BEL04     BEL_04_133 SCR      2023-06-05            1 LBSER             1
       3 BEL04     BEL_04_133 SCR      2023-06-05            1 LBSER             1
       4 BEL04     BEL_04_133 SCR      2023-06-05            1 LBSER             1
       5 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBSER             2
       6 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBSER             2
       7 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBSER             2
       8 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBSER             2
       9 BEL04     BEL_04_133 VIS3     2023-08-02            3 LBSER             3
      10 BEL04     BEL_04_133 VIS3     2023-08-02            3 LBSER             3
      # i 170 more rows
      # i 18 more variables: edit_date_time <dttm>, day <drtn>, event_name <chr>,
      #   event_label <fct>, form_type <chr>, item_name <fct>, item_type <chr>,
      #   item_group <chr>, item_unit <chr>, lower_lim <dbl>, upper_lim <dbl>,
      #   item_value <dbl>, significance <fct>, reason_notdone <chr>, region <chr>,
      #   value_scaled <dbl>, out_of_lim <fct>, text_label <chr>
      
      $`Medical History`
      # A tibble: 757 x 22
         site_code subject_id event_id  event_date event_repeat form_id form_repeat
         <chr>     <chr>      <chr>     <date>            <int> <chr>         <int>
       1 BEL04     BEL_04_133 COMMON_MH 2023-06-23            1 MH                1
       2 BEL04     BEL_04_133 COMMON_MH 2023-06-23            1 MH                1
       3 BEL04     BEL_04_133 COMMON_MH 2023-06-23            1 MH                1
       4 BEL04     BEL_04_133 COMMON_MH 2023-06-23            1 MH                1
       5 BEL04     BEL_04_772 COMMON_MH 2023-07-11            1 MH                1
       6 BEL04     BEL_04_772 COMMON_MH 2023-07-11            1 MH                1
       7 BEL04     BEL_04_772 COMMON_MH 2023-07-11            1 MH                1
       8 BEL04     BEL_04_772 COMMON_MH 2023-07-11            1 MH                1
       9 BEL04     BEL_04_772 COMMON_MH 2023-07-11            2 MH                2
      10 BEL04     BEL_04_772 COMMON_MH 2023-07-11            2 MH                2
      # i 747 more rows
      # i 15 more variables: edit_date_time <dttm>, day <drtn>, event_name <chr>,
      #   event_label <fct>, form_type <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_unit <chr>, lower_lim <dbl>, upper_lim <dbl>,
      #   item_value <chr>, significance <chr>, reason_notdone <chr>, region <chr>
      
      $Medication
      # A tibble: 3,183 x 22
         site_code subject_id event_id  event_date event_repeat form_id form_repeat
         <chr>     <chr>      <chr>     <date>            <int> <chr>         <int>
       1 BEL04     BEL_04_133 COMMON_CM 2023-06-23            1 CM                1
       2 BEL04     BEL_04_133 COMMON_CM 2023-06-23            1 CM                1
       3 BEL04     BEL_04_133 COMMON_CM 2023-06-23            1 CM                1
       4 BEL04     BEL_04_133 COMMON_CM 2023-06-23            1 CM                1
       5 BEL04     BEL_04_133 COMMON_CM 2023-06-23            1 CM                1
       6 BEL04     BEL_04_133 COMMON_CM 2023-06-23            1 CM                1
       7 BEL04     BEL_04_772 COMMON_CM 2023-07-11            1 CM                1
       8 BEL04     BEL_04_772 COMMON_CM 2023-07-11            1 CM                1
       9 BEL04     BEL_04_772 COMMON_CM 2023-07-11            1 CM                1
      10 BEL04     BEL_04_772 COMMON_CM 2023-07-11            1 CM                1
      # i 3,173 more rows
      # i 15 more variables: edit_date_time <dttm>, day <drtn>, event_name <chr>,
      #   event_label <fct>, form_type <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_unit <chr>, lower_lim <dbl>, upper_lim <dbl>,
      #   item_value <chr>, significance <chr>, reason_notdone <chr>, region <chr>
      
      $`Renal function`
      # A tibble: 135 x 25
         site_code subject_id event_id event_date event_repeat form_id form_repeat
         <chr>     <chr>      <chr>    <date>            <int> <chr>         <int>
       1 BEL04     BEL_04_133 SCR      2023-06-05            1 LBSER             1
       2 BEL04     BEL_04_133 SCR      2023-06-05            1 LBSER             1
       3 BEL04     BEL_04_133 SCR      2023-06-05            1 LBSER             1
       4 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBSER             2
       5 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBSER             2
       6 BEL04     BEL_04_133 VIS1     2023-07-05            1 LBSER             2
       7 BEL04     BEL_04_133 VIS3     2023-08-02            3 LBSER             3
       8 BEL04     BEL_04_133 VIS3     2023-08-02            3 LBSER             3
       9 BEL04     BEL_04_133 VIS3     2023-08-02            3 LBSER             3
      10 BEL04     BEL_04_133 VIS5     2023-08-30            5 LBSER             4
      # i 125 more rows
      # i 18 more variables: edit_date_time <dttm>, day <drtn>, event_name <chr>,
      #   event_label <fct>, form_type <chr>, item_name <fct>, item_type <chr>,
      #   item_group <chr>, item_unit <chr>, lower_lim <dbl>, upper_lim <dbl>,
      #   item_value <dbl>, significance <fct>, reason_notdone <chr>, region <chr>,
      #   value_scaled <dbl>, out_of_lim <fct>, text_label <chr>
      
      $Response
      # A tibble: 6 x 22
        site_code subject_id event_id event_date event_repeat form_id form_repeat
        <chr>     <chr>      <chr>    <date>            <int> <chr>         <int>
      1 BEL04     BEL_04_133 VIS5     2023-08-30            5 RS                1
      2 BEL04     BEL_04_133 VIS5     2023-08-30            5 RS                1
      3 BEL04     BEL_04_772 VIS5     2023-08-30            5 RS                1
      4 BEL04     BEL_04_772 VIS5     2023-08-30            5 RS                1
      5 NLD06     NLD_06_72  VIS5     2023-09-06            5 RS                1
      6 NLD06     NLD_06_72  VIS5     2023-09-06            5 RS                1
      # i 15 more variables: edit_date_time <dttm>, day <drtn>, event_name <chr>,
      #   event_label <fct>, form_type <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_unit <chr>, lower_lim <dbl>, upper_lim <dbl>,
      #   item_value <chr>, significance <chr>, reason_notdone <chr>, region <chr>
      
      $`Vital signs`
      # A tibble: 478 x 25
         site_code subject_id event_id event_date event_repeat form_id form_repeat
         <chr>     <chr>      <chr>    <date>            <int> <chr>         <int>
       1 BEL04     BEL_04_133 SCR      2023-06-05            1 VS                1
       2 BEL04     BEL_04_133 SCR      2023-06-05            1 VS                1
       3 BEL04     BEL_04_133 SCR      2023-06-05            1 VS                1
       4 BEL04     BEL_04_133 SCR      2023-06-05            1 VS                1
       5 BEL04     BEL_04_133 SCR      2023-06-05            1 VS                1
       6 BEL04     BEL_04_133 SCR      2023-06-05            1 VS                1
       7 BEL04     BEL_04_133 SCR      2023-06-05            1 VS                1
       8 BEL04     BEL_04_133 VIS1     2023-07-05            1 VS                2
       9 BEL04     BEL_04_133 VIS1     2023-07-05            1 VS                2
      10 BEL04     BEL_04_133 VIS1     2023-07-05            1 VS                2
      # i 468 more rows
      # i 18 more variables: edit_date_time <dttm>, day <drtn>, event_name <chr>,
      #   event_label <fct>, form_type <chr>, item_name <fct>, item_type <chr>,
      #   item_group <chr>, item_unit <chr>, lower_lim <dbl>, upper_lim <dbl>,
      #   item_value <dbl>, significance <fct>, reason_notdone <chr>, region <chr>,
      #   value_scaled <dbl>, out_of_lim <fct>, text_label <chr>
      

