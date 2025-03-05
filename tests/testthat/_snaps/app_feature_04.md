# Feature 4 | Load data from CSV files, merge with custom metadata, and start the application.As a user, I want to be able to load data from the raw csv file output from the EDC system and use a different metadata file to customize the application.: Scenario 1 - Load raw data with custom metadata. Given raw CSV data exported from the EDC system from subject [9600-002], and with metadata in which total visits is restricted to V0-V10, and with only the study_forms 'Response' and 'Vitals adjusted' in the metadataand with, at both Screening and Visit 2 the [Systolic blood pressure] being 99, [Diastolic Blood Pressure] 77, [Pulse] 77, [Resp] 9, and [Temperature] 37.5, and (only at screening) [Weight] 70kg, and that I browse to the 'Common events' tabI expect that I see the timeline of subject [9600-002] with study visits in itand when I browser to the 'Study data' tabI expect that I see the Vital signs page of subject [9600-002]', and that the compact header timeline shows visits V0-V10 and that I see a figure with the data displayed, and that I see a table with the data displayed after clicking on the table view, and that the data for the figure and table in the app is the same as the raw data in the CSV files.

    Code
      fig_data
    Output
      # A tibble: 12 x 9
         subject_id event_name event_date item_group    item_name item_value item_unit
         <chr>      <chr>      <date>     <chr>         <fct>          <dbl> <chr>    
       1 9600-002   Screening  2022-01-01 Vitals adjus~ Systolic~       99   mmHg     
       2 9600-002   Visit 2    2022-09-11 Vitals adjus~ Systolic~       99   mmHg     
       3 9600-002   Screening  2022-01-01 Vitals adjus~ Diastoli~       77   mmHg     
       4 9600-002   Visit 2    2022-09-11 Vitals adjus~ Diastoli~       77   mmHg     
       5 9600-002   Screening  2022-01-01 Vitals adjus~ Pulse           77   beats/min
       6 9600-002   Visit 2    2022-09-11 Vitals adjus~ Pulse           77   beats/min
       7 9600-002   Screening  2022-01-01 Vitals adjus~ Resp             9   breaths/~
       8 9600-002   Visit 2    2022-09-11 Vitals adjus~ Resp             9   breaths/~
       9 9600-002   Screening  2022-01-01 Vitals adjus~ Temperat~       37.5 °C       
      10 9600-002   Visit 2    2022-09-11 Vitals adjus~ Temperat~       37.5 °C       
      11 9600-002   Screening  2022-01-01 Vitals adjus~ BMI             22.1 kg/m2    
      12 9600-002   Screening  2022-01-01 Vitals adjus~ Weight          70   kg       
      # i 2 more variables: lower_lim <dbl>, upper_lim <dbl>

---

    Code
      print(table_data, width = Inf)
    Output
      # A tibble: 2 x 11
        o_reviewed       subject_id event_name `Systolic blood pressure`
        <list>           <chr>      <chr>      <chr>                    
      1 <named list [5]> 9600-002   Screening  <b>99*</b> mmHg          
      2 <named list [5]> 9600-002   Visit 2    <b>99*</b> mmHg          
        `Diastolic blood pressure` Pulse                Resp                 
        <chr>                      <chr>                <chr>                
      1 <b>77*</b> mmHg            <b>77*</b> beats/min <b>9*</b> breaths/min
      2 <b>77*</b> mmHg            <b>77*</b> beats/min <b>9*</b> breaths/min
        Temperature     `Weight change since screening` BMI                
        <chr>           <chr>                           <chr>              
      1 <b>37.5*</b> °C <NA>                            <b>22.09*</b> kg/m2
      2 <b>37.5*</b> °C <NA>                            <NA>               
        Weight       
        <chr>        
      1 <b>70*</b> kg
      2 <NA>         

