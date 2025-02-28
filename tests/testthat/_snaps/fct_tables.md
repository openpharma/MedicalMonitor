# Create_table.default() works and creates a table in wide format.: Creates expected table output

    Code
      outcome
    Output
      # A tibble: 4 x 5
        ID    Site  item1     item3      item2
        <chr> <chr> <chr>     <chr>      <chr>
      1 ID1   Site2 16; 34; 1 <NA>       <NA> 
      2 ID2   Site2 47; 29    <NA>       <NA> 
      3 ID2   Site1 <NA>      41; 26; 44 8    
      4 ID1   Site1 <NA>      17         <NA> 

# create_table.continuous() works.: Produces the expected output

    Code
      output
    Output
      # A tibble: 6 x 5
        ID    Site  item1                                 item3         item2         
        <chr> <chr> <chr>                                 <chr>         <chr>         
      1 ID1   Site1 "3 Donkey power"                      <NA>          "1 mL"        
      2 ID3   Site2 "1 mL; missing (my fish was unwell) " 1 Hubble-barn "1 mL"        
      3 ID1   Site2  <NA>                                 <NA>          "missing (my ~
      4 ID2   Site2  <NA>                                 <NA>          "1 mL"        
      5 ID2   Site1  <NA>                                 <NA>          "3 "          
      6 ID3   Site1  <NA>                                 <NA>          "3 "          

# create_table.continuous() works.: Ignores explanation_column and unit_column if they are NULL

    Code
      output
    Output
      # A tibble: 6 x 5
        ID    Site  item1                           item3 item2                      
        <chr> <chr> <chr>                           <chr> <chr>                      
      1 ID1   Site1 "3 "                            <NA>  "1 "                       
      2 ID3   Site2 "1 ; missing (reason unknown) " "1 "  "1 "                       
      3 ID1   Site2  <NA>                           <NA>  "missing (reason unknown) "
      4 ID2   Site2  <NA>                           <NA>  "1 "                       
      5 ID2   Site1  <NA>                           <NA>  "3 "                       
      6 ID3   Site1  <NA>                           <NA>  "3 "                       

# create_table.general() works: creates expected table

    Code
      print(create_table(df, expected_columns = expected_cols), n = 25)
    Output
      # A tibble: 25 x 23
         subject_id Age   Sex    ECOG  Eligible Eligible_Date WHO.classification
         <chr>      <chr> <chr>  <chr> <chr>    <chr>         <chr>             
       1 BEL_04_133 88    Male   1     Yes      2023-07-06    Syndrome K        
       2 BEL_04_772 78    Male   0     Yes      2023-08-17    Syndrome O        
       3 BEL_07_193 26    Female 1     Yes      2023-08-23    Syndrome D        
       4 BEL_07_431 42    Male   1     <NA>     <NA>          <NA>              
       5 BEL_07_497 50    Female <NA>  Yes      <NA>          <NA>              
       6 BEL_07_645 46    Male   1     Yes      2023-06-07    Syndrome J        
       7 BEL_08_45  64    Male   2     Yes      <NA>          Syndrome V        
       8 BEL_08_736 45    Female 0     Yes      2023-08-17    Syndrome A        
       9 BEL_08_885 82    Male   1     Yes      2023-07-05    Syndrome S        
      10 BEL_09_361 38    Male   0     Yes      2023-07-05    Syndrome G        
      11 BEL_09_464 44    Male   2     Yes      2023-08-14    Syndrome F        
      12 BEL_09_556 93    Male   <NA>  <NA>     <NA>          <NA>              
      13 DEU_01_541 74    Male   1     Yes      2023-08-30    Syndrome Y        
      14 DEU_01_977 59    Female 1     Yes      2023-08-09    Syndrome I        
      15 DEU_02_387 92    Female 0     Yes      2023-08-02    Syndrome Q        
      16 DEU_02_482 40    Male   1     Yes      2023-07-04    Syndrome T        
      17 DEU_02_866 31    Male   1     Yes      2023-08-08    Syndrome C        
      18 DEU_02_968 25    Female 1     Yes      <NA>          Syndrome P        
      19 NLD_03_207 68    Male   1     Yes      2023-07-12    Syndrome E        
      20 NLD_05_282 82    Female <NA>  Yes      <NA>          <NA>              
      21 NLD_05_561 38    Female 1     Yes      2023-08-28    Syndrome M        
      22 NLD_06_72  82    Female 1     Yes      2023-07-18    Syndrome Z        
      23 NLD_06_755 65    Male   1     Yes      2023-07-07    Syndrome W        
      24 NLD_06_893 41    Male   0     Yes      2023-09-13    Syndrome R        
      25 NLD_06_959 94    Male   1     Yes      <NA>          Syndrome X        
      # i 16 more variables: WHO.subclassification <chr>, Race <chr>,
      #   ChildbearingPotential <chr>, MenopauseReason <chr>,
      #   DiscontinuationDate <chr>, DiscontinuationReason <chr>,
      #   DisconDeathDate <chr>, DrugAdminDate <chr>, DrugAdminDose <chr>,
      #   DoseModificationDate <chr>, DoseModificationReason <chr>,
      #   DoseModificationNewDose <chr>, DrugDiscontDate <chr>,
      #   DrugDiscontReason <chr>, status <chr>, status_label <chr>

# create_table.adverse_events() works: creates expected AE table

    Code
      print(create_table(df, expected_columns = expected_cols), n = 25)
    Output
      # A tibble: 90 x 18
         subject_id form_repeat Name    AESI  `start date` `end date` `CTCAE severity`
         <chr>            <int> <chr>   <chr> <chr>        <chr>      <chr>           
       1 BEL_07_193           1 Atelec~ None  2023-09-14   <NA>       Grade 3         
       2 NLD_03_207           1 Sepsis  None  2023-08-14   2023-08-14 Grade 5         
       3 BEL_08_885           1 Seizure None  2023-08-11   <NA>       Grade 3         
       4 NLD_03_207           1 Sepsis  None  2023-08-02   2023-08-14 Grade 3         
       5 DEU_02_482           1 Pneumo~ None  2023-07-09   2023-07-18 Grade 3         
       6 DEU_02_482           2 Pneumo~ None  2023-08-19   2023-08-19 Grade 5         
       7 DEU_02_387           2 Sepsis  None  2023-08-04   2023-08-08 Grade 5         
       8 BEL_09_361           2 Seizure None  2023-06-13   <NA>       Grade 3         
       9 BEL_08_885           3 Seizure None  2023-08-29   2023-09-01 Grade 3         
      10 NLD_06_755           6 Pneumo~ None  2023-08-01   <NA>       Grade 3         
      11 BEL_09_361           7 Sepsis  None  2023-08-03   <NA>       Grade 4         
      12 NLD_06_755          22 Stroke  None  2023-08-24   2023-08-24 Grade 5         
      13 NLD_06_755          22 Stroke  None  2023-08-22   2023-08-24 Grade 4         
      14 BEL_08_736           1 Hypote~ None  2023-08-22   <NA>       Grade 4         
      15 DEU_01_977           1 Urinar~ None  2023-08-21   <NA>       Grade 1         
      16 NLD_06_72            1 Lower ~ None  2023-08-18   <NA>       Grade 1         
      17 DEU_01_541           1 Anemia  None  2023-08-16   <NA>       <NA>            
      18 DEU_02_866           1 Urinar~ None  2023-08-11   <NA>       Grade 1         
      19 NLD_05_561           1 Joint ~ None  2023-08-06   2023-08-17 Grade 3         
      20 NLD_06_755           1 Allerg~ None  2023-07-08   2023-07-18 Grade 3         
      21 BEL_04_772           1 Hypote~ None  2023-07-07   <NA>       Grade 1         
      22 DEU_02_387           1 Nausea  None  2023-07-07   2023-07-20 Grade 3         
      23 BEL_09_361           1 Lower ~ None  2023-06-08   2023-06-08 Grade 2         
      24 DEU_02_866           2 Anemia  None  2023-08-30   <NA>       Grade 3         
      25 NLD_05_561           2 Urinar~ None  2023-08-25   <NA>       <NA>            
      # i 65 more rows
      # i 11 more variables: `Treatment related` <chr>, `Treatment action` <chr>,
      #   `Other action` <chr>, `Serious Adverse Event` <chr>,
      #   `SAE Awareness date` <chr>, `SAE Start date` <chr>, `SAE End date` <chr>,
      #   `SAE Category` <chr>, `SAE outcome` <chr>, `SAE Date of death` <chr>,
      #   `SAE Death reason` <chr>

# create_table.medication() works: creates expected medication table

    Code
      print(create_table(df, expected_columns = expected_cols), n = 25)
    Output
      # A tibble: 283 x 10
         subject_id form_repeat Name  Indication Dose  `Start Date` Ongoing `End Date`
         <chr>            <int> <chr> <chr>      <chr> <chr>        <chr>   <chr>     
       1 DEU_01_541           1 Lisi~ Insomnia   300 ~ 2023-NK-NK   Yes     <NA>      
       2 DEU_01_541           2 Oxyc~ Infection  500 ~ 2023-NK-NK   Yes     <NA>      
       3 BEL_07_645           6 Lisi~ Nausea     5 Mi~ 2023-09-13   Yes     <NA>      
       4 BEL_07_645           7 Lisi~ Insomnia   20 M~ 2023-09-13   Yes     <NA>      
       5 BEL_07_193           9 Hydr~ Adverse e~ 100 ~ 2023-08-30   Yes     <NA>      
       6 BEL_07_193          10 Hydr~ Adverse e~ 8751~ 2023-08-30   Yes     <NA>      
       7 DEU_01_541           3 Pant~ Prophylax~ NA N~ 2023-08-30   Yes     <NA>      
       8 DEU_02_866          10 Acet~ Adverse e~ 500 ~ 2023-08-30   Yes     <NA>      
       9 BEL_04_772          15 Pant~ Pain       NA N~ 2023-08-23   Yes     <NA>      
      10 NLD_06_755          33 Para~ Nausea     10 M~ 2023-08-22   Yes     <NA>      
      11 NLD_06_755          34 Meto~ Hypertens~ 20 M~ 2023-08-22   Yes     <NA>      
      12 BEL_04_772          14 Hydr~ Adverse e~ 500 ~ 2023-08-16   Yes     <NA>      
      13 BEL_09_556           1 Pant~ Infection  70 M~ 2023-08-15   Yes     <NA>      
      14 BEL_09_556           2 Lora~ Insomnia   4500~ 2023-08-15   Yes     <NA>      
      15 BEL_08_736           4 Acet~ Infection  5 Mi~ 2023-08-08   Yes     <NA>      
      16 BEL_08_736           5 Meto~ Prophylax~ 20 M~ 2023-08-08   Yes     <NA>      
      17 NLD_06_755          28 Melo~ Prophylax~ 1000~ 2023-08-08   Yes     <NA>      
      18 NLD_06_755           9 Pant~ Adverse e~ 2 Gr~ 2023-08-01   Yes     <NA>      
      19 DEU_02_482          15 Melo~ Infection  NA M~ 2023-07-31   Yes     <NA>      
      20 BEL_09_464           8 Acet~ Pain       1 Mi~ 2023-07-28   Yes     <NA>      
      21 NLD_06_755          29 Cana~ Prophylax~ 1 Un~ 2023-07-26   Yes     <NA>      
      22 NLD_06_755          30 Hydr~ Insomnia   20 M~ 2023-07-23   Yes     <NA>      
      23 BEL_09_361          24 Acet~ Prophylax~ 500 ~ 2023-07-20   Yes     <NA>      
      24 BEL_09_361          25 Melo~ Hypertens~ 10 M~ 2023-07-20   Yes     <NA>      
      25 NLD_06_755          18 Meto~ Pain       1000~ 2023-07-19   Yes     <NA>      
      # i 258 more rows
      # i 2 more variables: `Related Medical History` <chr>, `Related AE` <chr>

# create_table.medical_history() works: creates expected medical history table

    Code
      print(create_table(df, expected_columns = expected_cols), n = 25)
    Output
      # A tibble: 152 x 8
         subject_id form_repeat Name         `Start Date` Ongoing `End Date` Treatment
         <chr>            <int> <chr>        <chr>        <chr>   <chr>      <chr>    
       1 BEL_04_133           1 Epilepsy     <NA>         Yes     <NA>       Medicati~
       2 BEL_04_772           1 Hypertension 2020-NK-NK   Yes     <NA>       Medicati~
       3 BEL_04_772           2 COPD         1995-NK-NK   Yes     <NA>       Medicati~
       4 BEL_04_772           3 Osteoporosis 2005-NK-NK   Yes     <NA>       Medicati~
       5 BEL_04_772           4 Congestive ~ 2020-NK-NK   Yes     <NA>       Medicati~
       6 BEL_04_772           5 Osteoporosis 2014-NK-NK   No      2014-NK-NK Surgery  
       7 BEL_04_772           6 COPD         2020-NK-NK   Yes     <NA>       Medicati~
       8 BEL_04_772           7 Hypertension 2015-NK-NK   Yes     <NA>       Medicati~
       9 BEL_04_772           8 Epilepsy     2023-01-NK   Yes     <NA>       Medicati~
      10 BEL_04_772           9 Arhtritis    2023-06-28   Yes     <NA>       No treat~
      11 BEL_07_193           1 Malaria      2023-05-12   Yes     <NA>       No treat~
      12 BEL_07_193           2 Familial Me~ 2023-06-02   Yes     <NA>       No treat~
      13 BEL_07_193           3 Hypothyroid~ 2021-01-04   Yes     <NA>       Non-drug~
      14 BEL_07_193           4 Osteoporosis 2022-09-23   Yes     <NA>       Non-drug~
      15 BEL_07_193           5 Diabetes me~ 2022-09-NK   Yes     <NA>       Medicati~
      16 BEL_07_193           6 Migraine     2022-09-30   Yes     <NA>       Medicati~
      17 BEL_07_193           7 Diabetes me~ 2019-09-NK   Yes     <NA>       Medicati~
      18 BEL_07_193           8 Hypertension 2015-05-NK   Yes     <NA>       Medicati~
      19 BEL_07_193           9 COPD         2019-09-NK   Yes     <NA>       Medicati~
      20 BEL_07_193          10 Congestive ~ 2019-09-NK   Yes     <NA>       No treat~
      21 BEL_07_193          11 Atrial fibr~ 2023-08-25   Yes     <NA>       Medicati~
      22 BEL_07_645           1 Sickle cell~ 2019-NK-NK   No      2019-NK-NK Medicati~
      23 BEL_08_45            1 Malaria      <NA>         Yes     <NA>       Medicati~
      24 BEL_08_45            2 Arhtritis    <NA>         Yes     <NA>       Medicati~
      25 BEL_08_45            3 Hypothyroid~ 2023-06-NK   <NA>    <NA>       <NA>     
      # i 127 more rows
      # i 1 more variable: Comment <chr>

# create_table.conc_procedures() works: creates expected table

    Code
      print(create_table(df, expected_columns = expected_cols), n = 25)
    Output
      # A tibble: 4 x 9
        subject_id form_repeat Name         Indication `Start Date` Ongoing `End Date`
        <chr>            <int> <chr>        <chr>      <chr>        <chr>   <chr>     
      1 BEL_07_645           1 Appendectomy Prophylax~ 2019-NK-NK   No      2019-NK-NK
      2 BEL_07_645           2 Amputation   Pain       2019-NK-NK   No      2019-NK-NK
      3 DEU_02_866           1 Thrombolysis Hypertens~ 2023-08-18   No      2023-08-18
      4 NLD_06_755           1 Amputation   Infection  2023-08-04   Yes     <NA>      
      # i 2 more variables: `Related Medical History` <chr>, `Related AE` <chr>

# create_table.bm_cytology() works: creates expected table

    Code
      print(create_table(df), n = 25)
    Output
      # A tibble: 1 x 8
        subject_id event_name event_repeat event_date `Bone marrow blasts`
        <chr>      <chr>             <dbl> <chr>      <chr>               
      1 885        Screening             1 2023-08-07 91                  
      # i 3 more variables: `BM smear assessment` <chr>, `Auer Rods` <chr>,
      #   `Ringed Sideroblasts` <chr>

# create_table.common_forms: creates expected medical history table

    Code
      print(create_table(df, expected_columns = expected_cols), n = 25)
    Output
      # A tibble: 152 x 9
         subject_id form_repeat `MH Number` `MH Name`     `MH Start Date` `MH Ongoing`
         <chr>            <int> <chr>       <chr>         <chr>           <chr>       
       1 BEL_04_133           1 1           Epilepsy      <NA>            Yes         
       2 BEL_04_772           1 1           Hypertension  2020-NK-NK      Yes         
       3 BEL_04_772           2 2           COPD          1995-NK-NK      Yes         
       4 BEL_04_772           3 3           Osteoporosis  2005-NK-NK      Yes         
       5 BEL_04_772           4 4           Congestive h~ 2020-NK-NK      Yes         
       6 BEL_04_772           5 5           Osteoporosis  2014-NK-NK      No          
       7 BEL_04_772           6 6           COPD          2020-NK-NK      Yes         
       8 BEL_04_772           7 7           Hypertension  2015-NK-NK      Yes         
       9 BEL_04_772           8 8           Epilepsy      2023-01-NK      Yes         
      10 BEL_04_772           9 9           Arhtritis     2023-06-28      Yes         
      11 BEL_07_193           1 1           Malaria       2023-05-12      Yes         
      12 BEL_07_193           2 2           Familial Med~ 2023-06-02      Yes         
      13 BEL_07_193           3 3           Hypothyroidi~ 2021-01-04      Yes         
      14 BEL_07_193           4 4           Osteoporosis  2022-09-23      Yes         
      15 BEL_07_193           5 5           Diabetes mel~ 2022-09-NK      Yes         
      16 BEL_07_193           6 6           Migraine      2022-09-30      Yes         
      17 BEL_07_193           7 7           Diabetes mel~ 2019-09-NK      Yes         
      18 BEL_07_193           8 8           Hypertension  2015-05-NK      Yes         
      19 BEL_07_193           9 9           COPD          2019-09-NK      Yes         
      20 BEL_07_193          10 10          Congestive h~ 2019-09-NK      Yes         
      21 BEL_07_193          11 11          Atrial fibri~ 2023-08-25      Yes         
      22 BEL_07_645           1 1           Sickle cell ~ 2019-NK-NK      No          
      23 BEL_08_45            1 1           Malaria       <NA>            Yes         
      24 BEL_08_45            2 2           Arhtritis     <NA>            Yes         
      25 BEL_08_45            3 3           Hypothyroidi~ 2023-06-NK      <NA>        
      # i 127 more rows
      # i 3 more variables: `MH End Date` <chr>, `MH Treatment` <chr>,
      #   `MH Comment` <chr>

