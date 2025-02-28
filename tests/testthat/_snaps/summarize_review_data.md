# summarize_review_data() works: Scenario 1 - Given a random data set provided, I expect that summary dataframe snapshot will be as expected

    Code
      summarize_review_data(review_df, common_vars = c("subject_id", "Form"),
      event_var = "Event", date_time_vars = "Edit date")
    Output
      # A tibble: 97 x 6
         subject_id Form            Event                  `Edit date` status reviewed
         <chr>      <chr>           <chr>                  <chr>       <chr>  <chr>   
       1 BEL_04_133 CBC regular     Screening, Visit 1, V~ 2023-09-13~ new/u~ No      
       2 BEL_04_133 Electrolytes    Screening, Visit 1, V~ 2023-08-30~ new/u~ No      
       3 BEL_04_133 Renal function  Screening, Visit 1, V~ 2023-08-30~ new/u~ No      
       4 BEL_04_133 Liver function  Screening, Visit 1, V~ 2023-08-30~ new/u~ No      
       5 BEL_04_133 Vital signs     Screening, Visit 1, V~ 2023-09-13~ new/u~ No      
       6 BEL_04_133 Medication      Any visit              2023-06-23~ new/u~ No      
       7 BEL_04_133 Medical History Any visit              2023-06-23~ updat~ No      
       8 BEL_04_133 Response        Visit 5                2023-09-13~ new/u~ No      
       9 BEL_04_772 CBC regular     Screening, Visit 1, V~ 2023-09-14~ new/u~ No      
      10 BEL_04_772 Electrolytes    Screening, Visit 1, V~ 2023-08-31~ new/u~ No      
      # i 87 more rows

