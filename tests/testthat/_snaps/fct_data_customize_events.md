# clean_event_metadata() works: works with only event_id and adds other required columns

    Code
      print(output, width = Inf)
    Output
      # A tibble: 3 x 10
        event_id event_id_pattern event_name_custom event_label_custom
        <chr>    <chr>            <chr>             <chr>             
      1 SCR      ^SCR$            <NA>              <NA>              
      2 START    ^START$          <NA>              <NA>              
      3 EXIT     ^EXIT$           <NA>              <NA>              
        is_regular_visit is_baseline_event generate_labels meta_event_order
        <lgl>            <lgl>             <lgl>                      <int>
      1 TRUE             TRUE              FALSE                          1
      2 TRUE             FALSE             FALSE                          2
      3 TRUE             FALSE             FALSE                          3
        add_visit_number add_event_repeat_number
        <lgl>            <lgl>                  
      1 FALSE            FALSE                  
      2 FALSE            FALSE                  
      3 FALSE            FALSE                  

