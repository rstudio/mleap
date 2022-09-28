# We can export and use pipeline model

    Code
      mleap_model_schema(model)
    Output
      # A tibble: 6 x 5
        name       type   nullable dimension io    
        <chr>      <chr>  <lgl>    <chr>     <chr> 
      1 qsec       double TRUE     <NA>      input 
      2 hp         double FALSE    <NA>      input 
      3 wt         double TRUE     <NA>      input 
      4 big_hp     double FALSE    <NA>      output
      5 features   double TRUE     (3)       output
      6 prediction double FALSE    <NA>      output

# We can export a list of transformers

    Code
      mleap_model_schema(model)
    Output
      # A tibble: 7 x 5
        name            type   nullable dimension io    
        <chr>           <chr>  <lgl>    <chr>     <chr> 
      1 Petal_Width     double TRUE     <NA>      input 
      2 Petal_Length    double TRUE     <NA>      input 
      3 predicted_label string TRUE     <NA>      output
      4 features        double TRUE     (2)       output
      5 prediction      double FALSE    <NA>      output
      6 rawPrediction   double TRUE     (3)       output
      7 probability     double TRUE     (3)       output

