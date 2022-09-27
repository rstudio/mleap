R interface for MLeap
================

[![Travis build
status](https://travis-ci.org/rstudio/mleap.svg?branch=master)](https://travis-ci.org/rstudio/mleap)
[![Coverage
status](https://codecov.io/gh/rstudio/mleap/branch/master/graph/badge.svg)](https://codecov.io/github/rstudio/mleap?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/mleap)](https://cran.r-project.org/package=mleap)

**mleap** is a [sparklyr](http://spark.rstudio.com/) extension that
provides an interface to [MLeap](https://github.com/combust/mleap),
which allows us to take Spark pipelines to production.

## Getting started

**mleap** can be installed from CRAN via

``` r
install.packages("mleap")
```

or, for the latest development version from GitHub, using

``` r
devtools::install_github("rstudio/mleap")
```

Once mleap has been installed, we can install the external dependencies
using

``` r
library(mleap)
install_maven()
# Alternatively, if you already have Maven installed, you can 
#  set options(maven.home = "path/to/maven")
install_mleap()
```

``` r
library(sparklyr)

Attaching package: 'sparklyr'
The following object is masked from 'package:stats':

    filter
library(modeldata)

data("small_fine_foods")

sc <- spark_connect(master = "local", version = "3.2")

sff_training_data <- copy_to(sc, training_data)

sff_testing_data <- copy_to(sc, testing_data)
```

``` r
sff_pipeline <- ml_pipeline(sc) %>% 
  ft_string_indexer(
    input_col = "score",
    output_col = "label",
    handle_invalid = "keep",
    string_order_type = "alphabetAsc"
  ) %>% 
  ft_tokenizer(
    input_col = "review",
    output_col = "word_list"
  ) %>% 
  ft_stop_words_remover(
    input_col = "word_list", 
    output_col = "wo_stop_words"
    ) %>% 
  ft_hashing_tf(
    input_col = "wo_stop_words", 
    output_col = "hashed_features", 
    binary = TRUE, 
    num_features = 4096
    ) %>%
  ft_normalizer(
    input_col = "hashed_features", 
    output_col = "normal_features"
    ) %>% 
  ft_vector_assembler(
    input_cols = "normal_features",
    output_col = "features"
  ) %>% 
  ml_logistic_regression(elastic_net_param = 0.05, reg_param = 0.25  )  

sff_pipeline_model <- ml_fit(sff_pipeline, sff_training_data)

sff_test_predictions <- sff_pipeline_model %>% 
  ml_transform(sff_testing_data) 

ml_metrics_binary(sff_test_predictions, truth = label, estimate = prediction)
# A tibble: 2 × 3
  .metric .estimator .estimate
  <chr>   <chr>          <dbl>
1 roc_auc binary         0.610
2 pr_auc  binary         0.580
```

``` r
ml_write_to_bundle_transformed(
  x = sff_pipeline_model,  
  transformed_dataset = sff_test_predictions,  
  path = "sff.zip", 
  overwrite = TRUE
  )
Model successfully exported.

spark_disconnect(sc) 
```

## MLeap Bundle (without Spark dependency)

``` r
sff_mleap_model <- mleap_load_bundle("sff.zip")
sff_mleap_model
MLeap Transformer
<64530c35-0f54-41ff-ac1d-4853baff67c8> 
  Name: pipeline__806171d5_4dd9_46d8_8e9c_361fc28978b8 
  Format: json 
  MLeap Version: 0.20.0
```

``` r
mleap_model_schema(sff_mleap_model)
# A tibble: 11 × 5
   name            type   nullable dimension io    
   <chr>           <chr>  <lgl>    <chr>     <chr> 
 1 review          string FALSE    <NA>      input 
 2 score           string TRUE     <NA>      input 
 3 normal_features double TRUE     (4096)    output
 4 wo_stop_words   string TRUE     <NA>      output
 5 word_list       string TRUE     <NA>      output
 6 features        double TRUE     (4096)    output
 7 label           double FALSE    <NA>      output
 8 hashed_features double TRUE     (4096)    output
 9 prediction      double FALSE    <NA>      output
10 rawPrediction   double TRUE     (3)       output
11 probability     double TRUE     (3)       output
```

``` r
tibble(review = "worst bad thing I will never buy again", score = "") %>% 
  mleap_transform(sff_mleap_model, .) %>% 
  dplyr::glimpse()
Rows: 1
Columns: 11
$ review          <chr> "worst bad thing I will never buy again"
$ score           <chr> ""
$ label           <dbl> 2
$ word_list       <list> ["worst", "bad", "thing", "i", "will", "never", "buy",…
$ wo_stop_words   <list> ["worst", "bad", "thing", "never", "buy"]
$ hashed_features <list> [[[433], [768], [2020], [3081], [4092]], [1, 1, 1, 1, …
$ normal_features <list> [[[433], [768], [2020], [3081], [4092]], [0.4472136, 0…
$ features        <list> [[[433], [768], [2020], [3081], [4092]], [0.4472136, …
$ rawPrediction   <list> [[5.469893, 6.00653, -11.47642], [3]]
$ probability     <list> [[0.3689702, 0.6310298, 1.611768e-08], [3]]
$ prediction      <dbl> 1
```

``` r
tibble(review = "I really loved the proudct best product", score = "") %>% 
  mleap_transform(sff_mleap_model, .) %>% 
  dplyr::glimpse()
Rows: 1
Columns: 11
$ review          <chr> "I really loved the proudct best product"
$ score           <chr> ""
$ label           <dbl> 2
$ word_list       <list> ["i", "really", "loved", "the", "proudct", "best", "pr…
$ wo_stop_words   <list> ["really", "loved", "proudct", "best", "product"]
$ hashed_features <list> [[[2187], [2365], [3229], [3727], [3984]], [1, 1, 1, 1…
$ normal_features <list> [[[2187], [2365], [3229], [3727], [3984]], [0.4472136,…
$ features        <list> [[[2187], [2365], [3229], [3727], [3984]], [0.4472136…
$ rawPrediction   <list> [[6.708236, 4.768167, -11.47642], [3]]
$ probability     <list> [[0.8743598, 0.1256402, 1.107122e-08], [3]]
$ prediction      <dbl> 0
```
