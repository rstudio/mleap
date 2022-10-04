R interface for MLeap
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/rstudio/mleap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/mleap/actions/workflows/R-CMD-check.yaml)
[![MLeap-Tests](https://github.com/rstudio/mleap/actions/workflows/mleap-tests.yaml/badge.svg)](https://github.com/rstudio/mleap/actions/workflows/mleap-tests.yaml)
[![Coverage
status](https://codecov.io/gh/rstudio/mleap/branch/master/graph/badge.svg)](https://codecov.io/github/rstudio/mleap?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/mleap)](https://cran.r-project.org/package=mleap)
<!-- badges: end -->

## Intro

### MLeap

[MLeap](https://github.com/combust/mleap) allows us to take Spark
pipelines to production. The MLeap runtime can recreate most of Spark’s
feature transformers and model predictions. This allows for the ML
Pipeline to be **deployed with no Spark dependencies**.

<figure>
<img src="man/readme/mleap-fit.png" style="width:80.0%"
alt="Figure 1 - Train in Spark" />
<figcaption aria-hidden="true">Figure 1 - Train in Spark</figcaption>
</figure>

In practice, we can save the ML Pipeline Model (fitted model) as an
MLeap bundle (see Figure 1). MLeap serializes the pipeline steps and
model. The resulting Zip file can then be used in an external
environment that has MLeap. Once the MLeap bundle is loaded in the new
environment, new data can be passed to obtain predictions (see Figure
2).

<figure>
<img src="man/readme/mleap-predict.png" style="width:50.0%"
alt="Figure 2 - Deploy with MLeap" />
<figcaption aria-hidden="true">Figure 2 - Deploy with MLeap</figcaption>
</figure>

### `mleap` package

The goal of the `mleap` package is twofold:

1.  Convert an ML Pipeline Model created in `sparklyr`, into an MLeap
    bundle file

2.  Load an MLeap bundle file into an R session (without Spark), and to
    use the loaded bundle to run predictions (transformations)

## Getting started

**mleap** can be installed from CRAN via:

``` r
install.packages("mleap")
```

or, for the latest development version from GitHub, using:

``` r
devtools::install_github("rstudio/mleap")
```

## Install Maven and MLeap

Once `mleap` has been installed, we can install the external
dependencies using

``` r
library(mleap)

install_maven()
```

Alternatively, if you already have Maven installed, you can let `mleap`
know by setting an R option: `options(maven.home = "path/to/maven")`

``` r
install_mleap()
```

## Example

``` r
library(sparklyr)
#> 
#> Attaching package: 'sparklyr'
#> The following object is masked from 'package:stats':
#> 
#>     filter
library(modeldata)

data("small_fine_foods")

sc <- spark_connect(master = "local", version = "3.2")

sff_training_data <- copy_to(sc, training_data)

sff_testing_data <- copy_to(sc, testing_data)
```

``` r
sff_training_data %>% 
  ft_string_indexer(
    input_col = "score",
    output_col = "label",
    handle_invalid = "keep",
    string_order_type = "alphabetAsc"
  ) %>% 
  select(score, label) %>% 
  dplyr::count(score, label)
#> # Source: spark<?> [?? x 3]
#> # Groups: score
#>   score label     n
#>   <chr> <dbl> <dbl>
#> 1 other     1  1400
#> 2 great     0  2600
```

``` r
sff_pipeline <- ml_pipeline(sc) %>% 
  ft_string_indexer(
    input_col = "score",
    output_col = "label",
    handle_invalid = "keep",
    string_order_type = "alphabetDesc"
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
    num_features = 4096,
    binary = TRUE
    ) %>%
  ft_normalizer(
    input_col = "hashed_features", 
    output_col = "features"
    ) %>% 
  ml_logistic_regression(elastic_net_param = 0.05, reg_param = 0.25)  

sff_pipeline_model <- ml_fit(sff_pipeline, sff_training_data)

sff_test_predictions <- sff_pipeline_model %>% 
  ml_transform(sff_testing_data) 

ml_metrics_binary(sff_test_predictions, truth = label, estimate = prediction)
#> # A tibble: 2 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.610
#> 2 pr_auc  binary         0.713
```

``` r
ml_write_to_bundle_transformed(
  x = sff_pipeline_model,  
  transformed_dataset = sff_test_predictions,  
  path = "sff.zip", 
  overwrite = TRUE
  )
#> Model successfully exported.

spark_disconnect(sc) 
```

## MLeap Bundle (without Spark dependency)

``` r
sff_mleap_model <- mleap_load_bundle("inst/app/sff.zip")
sff_mleap_model
#> MLeap Transformer
#> <64530c35-0f54-41ff-ac1d-4853baff67c8> 
#>   Name: pipeline__806171d5_4dd9_46d8_8e9c_361fc28978b8 
#>   Format: json 
#>   MLeap Version: 0.20.0
```

``` r
str(sff_mleap_model)
#> List of 6
#>  $ uid          : chr "64530c35-0f54-41ff-ac1d-4853baff67c8"
#>  $ name         : chr "pipeline__806171d5_4dd9_46d8_8e9c_361fc28978b8"
#>  $ format       : chr "json"
#>  $ mleap_version: chr "0.20.0"
#>  $ schema       : tibble [11 × 5] (S3: tbl_df/tbl/data.frame)
#>   ..$ name     : chr [1:11] "review" "score" "normal_features" "wo_stop_words" ...
#>   ..$ type     : chr [1:11] "string" "string" "double" "string" ...
#>   ..$ nullable : logi [1:11] FALSE TRUE TRUE TRUE TRUE TRUE ...
#>   ..$ dimension: chr [1:11] NA NA "(4096)" NA ...
#>   ..$ io       : chr [1:11] "input" "input" "output" "output" ...
#>  $ .jobj        :Formal class 'jobjRef' [package "rJava"] with 2 slots
#>   .. ..@ jobj  :<externalptr> 
#>   .. ..@ jclass: chr "ml/combust/bundle/dsl/Bundle"
#>  - attr(*, "class")= chr "mleap_transformer"
```

``` r
tibble(review = "worst bad thing I will never buy again", score = "") %>% 
  mleap_transform(sff_mleap_model, .) %>% 
  glimpse()
#> Rows: 1
#> Columns: 11
#> $ review          <chr> "worst bad thing I will never buy again"
#> $ score           <chr> ""
#> $ label           <dbl> 2
#> $ word_list       <list> ["worst", "bad", "thing", "i", "will", "never", "buy",…
#> $ wo_stop_words   <list> ["worst", "bad", "thing", "never", "buy"]
#> $ hashed_features <list> [[[433], [768], [2020], [3081], [4092]], [1, 1, 1, 1, …
#> $ normal_features <list> [[[433], [768], [2020], [3081], [4092]], [0.4472136, 0…
#> $ features        <list> [[[433], [768], [2020], [3081], [4092]], [0.4472136, …
#> $ rawPrediction   <list> [[5.469893, 6.00653, -11.47642], [3]]
#> $ probability     <list> [[0.3689702, 0.6310298, 1.611768e-08], [3]]
#> $ prediction      <dbl> 1
```

``` r
tibble(review = "I really loved the proudct best product", score = "") %>% 
  mleap_transform(sff_mleap_model, .) %>% 
  dplyr::glimpse()
#> Rows: 1
#> Columns: 11
#> $ review          <chr> "I really loved the proudct best product"
#> $ score           <chr> ""
#> $ label           <dbl> 2
#> $ word_list       <list> ["i", "really", "loved", "the", "proudct", "best", "pr…
#> $ wo_stop_words   <list> ["really", "loved", "proudct", "best", "product"]
#> $ hashed_features <list> [[[2187], [2365], [3229], [3727], [3984]], [1, 1, 1, 1…
#> $ normal_features <list> [[[2187], [2365], [3229], [3727], [3984]], [0.4472136,…
#> $ features        <list> [[[2187], [2365], [3229], [3727], [3984]], [0.4472136…
#> $ rawPrediction   <list> [[6.708236, 4.768167, -11.47642], [3]]
#> $ probability     <list> [[0.8743598, 0.1256402, 1.107122e-08], [3]]
#> $ prediction      <dbl> 0
```

## Shiny app

A very simple Shiny app

``` r
remotes::install_github("rstudio/mleap", ref = "fixes")
shiny::runApp(system.file(package = "mleap", "app"))
```
