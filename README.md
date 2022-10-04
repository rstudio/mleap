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

![Figure 1 - Train in Spark](man/readme/mleap-fit.png)

In practice, we can save the ML Pipeline Model (fitted model) as an
MLeap bundle (see Figure 1). MLeap serializes the pipeline steps and
model. The resulting Zip file can then be used in an external
environment that has MLeap. Once the MLeap bundle is loaded in the new
environment, new data can be passed to obtain predictions (see Figure
2).

![Figure 2 - Deploy with MLeap](man/readme/mleap-predict.png)

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
```

``` r
ml_write_to_bundle_transformed(
  x = sff_pipeline_model,  
  transformed_dataset = sff_test_predictions,  
  path = "sff.zip", 
  overwrite = TRUE
  )

spark_disconnect(sc) 
```

## MLeap Bundle (without Spark dependency)

``` r
sff_mleap_model <- mleap_load_bundle("inst/app/sff.zip")
sff_mleap_model
```

``` r
str(sff_mleap_model)
```

``` r
tibble(review = "worst bad thing I will never buy again", score = "") %>% 
  mleap_transform(sff_mleap_model, .) %>% 
  glimpse()
```

``` r
tibble(review = "I really loved the proudct best product", score = "") %>% 
  mleap_transform(sff_mleap_model, .) %>% 
  dplyr::glimpse()
```

## Shiny app

A very simple Shiny app

``` r
remotes::install_github("rstudio/mleap", ref = "fixes")
shiny::runApp(system.file(package = "mleap", "app"))
```
