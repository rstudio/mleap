R interface for MLeap
================

[![Travis build
status](https://travis-ci.org/rstudio/mleap.svg?branch=master)](https://travis-ci.org/rstudio/mleap)
[![Coverage
status](https://codecov.io/gh/rstudio/mleap/branch/master/graph/badge.svg)](https://codecov.io/github/rstudio/mleap?branch=master)

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

We can now export Spark ML pipelines from sparklyr.

``` r
library(sparklyr)
sc <- spark_connect(master = "local")
mtcars_tbl <- sdf_copy_to(sc, mtcars, overwrite = TRUE)

# Create a pipeline and fit it
pipeline <- ml_pipeline(sc) %>%
  ft_binarizer("hp", "big_hp", threshold = 100) %>%
  ft_vector_assembler(c("big_hp", "wt", "qsec"), "features") %>%
  ml_gbt_regressor(label_col = "mpg")
pipeline_model <- ml_fit(pipeline, mtcars_tbl)

# A transformed data frame with the appropriate schema is required
#   for exporting the pipeline model
transformed_tbl <- ml_transform(pipeline_model, mtcars_tbl)

# Export model
model_path <- file.path(tempdir(), "mtcars_model.zip")
ml_write_bundle(pipeline_model, transformed_tbl, model_path)

# Disconnect from Spark
spark_disconnect(sc)
```

At this point, we can share `mtcars_model.zip` with our
deployment/implementation engineers, and they would be able to embed the
model in another application. See the [MLeap
docs](http://mleap-docs.combust.ml/) for details.

We also provide R functions for testing that the saved models behave as
expected. Here we load the previously saved model:

``` r
model <- mleap_load_bundle(model_path)
model
```

    ## MLeap Transformer
    ## <fcf4647e-31ee-4d8e-b620-05b28c23a4c0> 
    ##   Name: pipeline_930b6090bbf9 
    ##   Format: json 
    ##   MLeap Version: 0.10.0-SNAPSHOT

We can retrieve the schema associated with the model:

``` r
mleap_model_schema(model)
```

    ## # A tibble: 6 x 4
    ##   name       type   nullable dimension
    ##   <chr>      <chr>  <lgl>    <chr>    
    ## 1 qsec       double TRUE     <NA>     
    ## 2 hp         double FALSE    <NA>     
    ## 3 wt         double TRUE     <NA>     
    ## 4 big_hp     double FALSE    <NA>     
    ## 5 features   double TRUE     (3)      
    ## 6 prediction double FALSE    <NA>

Then, we create a new data frame to be scored, and make predictions
using our model:

``` r
newdata <- tibble::tribble(
  ~qsec, ~hp, ~wt,
  16.2,  101, 2.68,
  18.1,  99,  3.08
)

# Transform the data frame
transformed_df <- mleap_transform(model, newdata)
dplyr::glimpse(transformed_df)
```

    ## Observations: 2
    ## Variables: 6
    ## $ qsec       <dbl> 16.2, 18.1
    ## $ hp         <dbl> 101, 99
    ## $ wt         <dbl> 2.68, 3.08
    ## $ big_hp     <dbl> 1, 0
    ## $ features   <list> [[[1, 2.68, 16.2], [3]], [[0, 3.08, 18.1], [3]]]
    ## $ prediction <dbl> 21.06529, 22.36667
