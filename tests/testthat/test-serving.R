context("save/load/predict")

test_that("We can export and use pipeline model", {
  skip_on_cran()
  sc <- testthat_spark_connection()
  
  library(sparklyr)
  mtcars_tbl <- sdf_copy_to(sc, mtcars, overwrite = TRUE)
  pipeline <- ml_pipeline(sc) %>%
    ft_binarizer("hp", "big_hp", threshold = 100) %>%
    ft_vector_assembler(c("big_hp", "wt", "qsec"), "features") %>%
    ml_gbt_regressor(label_col = "mpg")
  pipeline_model <- ml_fit(pipeline, mtcars_tbl)
  
  # export model
  model_path <- file.path(tempdir(), "mtcars_model.zip")
  expect_message(ml_write_bundle(pipeline_model, 
                                 ml_transform(pipeline_model, mtcars_tbl),
                                 model_path,
                                 overwrite = TRUE),
                 "Model successfully exported"
  )
  
  # error message when file exists
  expect_error(ml_write_bundle(pipeline_model, 
                               ml_transform(pipeline_model, mtcars_tbl),
                               model_path,
                               overwrite = FALSE),
               "*already exists\\.$"
  )
  
  # load model
  model <- mleap_load_bundle(model_path)
  
  # check model schema
  expect_known_output(
    mleap_model_schema(model),
    output_file("mtcars_model_schema.txt"),
    print = TRUE
  )
  
  newdata <- tibble::tribble(
    ~qsec, ~hp, ~wt,
    16.2,  101, 2.68,
    18.1,  99,  3.08
  )
  
  transformed_df <- mleap_transform(model, newdata)
  
  expect_identical(dim(transformed_df), c(2L, 6L))
  expect_identical(colnames(transformed_df),
                   c("qsec", "hp", "wt", "big_hp", "features", "prediction")
  )
})

test_that("We can export a list of transformers", {
  skip_on_cran()
  sc <- testthat_spark_connection()
  
  library(sparklyr)
  iris_tbl <- sdf_copy_to(sc, iris, overwrite = TRUE)
  string_indexer <- ft_string_indexer(sc, "Species", "label", dataset = iris_tbl)
  pipeline <- ml_pipeline(string_indexer) %>%
    ft_vector_assembler(c("Petal_Width", "Petal_Length"), "features") %>%
    ml_logistic_regression() %>%
    ft_index_to_string("prediction", "predicted_label",
                       labels = ml_labels(string_indexer))
  pipeline_model <- ml_fit(pipeline, iris_tbl)
  stages <- pipeline_model %>%
    ml_stages(c("vector_assembler", "logistic", "index_to_string"))
  transformed_tbl <- stages %>%
    purrr::reduce(sdf_transform, .init = iris_tbl)
  model_path <- file.path(tempdir(), "iris_model.zip")
  
  expect_message(
    ml_write_bundle(stages, transformed_tbl, model_path, overwrite = TRUE),
    "Model successfully exported"
  )
  
  # load model
  model <- mleap_load_bundle(model_path)
  
  expect_known_output(
    mleap_model_schema(model),
    output_file("iris_model_schema.txt"),
    print = TRUE
  )
  
  newdata <- tibble::tribble(
    ~Petal_Width, ~Petal_Length,
    1.4,          0.2,
    5.2,          1.8
  )
  
  newdata_tbl <- sdf_copy_to(sc, newdata, overwrite = TRUE)
  
  transformed_df <- mleap_transform(model, newdata)
  predictions_mleap <- transformed_df %>%
    dplyr::pull(predicted_label)
  
  predictions_spark <- ml_transform(
    pipeline_model, newdata_tbl) %>%
    dplyr::pull(predicted_label)
  
  expect_identical(dim(transformed_df), c(2L, 7L))
  expect_identical(colnames(transformed_df),
                   c("Petal_Width", "Petal_Length", "features",
                     "rawPrediction", "probability", "prediction",
                     "predicted_label")
  )
  expect_identical(predictions_mleap, predictions_spark)
  
})

test_that("mleap_transform() handles heterogenous predictors", {
  skip_on_cran()
  sc <- testthat_spark_connection()
  
  diamonds_tbl <- sdf_copy_to(sc, ggplot2::diamonds) %>%
    dplyr::mutate(price = as.numeric(price))
  
  pipeline <- ml_pipeline(sc) %>%
    ft_string_indexer("cut", "cut_cat") %>%
    ft_string_indexer("color", "color_cat") %>%
    ft_string_indexer("clarity", "clarity_cat") %>%
    ft_vector_assembler(
      c("carat", "cut_cat", "color_cat", "clarity_cat",
        "depth", "table", "x", "y", "z"),
      "features"
    ) %>%
    ml_gbt_regressor(label_col = "price", seed = 42)
  
  pipeline_model <- pipeline %>%
    ml_fit(diamonds_tbl)
  
  model_path <- file.path(tempdir(), "diamonds_model.zip")
  ml_write_bundle(
    pipeline_model,
    ml_transform(pipeline_model, diamonds_tbl),
    model_path)
  
  mleap_model <- mleap_load_bundle(model_path)
  
  pred_data <- data.frame(
    carat = 0.65, cut = "Good", color = "E",
    clarity = "VS1", depth = 60, table = 60,
    x = 4.5, y = 4.6, z = 2.7,
    stringsAsFactors = FALSE
  )
  pred_data_tbl <- sdf_copy_to(sc, pred_data)
  
  prediction_mleap <- mleap_transform(mleap_model, pred_data) %>%
    dplyr::pull(prediction)
  prediction_spark <- ml_transform(
    pipeline_model, 
    pred_data_tbl) %>%
    dplyr::pull(prediction)
  
  expect_equal(prediction_mleap, prediction_spark)
  
})