#' Export a Spark pipeline for serving
#' 
#' This functions serializes a Spark pipeline model into an MLeap bundle.
#' 
#' @param x A Spark pipeline model object.
#' @param sample_input A sample input Spark DataFrame with the expected schema.
#' @param path Where to save the bundle.
#' @param overwrite Whether to overwrite an existing file, defaults to \code{FALSE}.
#' 
#' @examples
#' \dontrun{
#' library(sparklyr)
#' sc <- spark_connect(master = "local")
#' mtcars_tbl <- sdf_copy_to(sc, mtcars, overwrite = TRUE)
#' pipeline <- ml_pipeline(sc) %>%
#'   ft_binarizer("hp", "big_hp", threshold = 100) %>%
#'   ft_vector_assembler(c("big_hp", "wt", "qsec"), "features") %>%
#'   ml_gbt_regressor(label_col = "mpg")
#' pipeline_model <- ml_fit(pipeline, mtcars_tbl)
#' model_path <- file.path(tempdir(), "mtcars_model.zip")
#' ml_write_bundle(pipeline_model, 
#'                 mtcars_tbl,
#'                 model_path,
#'                 overwrite = TRUE)
#' }
#' 
#' @export
ml_write_bundle <- function(x, sample_input, path, overwrite = FALSE) {
  stages <- if (purrr::is_bare_list(x)) {
    purrr::map(x, spark_jobj)
  } else {
    list(spark_jobj(x))
  }
  
  sc <- spark_connection(stages[[1]])
  
  sdf <- x %>% 
    ml_transform(sample_input) %>% 
    spark_dataframe()
  
  path <- resolve_path(path)
  
  if (!identical(fs::path_ext(path), "zip"))
    stop("The bundle path must have a `.zip` extension.", call. = FALSE)
  
  if (fs::file_exists(path)) {
    if (!overwrite) {
      stop(paste0("Can't save bundle file: ", basename(path), " already exists."),
           call. = FALSE)
    } else
      fs::file_delete(path)
  }
  
  invoke_static(sc, "mleap.Main", "exportTransformer", sdf, uri(path), stages)
  message("Model successfully exported.")
  invisible(NULL)
}
