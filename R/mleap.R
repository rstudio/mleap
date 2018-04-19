new_mleap_transformer <- function(jobj) {
  info <- jobj$info()
  structure(
    list(
      uid = info$uid()$toString(),
      name = info$name(),
      format = info$format()$toString(),
      mleap_version = info$version(),
      schema = retrieve_model_schema(jobj),
      .jobj = jobj
    ),
    class = "mleap_transformer"
  )
}

# no covered because uid different each run
#' @export
print.mleap_transformer <- function(x, ...) { # nocov start
   cat("MLeap Transformer\n")
   cat(paste0("<", x$uid, ">"), "\n")
   cat(paste0("  ", "Name: ", x$name), "\n")
   cat(paste0("  ", "Format: ", x$format), "\n")
   cat(paste0("  ", "MLeap Version: ", x$mleap_version))
} # nocov end

#' MLeap model schema
#' 
#' Returns the schema of an MLeap transformer.
#' 
#' @param x An MLeap model object.
#' @return A data frame of the model schema.
#' 
#' @export
mleap_model_schema <- function(x) {
  x$schema
}

retrieve_model_schema <- function(jobj) {
  schema <- jobj$root()$schema()$fields()
  ct <- rJava::.jnew("scala.reflect.ClassTag$")
  ct <- ct$`MODULE$`$apply(schema$head()$getClass())
  
  schema$toArray(ct) %>%
    as.list() %>%
    purrr::map(function(x) {
      data_type <- x$dataType()
      base_type <- data_type$base()$toString()
      dimensions <- tryCatch(
        data_type$dimensions()$get()$toIterable()$array() %>%
          paste0("(", ., ")", collapse = ", "),
        error = function(e) NA_character_
      )
      is_nullable <- data_type$isNullable()
      list(x$name(),
           base_type,
           is_nullable,
           dimensions)
      }) %>%
    purrr::transpose() %>%
    purrr::set_names(c("name", "type", "nullable", "dimension")) %>%
    purrr::map(unlist) %>%
    tibble::as_tibble()
}

#' Loads an MLeap bundle
#' 
#' @param path Path to the exported bundle zip file.
#' @return An MLeap model object.
#'
#' @export
mleap_load_bundle <- function(path) {
  # if mleap runtime jars aren't in class path (from package load),
  #   load jars
  if (!any(grepl("mleap-runtime", rJava::.jclassPath())))
    load_mleap_jars()
  
  ctx_builder <- rJava::.jnew("ml.combust.mleap.runtime.javadsl.ContextBuilder")
  ctx <- ctx_builder$createMleapContext()
  bundle_file <- rJava::.jnew("java.io.File", path)
  bundle_builder <- rJava::.jnew("ml.combust.mleap.runtime.javadsl.BundleBuilder")
  transformer <- bundle_builder$load(bundle_file, ctx)
  new_mleap_transformer(transformer)
}

#' Export a Spark pipeline for serving
#' 
#' This functions serializes a Spark pipeline model into an MLeap bundle.
#' 
#' @param x A Spark pipeline model object.
#' @param dataset A Spark DataFrame with the schema of the transformed DataFrame.
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
#'                 ml_transform(pipeline_model, mtcars_tbl),
#'                 model_path,
#'                 overwrite = TRUE)
#' }
#' 
#' @export
ml_write_bundle <- function(x, dataset, path, overwrite = FALSE) {
  stages <- if (purrr::is_bare_list(x)) {
    purrr::map(x, sparklyr::spark_jobj)
  } else {
    list(sparklyr::spark_jobj(x))
  }
  
  sc <- sparklyr::spark_connection(stages[[1]])
  sdf <- sparklyr::spark_dataframe(dataset)
  path <- resolve_path(path)

  if (fs::file_exists(path)) {
    if (!overwrite) {
      stop(paste0("Can't save bundle file: ", basename(path), " already exists."),
           call. = FALSE)
    } else
      fs::file_delete(path)
  }
  
  sparklyr::invoke_static(sc, "mleap.Main", "exportArrayToBundle",
                          sdf, uri(path), stages)
  message("Model successfully exported.")
  invisible(NULL)
}
