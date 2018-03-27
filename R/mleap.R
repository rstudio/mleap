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

#' @export
print.mleap_transformer <- function(x, ...) {
   cat("MLeap Transformer\n")
   cat(paste0("<", x$uid, ">"), "\n")
   cat(paste0("  ", "Name: ", x$name), "\n")
   cat(paste0("  ", "Format: ", x$format), "\n")
   cat(paste0("  ", "MLeap Version: ", x$mleap_version))
}

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
  if (!length(list.files(resolve_mleap_path())))
    stop("Can't find MLeap jars. Please set options(mleap.home = ...) or run install_mleap().",
         call. = FALSE)
  ctx_builder <- rJava::.jnew("ml.combust.mleap.runtime.javadsl.ContextBuilder")
  ctx <- ctx_builder$createMleapContext()
  bundle_file <- rJava::.jnew("java.io.File", resolve_path(path))
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
#' @export
ml_save_bundle <- function(x, dataset, path, overwrite = FALSE) {
  stages <- if (purrr::is_bare_list(x)) {
    purrr::map(x, sparklyr::spark_jobj)
  } else {
    list(sparklyr::spark_jobj(x))
  }
  
  sc <- sparklyr::spark_connection(stages[[1]])
  sdf <- sparklyr::spark_dataframe(dataset)
  path <- resolve_path(path)
  
  is_local <- stages[[1]] %>%
    sparklyr::spark_connection() %>%
    sparklyr::spark_context() %>%
    sparklyr::invoke("isLocal")
  
  # for local spark connection, we can check to see if
  #   file exists on local filesystem
  if (is_local && fs::file_exists(path)) {
    if (!overwrite) {
      stop(paste0("Can't save bundle file: ", basename(path), " already exists."),
           call. = FALSE)
    } else
      fs::file_delete(path)
  }
  
  sparklyr::invoke_static(sc, "mleap.Main", "exportArrayToBundle",
                          sdf, path, stages)
  message("Model successfully exported.")
  invisible(NULL)
}
