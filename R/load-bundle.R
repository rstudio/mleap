#' Loads an MLeap bundle
#'
#' @param path Path to the exported bundle zip file.
#' @return An MLeap model object.
#'
#' @export
mleap_load_bundle <- function(path) {

  load_mleap_jars() 

  ctx_builder <- .jnew("ml.combust.mleap.runtime.javadsl.ContextBuilder")
  ctx <- .jcall(
    ctx_builder, 
    "Lml/combust/mleap/runtime/MleapContext;",
    "createMleapContext"
  )
  path <- normalizePath(path)
  bundle_file <- .jnew("java.io.File", path)
  bundle_builder <- .jnew("ml.combust.mleap.runtime.javadsl.BundleBuilder")
  transformer <- .jcall(
    bundle_builder,
    "Lml/combust/bundle/dsl/Bundle;",
    "load", 
    bundle_file, 
    ctx
  )
  new_mleap_transformer(transformer)
}

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

retrieve_model_schema <- function(jobj) {
  input_schema <- jobj$root()$inputSchema()$fields()
  output_schema <- jobj$root()$outputSchema()$fields()
  ct <- .jnew("scala.reflect.ClassTag$")
  ct <- ct$`MODULE$`$apply(input_schema$head()$getClass())

  get_schema_tbl <- function(schema, ct, io) {
    df <- schema$toArray(ct) |>
      as.list() |>
      map(function(x) {
        data_type <- x$dataType()
        base_type <- data_type$base()$toString()
        dimensions <- tryCatch(
          data_type$dimensions()$get()$toIterable()$array() |>
            paste0("(", ., ")", collapse = ", "),
          error = function(e) NA_character_
        )
        is_nullable <- data_type$isNullable()
        list(
          x$name(),
          base_type,
          is_nullable,
          dimensions
        )
      }) |>
      transpose() |>
      set_names(c("name", "type", "nullable", "dimension")) |>
      map(unlist) |>
      as_tibble()

    df$io <- io
    df
  }

  rbind(
    get_schema_tbl(input_schema, ct, "input"),
    get_schema_tbl(output_schema, ct, "output")
  )
}
