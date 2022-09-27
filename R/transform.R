#' Transform data using an MLeap model
#' 
#' This functions transforms an R data frame using an MLeap model.
#' 
#' @param model An MLeap model object, obtained by \code{mleap_load_bundle()}.
#' @param data An R data frame.
#' @return A transformed data frame.
#' @seealso [mleap_load_bundle()]
#' @export
mleap_transform <- function(model, data) {
  
  columns <- colnames(data)

  input_schema <- model$schema[model$schema$io == "input",]

  types <- columns %>%
    map_chr(~ input_schema$type[[match(.x, input_schema$name)]])
  

  types <- gsub("^int$", "integer", types)

  schema <- list(fields = map2(
    columns, types, ~ list(name = .x, type = .y))
  )
  rows <- data %>%
    transpose() %>%
    map(unname)

  data_json <- list(schema = schema, rows = rows) %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  data_bytes <- .jnew("scala.io.Source$") %>%
    .jcall("Lscala/io/Source;", "fromString", as.character(data_json)) %>%
    .jcall("S", "mkString", evalString = FALSE) %>%
    .jcall("[B", "getBytes", evalArray= FALSE)

  frame_reader <- .jnew("ml.combust.mleap.runtime.serialization.FrameReader$")
  frame_reader <- frame_reader %>%
    .jcall("Lml/combust/mleap/runtime/serialization/FrameReader;", "apply",
                  .jcall(frame_reader, "S", "apply$default$1"),
                  .jcall(frame_reader, "Lscala/Option;", "apply$default$2")
    )

  frame <- frame_reader %>%
    .jcall(
      "Lscala/util/Try;", "fromBytes",
      data_bytes,
      .jcall("java.nio.charset.Charset", "Ljava/nio/charset/Charset;", "forName", "UTF-8")
    )
  model <- model$.jobj$root()
  output_frame <- model$transform(frame$get())$get()

  ct <- J("scala.reflect.ClassTag$")
  ct <- ct$`MODULE$`$apply(output_frame$getClass())

  frame_writer <- J("ml.combust.mleap.runtime.serialization.FrameWriter$")
  frame_writer <- frame_writer$`MODULE$`$apply(
    output_frame,
    frame_writer$`MODULE$`$`apply$default$2`(),
    frame_writer$`MODULE$`$`apply$default$3`(),
    ct)

  parse_mleap_json <- function(x) {
    col_names <- x$schema$fields %>%
      map_chr("name")
    x$rows %>%
      transpose() %>%
      set_names(col_names) %>%
      map_if(~ !is.list(.x[[1]]), unlist) %>%
      tibble::as_tibble()
  }

  frame_writer$toBytes(frame_writer$`toBytes$default$1`())$get() %>%
    rawToChar() %>%
    jsonlite::fromJSON(simplifyVector = FALSE) %>%
    parse_mleap_json()
}

# no covered because uid different each run
#' @export
print.mleap_transformer <- function(x, ...) { # nocov start
  cat("MLeap Transformer\n")
  cat(paste0("<", x$uid, ">"), "\n")
  cat(paste0("  ", "Name: ", x$name), "\n")
  cat(paste0("  ", "Format: ", x$format), "\n")
  cat(paste0("  ", "MLeap Version: ", x$mleap_version))
} 
# nocov end
