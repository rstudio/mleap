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
  
  model_schema <- model$schema
  types <- columns %>%
    purrr::map_chr(~ model_schema$type[[match(.x, model_schema$name)]])
  
  schema <- list(fields = purrr::map2(
    columns, types, ~ list(name = .x, type = .y))
    )
  rows <- data %>%
    apply(1, as.list) %>%
    purrr::map(unname)
  
  data_json <- list(schema = schema, rows = rows) %>%
    jsonlite::toJSON(auto_unbox = TRUE)
  
  data_bytes <- rJava::J("scala.io.Source")$fromString(as.character(data_json)) %>%
    rJava::.jcall("S", "mkString", evalString = FALSE) %>%
    rJava::.jcall("[B", "getBytes", evalArray= FALSE)
  
  frame_reader <- rJava::J("ml.combust.mleap.runtime.serialization.FrameReader$")
  frame_reader <- frame_reader$`MODULE$`$apply(
    "ml.combust.mleap.json", 
    frame_reader$`MODULE$`$`apply$default$2`()
  )
  frame <- frame_reader$fromBytes(data_bytes, frame_reader$`fromBytes$default$2`())
  model <- model$.jobj$root()
  output_frame <- model$transform(frame$get())$get()
  
  ct <- rJava::J("scala.reflect.ClassTag$")
  ct <- ct$`MODULE$`$apply(output_frame$getClass())
  
  frame_writer <- rJava::J("ml.combust.mleap.runtime.serialization.FrameWriter$")
  frame_writer <- frame_writer$`MODULE$`$apply(
    output_frame,
    frame_writer$`MODULE$`$`apply$default$2`(), 
    frame_writer$`MODULE$`$`apply$default$3`(),
    ct)
  
  parse_mleap_json <- function(x) {
    col_names <- x$schema$fields %>%
      purrr::map_chr("name")
    x$rows %>%
      purrr::transpose() %>%
      purrr::set_names(col_names) %>%
      purrr::map_if(~ !is.list(.x[[1]]), unlist) %>%
      tibble::as_tibble()
  }
  
  frame_writer$toBytes(frame_writer$`toBytes$default$1`())$get() %>%
    rawToChar() %>%
    jsonlite::fromJSON(simplifyVector = FALSE) %>%
    parse_mleap_json()
}