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
