resolve_path <- function(path) {
  if (grepl("[a-zA-Z]+://", path)) {
    path
  }
  else {
    dir_path <- path %>%
      normalizePath(mustWork = FALSE, winslash = "/") %>%
      dirname() %>%
      tools::file_path_as_absolute()
    prefix <- if (.Platform$OS.type == "windows") "file:///" else "file:"
    paste0(prefix, file.path(dir_path, basename(path)))
  }
}

#' @importFrom purrr %||%
NULL