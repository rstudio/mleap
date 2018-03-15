resolve_path <- function(path) {
  if (grepl("[a-zA-Z]+://", path)) {
    path
  }
  else {
    dir_path <- path %>%
      normalizePath(mustWork = FALSE) %>%
      dirname() %>%
      tools::file_path_as_absolute()
    file.path(dir_path, basename(path))
  }
}