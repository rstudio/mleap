.globals <- new.env(parent = emptyenv())

config <- jsonlite::fromJSON(system.file(file.path("extdata", "config.json"), 
                                         package = packageName()
)
)
.globals$default_maven_version <- config$maven_version
.globals$default_mleap_version <- config$mleap_version
.globals$default_maven_repo <- config$maven_repo

.globals$maven_dir <- NULL
.globals$mleap_dir <- NULL

utils::globalVariables(".")

resolve_path <- function(path) {
  if (grepl("[a-zA-Z]+://", path)) {
    path
  }
  else {
    dir_path <- path %>%
      normalizePath(mustWork = FALSE, winslash = "/") %>%
      dirname() %>%
      tools::file_path_as_absolute()
    file.path(dir_path, basename(path))
  }
}

uri <- function(path) {
  prefix <- if (.Platform$OS.type == "windows") "file:///" else "file:"
  paste0(prefix, path)
}


