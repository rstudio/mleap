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

spark_dependencies <- function(spark_version, scala_version, ...) {
  mleap_version <- if (spark_version >= "3.2.0") {
    "0.20.0"
  } else if (spark_version >= "3.0.2") {
    "0.19.0" 
  }
    else if (spark_version >= "2.4.0") {
    "0.15.0"
  } else if (spark_version >= "2.3.0") {
    "0.13.0"
  } else {
    "0.11.0"
  }
  
  sparklyr::spark_dependency(
    jars = c(
      system.file(
        sprintf("java/mleap-%s-%s.jar", spark_version, scala_version),
        package = "mleap",
        mustWork = TRUE
      )
    ),
    packages = c(
      sprintf("ml.combust.mleap:mleap-spark_%s:%s", scala_version, mleap_version)
    )
  )
}

temp_tibble <- function(x) tibble::as_tibble(x)
