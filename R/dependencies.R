spark_dependencies <- function(spark_version, scala_version, ...) {
  mleap_version <- if (spark_version >= "2.4.0") {
    "0.16.0"
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

#' @import sparklyr
.onLoad <- function(libname, pkgname) { # nocov start
  sparklyr::register_extension(pkgname)
  jar_paths <- tryCatch(
    list.files(resolve_mleap_path(), full.names = TRUE),
    error = function(e) ""
  )
  rJava::.jpackage(pkgname, lib.loc = libname, 
                   morePaths = jar_paths)
} # nocov end
