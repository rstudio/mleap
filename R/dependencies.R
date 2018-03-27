spark_dependencies <- function(spark_version, scala_version, ...) {
  sparklyr::spark_dependency(
    jars = c(
      system.file(
        sprintf("java/mleap-%s-%s.jar", spark_version, scala_version),
        package = "mleap"
      )
    ),
    packages = c(
      sprintf("ml.combust.mleap:mleap-spark_%s:0.9.4", scala_version)
    )
  )
}

#' @import sparklyr
.onLoad <- function(libname, pkgname) {
  sparklyr::register_extension(pkgname)
  rJava::.jpackage(pkgname, lib.loc = libname, 
                   morePaths = list.files(resolve_mleap_path(),
                                          full.names = TRUE))
}
