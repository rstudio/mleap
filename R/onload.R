.onLoad <- function(libname, pkgname) {
  
  sparklyr::register_extension(pkgname)
  
}

spark_dependencies <- function(spark_version, scala_version, ...) {
  mleap_version <- mleap_dep_versions_list(spark_version, scala_version)

  sparklyr::spark_dependency(
    jars = c(
      system.file(
        path("java", mleap_version$jar_name),
        package = "mleap",
        mustWork = TRUE
      )
    ),
    packages = mleap_version$maven
  )
}

temp_tibble <- function(x) tibble::as_tibble(x)
