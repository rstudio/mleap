.onLoad <- function(libname, pkgname) {
  
  rjava_java_home <- Sys.getenv("RJAVA_JAVA_HOME")
  if(!is.null(rjava_java_home)) {
    old_java_home <- Sys.getenv("JAVA_HOME")
    Sys.setenv(JAVA_HOME = rjava_java_home)
    rJava::.jinit()
    Sys.setenv(JAVA_HOME = old_java_home)
  }
  
  sparklyr::register_extension(pkgname)
  jar_paths <- tryCatch(
    list.files(resolve_mleap_path(), full.names = TRUE),
    error = function(e) ""
  )

  .jpackage(pkgname, lib.loc = libname, morePaths = jar_paths)
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
