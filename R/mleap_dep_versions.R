#' A table containing the combinations of Spark, Scala and MLeap versions to use
#' @export
mleap_dependency_versions <- function() {
  mleap_get_session_defaults("versions") 
}

mleap_dep_versions_list <- function(spark_version = NULL, 
                                    scala_version = NULL,
                                    mleap_version = NULL
                                    ) {
  prep_ver <- mleap_get_session_defaults("versions") %>% 
    transpose()

  if (!is.null(spark_version)) {
    ver_scala_lgl <- map_lgl(prep_ver, ~ .x$scala == scala_version)
    ver_scala <- prep_ver[ver_scala_lgl]
    spark_major <- substr(spark_version, 1, 3)
    ver_spark_lgl <- map_lgl(ver_scala, ~ .x$spark_major == spark_major)
    if (sum(ver_spark_lgl) > 1) {
      stop("More than one MLeap version matches the Spark and Scala versions")
    }
    if (sum(ver_spark_lgl) == 0) {
      stop(paste0("No MLeap JAR matched the requested Spark ", spark_major, " version"))
    }
    return(ver_scala[ver_spark_lgl][[1]])
  }

  if (!is.null(mleap_version)) {
    ver_mleap_lgl <- map_lgl(prep_ver, ~ .x$mleap == mleap_version)
    ver_mleap <- prep_ver[ver_mleap_lgl]
    return(head(ver_mleap, 1))
  }

  prep_ver
}
