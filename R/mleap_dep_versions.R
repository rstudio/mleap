#' A table containing the combinations of Spark, Scala and MLeap versions to use
#' @export
mleap_dep_versions_table <- function() {
  mleap_dep_versions_list() %>%
    map(~ as_tibble(.x)) %>%
    reduce(function(x, y) rbind(x, y))
}

mleap_dep_versions_list <- function(spark_version = NULL, scala_version = NULL,
                                    mleap_version = NULL) {
  ver <- list(
    list(spark = "2.0.0", scala = "2.11", mleap = "0.10.3"),
    list(spark = "2.1.0", scala = "2.11", mleap = "0.11.0"),
    list(spark = "2.2.0", scala = "2.11", mleap = "0.11.0"),
    list(spark = "2.3.0", scala = "2.11", mleap = "0.13.0"),
    list(spark = "2.4.0", scala = "2.11", mleap = "0.15.0"),
    list(spark = "2.4.5", scala = "2.12", mleap = "0.17.0"),
    list(spark = "3.0.2", scala = "2.12", mleap = "0.18.1"),
    list(spark = "3.2.0", scala = "2.12", mleap = "0.20.0")
  )
  prep_ver <- map(
    ver,
    ~ {
      x <- .x
      x$maven <- sprintf("ml.combust.mleap:mleap-spark_%s:%s", x$scala, x$mleap)
      x$spark_major <- substr(x$spark, 1, 3)
      x$jar_name <- sprintf("mleap-%s-%s.jar", x$spark_major, x$scala)
      x
    }
  )

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
