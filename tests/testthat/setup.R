current_spark_env_version <- testthat_spark_env_version()
mleap_deps <- mleap_dep_versions()
spark_max <- max(mleap_deps$spark_major)

if(!(spark_max >= current_spark_env_version)) {
  testthat_spark_env_version(set_to = spark_max)
  spark_major <- spark_max
} else {
  spark_major <- substr(current_spark_env_version, 1, 3)
}

matched_dep <- mleap_deps[spark_major == mleap_deps$spark_major, ]

if(nrow(matched_dep) == 0) stop(paste0("No matched MLeap versions to Spark ", spark_major))

matched_dep <- matched_dep[1, ]

ver_spark <- matched_dep$spark
ver_mleap <- matched_dep$mleap

testthat_context$mleap_version <- ver_mleap

cat("\n----- mleap test setup ----")
cat("\nSpark:", ver_spark)
cat("\nMLeap:", ver_mleap)
cat("\n------------------------------\n")
cat("\n--- Creating Spark session ---\n")
testthat_spark_connection()
cat("------------------------------\n\n")

## Disconnects all at the end
withr::defer(spark_disconnect_all(), teardown_env())



