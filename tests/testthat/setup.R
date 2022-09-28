current_spark_env_version <- testthat_spark_env_version()
mleap_deps <- mleap_dep_versions_table()
mleap_max <- max(mleap_deps$spark_major)

cat("\n----- mleap test setup ----")
if(!(mleap_max >= current_spark_env_version)) {
  cat("\nSpark", current_spark_env_version, " not supported by MLeap")  
  testthat_spark_env_version(mleap_max)
  cat("\n--- using Spark:", mleap_max)  
} else {
  cat("\nSpark:", current_spark_env_version)
}
cat("\n------------------------------\n")
cat("\n--- Creating Spark session ---\n")
testthat_spark_connection()
cat("------------------------------\n\n")

## Disconnects all at the end
withr::defer(spark_disconnect_all(), teardown_env())



