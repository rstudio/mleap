cat("\n----- mleap test setup ----")
cat("\nSpark:", testthat_spark_env_version())
cat("\n------------------------------\n")

cat("\n--- Creating Spark session ---\n")
testthat_spark_connection()
cat("------------------------------\n\n")

## Disconnects all at the end
withr::defer(spark_disconnect_all(), teardown_env())


