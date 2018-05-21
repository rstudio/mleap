library(testthat)
library(mleap)

temp <- tempdir()
maven_dir <- file.path(temp, "maven")
mleap_dir <- file.path(temp, "mleap")

testthat_spark_connection <- function() {
  version <- Sys.getenv("SPARK_VERSION", unset = "2.3.0")

  spark_installed <- sparklyr::spark_installed_versions()
  if (nrow(spark_installed[spark_installed$spark == version, ]) == 0) {
    options(sparkinstall.verbose = TRUE)
    spark_install(version)
  }
  
  expect_gt(nrow(sparklyr::spark_installed_versions()), 0)
  
  # generate connection if none yet exists
  connected <- FALSE
  if (exists(".testthat_spark_connection", envir = .GlobalEnv)) {
    sc <- get(".testthat_spark_connection", envir = .GlobalEnv)
    connected <- sparklyr::connection_is_open(sc)
  }
  
  if (!connected) {
    config <- sparklyr::spark_config()
    
    options(sparklyr.sanitize.column.names.verbose = TRUE)
    options(sparklyr.verbose = TRUE)
    options(sparklyr.na.omit.verbose = TRUE)
    options(sparklyr.na.action.verbose = TRUE)
    
    
    sc <- tryCatch({
      sparklyr::spark_connect(master = "local", version = version, config = config)
    }, error = function(e) {
      sparklyr::spark_connect(master = "local", version = version, config = config)
    })
    assign(".testthat_spark_connection", sc, envir = .GlobalEnv)
  }
  
  # retrieve spark connection
  get(".testthat_spark_connection", envir = .GlobalEnv)
}

output_file <- function(filename) file.path("output", filename)
