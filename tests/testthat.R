
## For Coverage us: Sys.setenv("CODE_COVERAGE" = "true")

## Installation tests will be skipped unless "RUN_INSTALL_TESTS" 
## environment variable is set to TRUE: 
##      Sys.setenv("RUN_INSTALL_TESTS" = "true")

if(identical(Sys.getenv("CODE_COVERAGE"), "true")) {
  library(testthat)
  library(mleap)
  library(sparklyr)
  library(fs)
  test_check("mleap")
}
