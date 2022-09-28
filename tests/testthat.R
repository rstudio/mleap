
## For Coverage us: Sys.setenv("CODE_COVERAGE" = "true")

if(identical(Sys.getenv("CODE_COVERAGE"), "true")) {
  library(testthat)
  library(mleap)
  library(sparklyr)
  library(fs)
  test_check("mleap")
}
