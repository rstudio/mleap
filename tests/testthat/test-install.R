context("installation")

temp <- tempdir()
maven_dir <- file.path(temp, "maven")
mleap_dir <- file.path(temp, "mleap/mleap-0.9.4")

test_that("install_maven() works", {
  expect_message(install_maven(dir = maven_dir),
                 "Maven installation succeeded\\."
  )
  options(maven.home = maven_dir)
})

test_that("install_mleap() works", {
  expect_message(install_mleap(dir = mleap_dir),
                 "MLeap installation succeeded\\."
  )
  options(mleap.home = mleap_dir)
})