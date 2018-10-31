context("installation")

test_that("install_maven() works", {
  skip_on_cran()
  if (mleap:::maven_found()) skip("Maven already installed.")
  expect_message(install_maven(dir = maven_dir),
                 "Maven installation succeeded\\."
  )
})

test_that("install_maven() detects existing installation", {
  skip_on_cran()
  expect_message(install_maven(dir = maven_dir),
                 "Maven already installed\\.")
})

test_that("install_mleap() works", {
  skip_on_cran()
  if (mleap:::mleap_found(.globals$default_mleap_version)) skip("MLeap already installed")
  expect_message(
    install_mleap(dir = mleap_dir),
    paste0("MLeap Runtime version ",
           mleap:::.globals$default_mleap_version,
           " installation succeeded\\.")
  )
})

test_that("install_mleap() detects existing installation", {
  skip_on_cran()
  expect_message(
    install_mleap(dir = mleap_dir),
    paste0("MLeap Runtime version ", 
           mleap:::.globals$default_mleap_version,
           " already installed\\.")
    )
})