context("installation")

test_that("install_maven() works", {
  if (mleap:::maven_found()) skip("Maven already installed.")
  expect_message(install_maven(dir = maven_dir),
                 "Maven installation succeeded\\."
  )
})

test_that("install_maven() detects existing installation", {
  expect_message(install_maven(dir = maven_dir),
                 "Maven already installed\\.")
})

test_that("install_mleap() works", {
  if (mleap:::mleap_found()) skip("MLeap already installed")
  expect_message(install_mleap(dir = mleap_dir),
                 "MLeap installation succeeded\\.")
})

test_that("install_mleap() detects existing installation", {
  expect_message(install_mleap(dir = mleap_dir),
                 "MLeap already installed\\.")
})