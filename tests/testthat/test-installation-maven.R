context("installation - maven")

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