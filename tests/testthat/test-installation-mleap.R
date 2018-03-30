context("installation - mleap")

test_that("install_mleap() works", {
  if (mleap:::mleap_found()) skip("MLeap already installed")
  expect_message(install_mleap(dir = mleap_dir),
                 "MLeap installation succeeded\\.")
})

test_that("install_mleap() detects existing installation", {
  expect_message(install_mleap(dir = mleap_dir),
                 "MLeap already installed\\.")
})