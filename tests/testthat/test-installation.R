skip_install_tests()

test_that("install_maven() works", {
  if(dir_exists(maven_dir)) dir_delete(maven_dir)
  expect_message(
    install_maven(dir = maven_dir),
    "Maven installation succeeded\\."
  )
})

test_that("install_maven() detects existing installation", {
  expect_message(
    install_maven(dir = maven_dir),
    "Maven already installed\\."
  )
})

test_that("install_mleap() works", {
  if(dir_exists(mleap_dir)) dir_delete(mleap_dir)
  expect_message(
    install_mleap(dir = mleap_dir, version = testthat_context$mleap_version)
  )
})

test_that("install_mleap() detects existing installation", {
  expect_message(
    install_mleap(dir = mleap_dir, version = testthat_context$mleap_version)
  )
})
