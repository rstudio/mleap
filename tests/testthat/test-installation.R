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
    install_mleap(dir = mleap_dir),
    paste0(
      "MLeap Runtime version ",
      mleap:::.globals$default_mleap_version,
      " installation succeeded\\."
    )
  )
})

test_that("install_mleap() detects existing installation", {
  expect_message(
    install_mleap(dir = mleap_dir),
    paste0(
      "MLeap Runtime version ",
      mleap:::.globals$default_mleap_version,
      " already installed\\."
    )
  )
})
