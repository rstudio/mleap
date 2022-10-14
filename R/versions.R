versions_default <- function() {
  readRDS(system.file(package = "mleap", "extdata/mleap_defaults.rds")) 
}

versions_packages <- function() {
  readRDS(system.file(package = "mleap", "extdata/mleap_packages.rds")) 
}

versions_available <- function() {
  readRDS(system.file(package = "mleap", "extdata/mleap_versions.rds")) 
}

versions_get_scala <- function(mleap) {
  x <- versions_default()
  ret <- x[x$mleap == mleap, ]
  ret$scala
}

versions_get_packages <- function(mleap, scala = NULL) {
  if(is.null(scala)) {
    scala <- versions_get_scala(mleap)
  }
  x <- versions_packages()
  ret <- x[x$mleap == mleap, ]
  ret <- ret[ret$scala == scala, ]
  ret$package
}