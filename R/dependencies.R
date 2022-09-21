#' @import sparklyr
.onLoad <- function(libname, pkgname) { # nocov start
  sparklyr::register_extension(pkgname)
  jar_paths <- tryCatch(
    list.files(resolve_mleap_path(), full.names = TRUE),
    error = function(e) ""
  )
  # rJava::.jpackage(pkgname, lib.loc = libname, 
  #                  morePaths = jar_paths)
} # nocov end
