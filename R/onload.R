.globals <- new.env(parent = emptyenv())
.globals$init_local_mleap <- TRUE


load_mleap_jars <- function(version = NULL) {
  if(.globals$init_local_mleap) {
    if(mleap_found()) {
      .jinit()
      if (!any(grepl("mleap", .jclassPath()))) {
        mleap_path <- resolve_mleap_path(version)
        jar_files <- dir_ls(mleap_path, type = "file", glob = "*.jar", recurse = TRUE)
        .jpackage("mleap", morePaths = jar_files)    
      }
      .globals$init_local_mleap <- FALSE 
    }
  }
}
