.mleap_globals <- new.env(parent = emptyenv())

.mleap_globals$session_defaults <- NULL

set_mleap_session_defaults <- function(apache_mirror_url = NULL,
                                       maven_repo = NULL,
                                       apache_mirror_selection = NULL,
                                       maven_download_path = NULL,
                                       mleap_default_version = NULL, 
                                       maven_default_version = NULL,
                                       versions = NULL
                                       ) {
  
  json_config <- system.file(file.path("extdata", "config.json"), package = "mleap")
  cj <- jsonlite::fromJSON(json_config)
  
  msd <- .mleap_globals$session_defaults 
  if(is.null(msd)) msd <- cj
  
  msd$setup$apache_mirror_selection <- apache_mirror_selection %||% msd$setup$apache_mirror_selection
  
  msd$setup$apache_mirror_url<- apache_mirror_url %||% 
    msd$setup$apache_mirror_url %||% 
    get_preferred_apache_mirror(msd$setup$apache_mirror_selection)
  
  msd$setup$maven_download_path <- maven_download_path %||% msd$setup$maven_download_path
  
  msd$setup$mleap_default_version <- mleap_default_version %||% msd$setup$mleap_default_version
  
  msd$setup$maven_default_version <- maven_default_version %||% msd$setup$maven_default_version
  
  msd$setup$maven_repo <- maven_repo %||% msd$setup$maven_repo
  
  msd$versions <- versions %||% msd$versions  
  
  .mleap_globals$session_defaults  <- msd
}

get_mleap_session_defaults <- function() {
  msd <- .mleap_globals$session_defaults 
  if(is.null(msd)) {
    set_mleap_session_defaults()
    msd <- .mleap_globals$session_defaults   
  } 
  msd
}
