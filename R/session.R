.mleap_globals <- new.env(parent = emptyenv())

.mleap_globals$session_defaults <- NULL

mleap_set_session_defaults <- function(apache_mirror_url = NULL,
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
  
  msd$installation$maven$apache_mirror_selection <- apache_mirror_selection %||% 
    msd$installation$maven$apache_mirror_selection
  
  msd$installation$maven$apache_mirror_url<- apache_mirror_url %||% 
    msd$installation$maven$apache_mirror_url %||% 
    get_preferred_apache_mirror(msd$installation$maven$apache_mirror_selection)
  
  msd$installation$maven$download_path <- maven_download_path %||% 
    msd$installation$maven$download_path
  
  msd$installation$maven$version <- maven_default_version %||% msd$installation$maven$version

  msd$installation$mleap_default_version <- mleap_default_version %||% 
    msd$installation$mleap$verion
  
  msd$installation$mleap$maven_repo <- maven_repo %||% msd$installation$mleap$maven_repo
  
  msd$versions <- versions %||% msd$versions  
  
  .mleap_globals$session_defaults  <- msd
}

mleap_get_session_defaults <- function(level1 = NULL, level2 = NULL, level3 = NULL) {
  msd <- .mleap_globals$session_defaults 
  if(is.null(msd)) {
    mleap_set_session_defaults()
    msd <- .mleap_globals$session_defaults   
  } 
  if(!is.null(level1)) msd <- msd[[level1]]
  if(!is.null(level2)) msd <- msd[[level2]]
  if(!is.null(level3)) msd <- msd[[level3]]
  msd
}


get_preferred_apache_mirror <- function(apache_portal = NULL) {
  mirrors_info <- fromJSON(apache_portal)
  mirrors_info$preferred
}

get_maven_download_link <- function(version) {
  sdf <- mleap_get_session_defaults()
  mir <- sdf$installation$maven$apache_mirror_url
  dwp <- gsub("\\$version", version, sdf$installation$maven$download_path)
  paste0(mir, dwp)
}
