.mleap_globals <- new.env(parent = emptyenv())

.mleap_globals$session_defaults <- NULL

mleap_set_session_defaults <- function(apache_mirror_url = NULL,
                                       maven_repo = NULL,
                                       apache_mirror_selection = NULL,
                                       maven_download_path = NULL,
                                       mleap_default_version = NULL, 
                                       mleap_default_folder = NULL, 
                                       maven_default_version = NULL,
                                       maven_default_folder = NULL,
                                       versions = NULL
                                       ) {
  
  json_config <- system.file(file.path("extdata", "config.json"), package = "mleap")
  cj <- jsonlite::fromJSON(json_config)
  
  msd <- .mleap_globals$session_defaults 
  if(is.null(msd)) msd <- cj
  
  
  mv <- msd$installation$maven
  
  ams <- apache_mirror_selection %||% mv$apache_mirror_selection
  
  installation_maven <- tibble(
    version = maven_default_version %||% mv$version,
    apache_mirror_selection = ams,
    apache_mirror_url = apache_mirror_url %||% mv$apache_mirror_url, get_apache_mirror(ams),
    download_path = maven_download_path %||% mv$download_path,
    base_folder = maven_default_folder %||%  mv$base_folder %||% install_dir("maven")
  )
  
  ml <- msd$installation$mleap
  
  installation_mleap <- tibble(
    version = mleap_default_version %||% ml$version,
    maven_repo = maven_repo %||% ml$maven_repo, 
    base_folder = mleap_default_folder %||% ml$base_folder %||% install_dir("mleap")
  )

  vers <- versions %||% msd$versions  
  
  all_versions <- finalize_versions(vers)
  
  ret <- list(
    installation = list(
      mleap = installation_mleap,
      maven = installation_maven
    ),
    versions = all_versions
  )
  
  .mleap_globals$session_defaults  <- ret
  
  invisible(NULL)
}

mleap_get_session_defaults <- function(...) {
  vars <- enexprs(...)
  msd <- .mleap_globals$session_defaults 
  if(is.null(msd)) {
    mleap_set_session_defaults()
    msd <- .mleap_globals$session_defaults   
  } 
  for(i in seq_along(vars)) {
    msd <- msd[[eval(vars[[i]])]]
  }
  msd
}

get_apache_mirror <- function(apache_portal = NULL) {
  return(NULL)
  mirrors_info <- fromJSON(apache_portal)
  mirrors_info$preferred
}

get_maven_download_link <- function(version) {
  sdf <- mleap_get_session_defaults()
  mir <- sdf$installation$maven$apache_mirror_url
  dwp <- gsub("\\$version", version, sdf$installation$maven$download_path)
  paste0(mir, dwp)
}

finalize_versions <- function(versions) {
  versions %>% 
    transpose() %>% 
    map(
      ~ {
        x <- .x
        x$maven_spark <- sprintf("ml.combust.mleap:mleap-spark_%s:%s", x$scala, x$mleap)
        x$maven_mleap <- sprintf("ml.combust.mleap:mleap-runtime_%s:%s", x$scala, x$mleap)
        x$spark_major <- substr(x$spark, 1, 3)
        x$jar_name <- sprintf("mleap-%s-%s.jar", x$spark_major, x$scala)
        x
      }
    ) %>% 
    map(~ as_tibble(.x)) %>%
    reduce(function(x, y) rbind(x, y))    
}
