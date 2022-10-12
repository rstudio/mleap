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
  
  msd$installation$maven$apache_mirror_selection <- coalesce_vars(
    apache_mirror_selection,
    msd$installation$maven$apache_mirror_selection
  ) 
  
  msd$installation$maven$apache_mirror_url<- coalesce_vars(
    apache_mirror_url,
    msd$installation$maven$apache_mirror_url,
    get_apache_mirror(msd$installation$maven$apache_mirror_selection)
  )  
  
  msd$installation$maven$download_path <- coalesce_vars(
    maven_download_path,
    msd$installation$maven$download_path
  ) 
  
  msd$installation$maven$version <- coalesce_vars(
    maven_default_version,
    msd$installation$maven$version
  )

  msd$installation$mleap$verion <- coalesce_vars(
    mleap_default_version,
    msd$installation$mleap$verion
  ) 
  
  msd$installation$mleap$maven_repo <- coalesce_vars(
    maven_repo,
    msd$installation$mleap$maven_repo
  )  
  
  msd$versions <- coalesce_vars(
    versions,
    msd$versions  
  ) %>% 
    finalize_versions()
  
  .mleap_globals$session_defaults  <- msd
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

coalesce_vars <- function(...) {
  vars <- enexprs(...)
  for(i in seq_along(vars)) {
    x <- vars[[i]]
    if(!is.null(x)) {
      cx <- class(x)
      if(cx %in% c("call", "name")) {
        x <- eval.parent(x, 1)
        if(!is.null(x)) return(x)
      } else {
        return(x)
      }
    }
  }
}

get_apache_mirror <- function(apache_portal = NULL) {
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
