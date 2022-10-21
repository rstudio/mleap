.mleap_globals <- new.env(parent = emptyenv())

.mleap_globals$session_defaults <- NULL

set_session_defaults <- function(mleap_home = NULL, mleap_version = NULL,
                                 maven_home = NULL, apache_mirror_url = NULL,
                                 maven_repo = NULL, apache_mirror_selection = NULL,
                                 maven_download_path = NULL, mleap_default_version = NULL, 
                                 mleap_default_folder = NULL, maven_default_version = NULL,
                                 maven_default_folder = NULL, versions = NULL
                                 ) {
  
  json_config <- system.file(file.path("extdata", "config.json"), package = "mleap")
  cj <- jsonlite::fromJSON(json_config)
  
  msd <- .mleap_globals$session_defaults 
  if(is.null(msd)) msd <- cj
  
  
  mv <- msd$installation$maven
  
  maven_install <- getOption("maven.install.dir") %||% install_dir("maven")
  
  installation_maven <- tibble(
    version = maven_default_version %||% mv$version,
    repo = maven_repo %||% getOption("maven.repo") %||% mv$repo, 
    apache_mirror_selection = apache_mirror_selection %||% mv$apache_mirror_selection %||% "",
    apache_mirror_url = apache_mirror_url %||% mv$apache_mirror_url %||% "",
    download_path = maven_download_path %||% mv$download_path,
    base_folder = maven_default_folder %||%  mv$base_folder %||% maven_install
  )
  
  ml <- msd$installation$mleap
  
  installation_mleap <- tibble(
    version = mleap_default_version %||% ml$version,
    base_folder = mleap_default_folder %||% ml$base_folder %||% install_dir("mleap_jars")
  )
  
  maven_home <- maven_home %||% getOption("maven.home") 
    
  if(is.null(maven_home)) {
    bf <- installation_maven$base_folder
    vf <- get_version_folder(bf)
    if(!is.null(vf)) maven_home <- path(bf, vf)
  }
  
  mleap_home <- mleap_home %||% getOption("mleap.home") 
  mleap_found <- TRUE
  
  if(is.null(mleap_home)) {
    if(dir_exists(installation_mleap$base_folder)) {
      mleap_home <- installation_mleap$base_folder
    }
  }
  
  if(!is.null(mleap_home)) {
    mleap_contents <- dir_ls(mleap_home, recurse = TRUE, glob = "*.jar")
    if(!length(mleap_contents)) mleap_home <- NULL
  }
  
  if(is.null(mleap_home)) {
    mleap_home <- ""
    mleap_found <- FALSE
  } 
  
  mleap_version <- mleap_version %||% msd$runtime$mleap_version %||% ""
  
  ret <- list(
    runtime = list(
      mleap = tibble(
        home = mleap_home,
        found = mleap_found,
        version = mleap_version 
      ),
      maven = tibble(
        home = maven_home %||% "",
        found = !is.null(maven_home)
      )
    ),
    installation = list(
      mleap = installation_mleap,
      maven = installation_maven
    )
  )
  
  .mleap_globals$session_defaults  <- ret
  
  invisible(NULL)
}

get_session_defaults <- function(...) {
  vars <- enexprs(...)
  msd <- .mleap_globals$session_defaults 
  if(is.null(msd)) {
    set_session_defaults()
    msd <- .mleap_globals$session_defaults   
  } 
  for(i in seq_along(vars)) {
    msd <- msd[[eval(vars[[i]])]]
  }
  msd
}

#' Gets the processed defaults used during the MLeap session
#' @param var Variable to extract from defaults
#' @export
mleap_defaults <- function(var = NULL) {
  x <- get_session_defaults("installation", "mleap") |> 
    transpose() |> 
    flatten()
  if(!is.null(var)) x <- x[[var]]
  x
}

get_version_folder <- function(base_folder, version = NULL) {
  if(!dir_exists(base_folder)) dir_create(base_folder)
  folder_contents <- dir_info(base_folder)
  ret <- NULL
  if(nrow(folder_contents) > 0) {
    file_names <- path_file(folder_contents$path)
    if(is.null(version)) {
      sorted_names <- sort(file_names, decreasing = TRUE)
      ret <- sorted_names[1]
    } else {
      if(version %in% file_names) ret <- version
    }
  } 
  ret
}

get_apache_mirror <- function(apache_portal = NULL) {
  mirrors_info <- fromJSON(apache_portal)
  mirrors_info$preferred
}

get_maven_download_link <- function(version) {
  curr_mirror <- get_session_defaults("installation", "maven", "apache_mirror_url")
  if(curr_mirror == "") {
    get_mirror <- get_session_defaults("installation", "maven", "apache_mirror_selection")
    curr_mirror <- get_apache_mirror(get_mirror)
    set_session_defaults(apache_mirror_url = get_mirror)
  }
  download_path <- get_session_defaults("installation", "maven", "download_path")
  dwp <- gsub("\\$version", version, download_path)
  paste0(curr_mirror, dwp)
}

