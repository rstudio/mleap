#' Install MLeap runtime
#'
#' @param dir (Optional) Directory to save the jars
#' @param version Version of MLeap to install, defaults to the latest version tested with this package.
#' @param use_temp_cache Whether to use a temporary Maven cache directory for downloading.
#'   Setting this to \code{TRUE} prevents Maven from creating a persistent \code{.m2/} directory.
#'   Defaults to \code{TRUE}.
#'
#' @examples
#' \dontrun{
#' install_mleap()
#' }
#' @export
install_mleap <- function(dir = NULL, version = NULL, use_temp_cache = TRUE) {
  version <- version %||% .globals$default_mleap_version

  if (mleap_found(version = version, path = dir)) {
    message("MLeap Runtime version ", version, " already installed.")
    return(invisible(NULL))
  }

  version_deps <- mleap_dep_versions_list(mleap_version = version)[[1]]

  mvn <- resolve_maven_path()

  if (!length(mvn)) stop("MLeap installation failed. Maven must be installed.")


  mleap_folder <- paste0("mleap-", version)

  if (!is.null(dir)) {
    mleap_dir <- normalizePath(
      file.path(dir, mleap_folder),
      mustWork = FALSE
    )
  } else {
    mleap_dir <- install_dir(path("mleap", mleap_folder))
  }

  if (!dir_exists(mleap_dir)) dir_create(mleap_dir, recurse = TRUE)

  message("Downloading MLeap Runtime ", version, "...")

  tryCatch(
    maven_download_jars(
      mvn,
      version_deps$maven_mleap,
      mleap_dir,
      use_temp_cache = use_temp_cache
    ),
    error = function(e) {
      dir_delete(mleap_dir)
      stop(e)
    }
  )

  .globals$mleap_dir <- dirname(mleap_dir)

  load_mleap_jars(version)

  message("MLeap Runtime version ", version, " installation succeeded.")

  invisible(NULL)
}

#' Find existing MLeap installations
#'
#' @return A data frame of MLeap Runtime installation versions and
#'   their locations.
#'
#' @export
mleap_installed_versions <- function() {
  mleap_dir <- .globals$mleap_dir %||% install_dir("mleap")
  dirs <- c(getOption("mleap.home"), list.files(mleap_dir, full.names = TRUE))
  versions <- dirs %>%
    map_chr(~ gsub("mleap-", "", basename(.x)))

  data.frame(
    mleap = versions, 
    dir = dirs,
    stringsAsFactors = FALSE
  ) %>%
    unique()
}

resolve_mleap_path <- function(version = NULL) {
  if (length(getOption("mleap.home")) && is.null(version)) {
    return(getOption("mleap.home"))
  }

  installed_versions <- mleap_installed_versions()
  if (nrow(installed_versions) == 0) {
    stop("Can't find MLeap Runtime jars. Specify options(mleap.home = ...) or run install_mleap().")
  }

  version <- version %||% (installed_versions$mleap %>%
    map(~ numeric_version(.x)) %>%
    reduce(~ (if (.x > .y) .x else .y)) %>%
    as.character())
  
  version_index <- which(version == installed_versions$mleap)[[1]]
  
  if (!length(version_index)) {
    stop("MLeap version ", version, " not found.")
  }

  mleap_dir <- installed_versions$dir[[version_index]]

  runtime_jars <- list.files(mleap_dir, full.names = TRUE, recursive = TRUE) %>%
    grep("mleap-runtime", ., value = TRUE)

  if (!length(runtime_jars)) {
    stop("Can't find MLeap Runtime jars. Specify options(mleap.home = ...) or run install_mleap().")
  }

  mleap_dir
}

mleap_found <- function(version = NULL, path = NULL) {
  if(!is.null(path)) {
    dir_exists(path)
  } else {
    if (length(safely(resolve_mleap_path)(version)$result)) TRUE else FALSE
  }
}

maven_download_jars <- function(dependency, 
                                mvn = resolve_maven_path(), 
                                install_dir = "test_dir", 
                                use_temp_cache = TRUE, 
                                temp_dir = "temp_dir"
                                ) {
  
  temp_dir <- path_abs(path(temp_dir, "mleap_maven"))
  install_dir <- path_abs(install_dir)

  maven_local_repo <- if (use_temp_cache) {
    temp_repo <- path(temp_dir, "temp_cache", dependency)
    dir_create(temp_repo)
    temp_repo
  }

  repo <- get_session_defaults("installation", "maven", "repo")

  pom_dir <- path(temp_dir, "pom_files")
  
    if(!dir_exists(pom_dir)) dir_create(pom_dir) {
    args_get_pom <- list(
      mvn,
      c(
        "dependency:get",
        paste0("-Dartifact=", dependency, ":pom"),
        paste0("-Ddest=", pom_dir),
        paste0("-DremoteRepositories=", repo)
      )
    )
  
    result_get_pom <- execute_command(
      args = args_get_pom, 
      maven_local_repo = maven_local_repo, 
      error_message = "Installation failed. Can't download pom for "
      ) 
    } else {
      message("POM folder already exists, skipping")
    }

  if(!dir_exists(install_dir)) dir_create(install_dir)
  
  args_get_artifact <- list(
    mvn,
    c(
      "dependency:get",
      paste0("-Dartifact=", dependency),
      paste0("-Ddest=", install_dir),
      paste0("-DremoteRepositories=", repo)
    )
  )
  
  execute_command(
    args = args_get_artifact, 
    maven_local_repo = maven_local_repo, 
    error_message = "Installation failed. Can't download dependencies for "
    )

  pom_path <- path(
    pom_dir,
    paste0(strsplit(dependency, ":")[[1]][2:3], collapse = "-"), 
    ext = "pom"
    )
  
  args_get_deps <- list(
    mvn,
    c(
      "dependency:copy-dependencies",
      "-f",
      pom_path,
      paste0("-DoutputDirectory=", install_dir)
    )
  )
  
  execute_command(
    args = args_get_deps,
    maven_local_repo = maven_local_repo, 
    error_message = "Installation failed. Can't copy dependencies for "
    )
}

execute_command <- function(args, maven_local_repo, error_message) {
  if (!is.null(maven_local_repo)) {
    args[[2]] <- c(
      args[[2]],
      paste0("-Dmaven.repo.local=", maven_local_repo)
    )
  }

  if (identical(.Platform$OS.type, "windows")) {
    result <- shell(paste0(unlist(args), collapse = " "),
      ignore.stdout = TRUE
    )
    if (identical(result, 0L)) attr(result, "status") <- 0L
  } else {
    result <- do.call(system2, c(args, stdout = TRUE, stderr = TRUE))
    if (is.null(attr(result, "status"))) attr(result, "status") <- 0L
  }
  
  if (!identical(attr(result, "status"), 0L)) {
    stop(paste0(error_message, dependency),
         "\n",
         paste0(result, collapse = "\n"),
         call. = FALSE
    )}
  invisible((NULL))
}

command_success <- function(result) {
  identical(attr(result, "status"), 0L)
}

load_mleap_jars <- function(version = NULL) {
    if(.globals$init_local_mleap) {
    .jinit()
    if (!any(grepl("mleap-runtime", .jclassPath()))) {
      mleap_path <- resolve_mleap_path(version)
      .jpackage(
        "mleap",
        morePaths = list.files(mleap_path, full.names = TRUE)
      )    
    }
    .globals$init_local_mleap <- FALSE 
  }
}
