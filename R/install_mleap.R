#' Install MLeap runtime
#'
#' @param dir Directory to save the jars. Defaults to what is processed by `mleap_session()`.
#' @param mleap_version Version of MLeap to install.  Defaults to what is processed by `mleap_session()`.
#' @param scala_version Version of Scala to match to the MLeap version. It defaults to 'auto'.
#' If 'auto', the installation process will determine the most recent version of Scala
#' to use available for the requested MLeap version. Other possible values are '2.10', 
#' '2.11', and '2.12'.
#' @param use_temp_cache Whether to use a temporary Maven cache directory for downloading.
#'   Setting this to \code{TRUE} prevents Maven from creating a persistent \code{.m2/} directory.
#'   Defaults to \code{TRUE}.
#' @param force Force the installation. Defaults to \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' install_mleap()
#' }
#' @export
install_mleap <- function(mleap_version = mleap_defaults("version"), 
                          force = FALSE, 
                          scala_version = "auto",                          
                          dir = mleap_defaults("base_folder"), 
                          use_temp_cache = TRUE
                          ) {
  
  if(scala_version == "auto") scala_version <- NULL

  pkgs <- versions_get_packages(mleap_version, scala_version)
  nrow_pkgs <- nrow(pkgs)
  
  if(!nrow_pkgs) {
    stop(
      "No Maven packages found for MLeap version: ", mleap_version, 
      " and Scala version: ", scala_version
    )
  } 
  
  if(is.null(scala_version)) {
    scala_version <- pkgs$scala[[1]] 
    cat("+-- Using Scala version:", scala_version, "\n")
  }
  
  if(force) {
    cat("+-- force=TRUE - Will replace previous installations\n")
  } else {
    cat("+-- force=FALSE - Will skip anything already installed\n")
  }
  
  cat("+-- Number of identified dependencies:", nrow_pkgs, "\n")

  if(!dir_exists(dir)) dir_create(dir)
  
   
  dir_scala <- paste0("scala-",scala_version)
  dir_mleap <- paste0("mleap-", mleap_version)
  
  target_dir <- path(dir, paste(dir_mleap, dir_scala, sep = "_"))
  
  if(!dir_exists(target_dir)) dir_create(target_dir)
  
  pkgs$number <- seq_len(nrow_pkgs)
  
  pkgs |> 
    transpose() |> 
    walk(~ {
      msg <- paste0("+---- [", .x$number, "/", nrow_pkgs, "]")
      cat(msg, .x$name, "\n")
      name_dir <- path(target_dir, .x$name)
      maven_download_jars(
        dependency = .x$package,
        use_temp_cache = use_temp_cache,
        force = force,
        install_dir = name_dir
        )
    })
  
}

#' Find existing MLeap installations
#'
#' @return A data frame of MLeap Runtime installation versions and
#'   their locations.
#'
#' @export
mleap_installed_versions <- function() {
  if(get_session_defaults("runtime", "mleap", "found")) {
    base_folder <- get_session_defaults("runtime", "mleap", "home")
    dirs <- dir_ls(base_folder, type = "directory")
    dir_names <- path_file(dirs)
    mleap_folders <- dir_names[grepl("mleap-", dir_names)]
    found <- FALSE
    if(length(mleap_folders)) {
      scala_folders <- dir_names[grepl("scala-", dir_names)]  
      ret <- NULL
      if(length(scala_folders)) {
        found <- TRUE
        for(i in seq_along(scala_folders)) {
          curr_folder <- scala_folders[i]
          split1 <- strsplit(curr_folder, "_")[[1]]
          split2 <- strsplit(split1, "-")
          mleap <- split2[[1]][[2]]
          scala <- split2[[2]][[2]]
          ct <- tibble(
            mleap = mleap,
            scala = scala,
            folder = path(base_folder, curr_folder)
          )
          ret <- rbind(ret, ct)
        }
        ret
        
      }
    }  
  } else {
   tibble(mleap = character(0), scala = character(0), folder = character(0)) 
  }
}

resolve_mleap_path <- function(version = NULL) {
  if(is.null(version)) {
    get_session_defaults("runtime", "mleap", "home")
  } else {
    res <- mleap_installed_versions()
    rv <- res[res$mleap == version, ]
    rv[1, ]$folder
  }
  
}

mleap_found <- function(version = NULL, path = NULL) {
  get_session_defaults("runtime", "mleap", "found")
}

mleap_verify <- function() {
  if(!mleap_found()) {
    msg <- paste("No MLeap installation found.",
                   "Use install_mleap() to install, or",
                   "point mleap to an installation folder",
                   "using options('mleap_home' = '{location}')",
                   sep = "\n"
                   )
    stop(msg)
  }
}

maven_download_jars <- function(dependency, 
                                mvn = resolve_maven_path(), 
                                install_dir = "test_dir", 
                                use_temp_cache = TRUE, 
                                temp_dir = "temp_dir",
                                force = FALSE
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
  if(!dir_exists(pom_dir)) dir_create(pom_dir)
  
  pom_path <- path(
    pom_dir,
    paste0(strsplit(dependency, ":")[[1]][2:3], collapse = "-"), 
    ext = "pom"
  )
  
  run_pom <- FALSE
  run_install <- FALSE
  
  if(!dir_exists(install_dir)) {
    dir_create(install_dir)
    run_install <- TRUE
  }  
  if(force) run_install <- TRUE
  
  if(run_install) {
    if(!file_exists(pom_path)) run_pom <- TRUE
    if(force) run_pom <- TRUE
    if(!run_pom) message("+------ Download files exist, skipping")
  } else {
    message("+------ Installation exists, skipping")
  }
  
  if(run_pom) {
    cat("+------ Downloading dependencies\n")
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
      dependency = dependency,
      maven_local_repo = maven_local_repo, 
      error_message = "Installation failed. Can't download pom for "
      ) 
    } 

  if(run_install) {
    cat("+------ Installing dependencies\n")
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
      dependency = dependency,
      maven_local_repo = maven_local_repo, 
      error_message = "Installation failed. Can't download dependencies for "
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
      dependency = dependency,
      maven_local_repo = maven_local_repo, 
      error_message = "Installation failed. Can't copy dependencies for "
    )
  } 
  
}

execute_command <- function(args, dependency, maven_local_repo, error_message) {
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
