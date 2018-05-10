install_dir <- function(dir_name) {
  dirs <- list(
    unix = paste0("~/", dir_name),
    windows = paste0("%LOCALAPPDATA%/", dir_name)
  )
  
  resolve_envpath <- function(path_with_end) {
    if (.Platform$OS.type == "windows") {
      parts <- strsplit(path_with_end, "/")[[1]]
      first <- gsub("%", "", parts[[1]])
      if (nchar(Sys.getenv(first)) > 0) parts[[1]] <- Sys.getenv(first)
      do.call("file.path", as.list(parts))
    }
    else {
      normalizePath(path_with_end, mustWork = FALSE)
    }
  }
  
  getOption("maven.install.dir", resolve_envpath(dirs[[.Platform$OS.type]]))
}

resolve_maven_path <- function() {
  maven_dir <- getOption("maven.home", .globals$maven_dir) %||% install_dir("maven")
  maven_path <- list.files(maven_dir, full.names = TRUE, recursive = TRUE) %>%
    grep("/bin/mvn$", ., value = TRUE) %>%
    utils::head(1)
  if (!length(maven_path))
    stop("Can't find Maven. Specify options(maven.home = ...) or run install_maven().",
         call. = FALSE)
  
  if (identical(.Platform$OS.type, "windows"))
    maven_path <- paste0(maven_path, ".cmd")
  
  maven_path
}

maven_found <- function() {
  if (length(purrr::safely(resolve_maven_path)()$result)) TRUE else FALSE
}

#' Install Maven
#' 
#' This function installs Apache Maven.
#' 
#' @param dir (Optional) Directory to install maven in.
#'   Defaults to \code{maven/} under user's home directory.
#' @param version Version of Maven to install, defaults to the latest version tested with this package.
#' 
#' @examples 
#' \dontrun{
#' install_maven()
#' }
#' 
#' @export
install_maven <- function(dir = NULL, version = NULL) {
  
  if (maven_found()) {
    message("Maven already installed.")
    return(invisible(NULL))
  }
  
  version <- version %||% .globals$default_maven_version
  
  maven_dir <- dir %||% install_dir("maven")
  if (!dir.exists(maven_dir))
    dir.create(maven_dir)
  
  maven_path <- file.path(
    maven_dir, 
    sprintf("apache-maven-%s-bin.tar.gz", version)
  )
  
  utils::download.file(
    get_maven_download_link(version = version),
    maven_path
  )
  
  checksum_url <- paste0("https://www.apache.org/dist/maven/maven-3/",
                         version,
                         "/binaries/apache-maven-",
                         version,
                         "-bin.tar.gz.sha256")
  
  if (!identical(digest::digest(file = normalizePath(maven_path),
                                algo = "sha256"),
                 readChar(checksum_url, nchars = 64)
  )) {
    fs::file_delete(maven_path)
    stop("Maven installation failed. Unable to verify checksum.")
  }
  
  status <- utils::untar(maven_path, compressed = "gzip",
                         exdir = maven_dir)
  if (!identical(status, 0L)) stop("Maven installation failed.", call. = FALSE)
  fs::file_delete(maven_path)
  
  .globals$maven_dir <- maven_dir
  message("Maven installation succeeded.")
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
    purrr::map_chr(~ gsub("mleap-", "", basename(.x)))
  
  data.frame(mleap = versions, dir = dirs,
             stringsAsFactors = FALSE) %>%
    unique()
}

resolve_mleap_path <- function(version = NULL) {
  if (length(getOption("mleap.home")) && is.null(version))
    return(getOption("mleap.home"))
  
  installed_versions <- mleap_installed_versions()
  if (nrow(installed_versions) == 0)
    stop("Can't find MLeap Runtime jars. Specify options(mleap.home = ...) or run install_mleap().")
  
  version <- version %||% (installed_versions$mleap %>%
                             purrr::map(~ numeric_version(.x)) %>%
                             purrr::reduce(~ (if (.x > .y) .x else .y)) %>%
                             as.character())
  version_index <- which(version == installed_versions$mleap)[[1]]
  if (!length(version_index))
    stop("MLeap version ", version, " not found.")
  
  mleap_dir <- installed_versions$dir[[version_index]]
  
  runtime_jars <- list.files(mleap_dir, full.names = TRUE, recursive = TRUE) %>%
    grep("mleap-runtime", ., value = TRUE)
  
  if (!length(runtime_jars))
    stop("Can't find MLeap Runtime jars. Specify options(mleap.home = ...) or run install_mleap().")
  
  mleap_dir
}

mleap_found <- function(version = NULL) {
  if (length(purrr::safely(resolve_mleap_path)(version)$result)) TRUE else FALSE
}

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
  
  if (mleap_found(version)) {
    message("MLeap Runtime version ", version, " already installed.")
    return(invisible(NULL))
  }
  
  mvn <- resolve_maven_path()
  if (!length(mvn)) stop("MLeap installation failed. Maven must be installed.")
  
  mleap_dir <- if (!is.null(dir)) {
    normalizePath(
      file.path(dir, paste0("mleap-", version)), 
      mustWork = FALSE
    )
  } else {
    install_dir(paste0("mleap/mleap-", version))
  }
  
  if (!fs::dir_exists(mleap_dir))
    fs::dir_create(mleap_dir, recursive = TRUE)
  
  message("Downloading MLeap Runtime ", version, "...")
  
  tryCatch(
    download_jars(mvn, paste0("ml.combust.mleap:mleap-runtime_2.11:", version), mleap_dir,
                  use_temp_cache = use_temp_cache),
    error = function(e) {fs::dir_delete(mleap_dir); stop(e)}
  )
  
  # download_jars(mvn, paste0("ml.combust.mleap:mleap-spark_2.11:", version), mleap_dir)
  .globals$mleap_dir <- dirname(mleap_dir)
  
  load_mleap_jars(version)
  
  message("MLeap Runtime version ", version, " installation succeeded.")
  invisible(NULL)
}

download_jars <- function(mvn, dependency, install_dir, use_temp_cache) {
  temp_dir <- tempdir()
  
  maven_local_repo <- if (use_temp_cache) {
    temp_repo <- paste0(temp_dir, "/local_repo")
    fs::dir_create(temp_repo)
    temp_repo
  }
  
  artifact_version <- dependency %>%
    strsplit(":") %>%
    unlist() %>%
    `[`(2:3)
  
  repo <- getOption("maven.repo", .globals$default_maven_repo)
  
  args_get_pom <- list(
    mvn, 
    c("dependency:get", 
      paste0("-Dartifact=", dependency, ":pom"), 
      paste0("-Ddest=", temp_dir),
      paste0("-DremoteRepositories=", repo)
    )
  )
  
  result_get_pom <- execute_command(args_get_pom, maven_local_repo)
  
  if (!command_success(result_get_pom))
    stop(paste0("Installation failed. Can't download pom for ", dependency),
         "\n",
         paste0(result_get_pom, collapse = "\n"),
         call. = FALSE
    )
  
  pom_path <- file.path(
    temp_dir, paste0(artifact_version[[1]], "-", artifact_version[[2]], ".pom")
  )
  # package_java_dir <-  file.path(package_path, "java/")
  
  args_get_artifact <- list(
    mvn,
    c("dependency:get",
      paste0("-Dartifact=", dependency),
      paste0(" -Ddest=", install_dir),
      paste0("-DremoteRepositories=", repo)
    )
  )
  result_get_artifact <- execute_command(args_get_artifact, maven_local_repo)
  
  if (!command_success(result_get_artifact))
    stop(paste0("Installation failed. Can't download dependencies for ", dependency),
         "\n",
         paste0(result_get_artifact, collapse = "\n"),
         call. = FALSE
    )
  
  args_get_deps <- list(
    mvn,
    c("dependency:copy-dependencies",
      "-f", 
      pom_path,
      paste0("-DoutputDirectory=", install_dir)
    )
  )
  result_get_deps <- execute_command(args_get_deps, maven_local_repo)
  
  if (!command_success(result_get_deps))
    stop(paste0("Installation failed. Can't copy dependencies for ", dependency),
         call. = FALSE
    )
}

get_preferred_apache_mirror <- function() {
  mirrors_info <- jsonlite::fromJSON("https://apache.org/dyn/closer.cgi?as_json=1")
  mirrors_info$preferred
}

get_maven_download_link <- function(version) {
  paste0(
    get_preferred_apache_mirror(),
    sprintf("maven/maven-3/%s/binaries/apache-maven-%s-bin.tar.gz", version, version)
  )
}

execute_command <- function(args, maven_local_repo) {
  
  if (!is.null(maven_local_repo)) {
    args[[2]] <- c(args[[2]],
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
  result
}

command_success <- function(result) {
  identical(attr(result, "status"), 0L)
}

load_mleap_jars <- function(version = NULL) {
  rJava::.jpackage("mleap",
                   morePaths = list.files(resolve_mleap_path(version),
                                          full.names = TRUE))
}
