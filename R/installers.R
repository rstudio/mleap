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
  if (is.null(maven_path))
    stop("Can't find Maven. Specify options(maven.home = ...) or run install_maven().",
         call. = FALSE)
  
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
  
  status <- utils::untar(maven_path, compressed = "gzip",
                         exdir = maven_dir)
  if (!identical(status, 0L)) stop("Maven installation failed.", call. = FALSE)
  file.remove(maven_path)
  
  .globals$maven_dir <- maven_dir
  message("Maven installation succeeded.")
  invisible(NULL)
}

resolve_mleap_path <- function() {
  mleap_dir <- getOption("mleap.home", .globals$mleap_dir) %||% install_dir("mleap")
  runtime_jars <- list.files(mleap_dir, full.names = TRUE, recursive = TRUE) %>%
    grep("mleap-runtime", ., value = TRUE)
  
  if (!length(runtime_jars))
    stop("Can't find MLeap. Specify options(mleap.home = ...) or run install_mleap().")
  
  versions <- purrr::map(
    runtime_jars, 
    ~ .x %>% 
      strsplit("-") %>%
      unlist() %>%
      utils::tail(1) %>%
      gsub("\\.jar$", "", .) %>%
      numeric_version()
  )
  
  latest_version <- versions %>%
    purrr::reduce(~ (if (.x > .y) .x else .y)) 
  latest_index <- versions %>%
    purrr::map(~ .x == latest_version) %>%
    which.max()
  
  dirname(runtime_jars[[latest_index]])
}

mleap_found <- function() {
  if (length(purrr::safely(resolve_mleap_path)()$result)) TRUE else FALSE
}

#' Install MLeap runtime
#' 
#' @param dir (Optional) Directory to save the jars
#' @param version Version of MLeap to install, defaults to the latest version tested with this package.
#' @export
install_mleap <- function(dir = NULL, version = NULL) {
  
  if (mleap_found()) {
    message("MLeap already installed.")
    return(invisible(NULL))
  }
  
  version <- version %||% .globals$default_mleap_version
  mleap_dir <- dir %||% install_dir(paste0("mleap/mleap-", version))
  if (!fs::dir_exists(mleap_dir))
    fs::dir_create(mleap_dir, recursive = TRUE)
  
  mvn <- resolve_maven_path()
  
  download_jars(mvn, paste0("ml.combust.mleap:mleap-runtime_2.11:", version), mleap_dir)
  download_jars(mvn, paste0("ml.combust.mleap:mleap-spark_2.11:", version), mleap_dir)
  .globals$mleap_dir <- mleap_dir
  
  rJava::.jpackage("mleap",
                   morePaths = list.files(resolve_mleap_path(),
                                          full.names = TRUE))
  
  message("MLeap installation succeeded.")
  invisible(NULL)
}

download_jars <- function(mvn, dependency, install_dir) {
  temp_dir <- tempdir()
  
  artifact_version <- dependency %>%
    strsplit(":") %>%
    unlist() %>%
    `[`(2:3)
  
  run_get_pom <- system(
    paste0(mvn, " dependency:get -Dartifact=", dependency, ":pom -Ddest=", temp_dir),
    ignore.stdout = TRUE
  )
  
  if (!identical(run_get_pom, 0L))
    stop(paste0("Installation failed. Can't download pom for ", dependency),
         call. = FALSE
    )
  
  pom_path <- file.path(
    temp_dir, paste0(artifact_version[[1]], "-", artifact_version[[2]], ".pom")
  )
  # package_java_dir <-  file.path(package_path, "java/")
  run_get_artifact <- system(
    paste0(mvn, 
           " dependency:get -Dartifact=",
           dependency,
           " -Ddest=",
           install_dir),
    ignore.stdout = TRUE
  )
  
  if (!identical(run_get_artifact, 0L))
    stop(paste0("Installation failed. Can't download dependencies for ", dependency),
         call. = FALSE
    )
  
  run_get_deps <- system(
    paste0(mvn,
           " dependency:copy-dependencies -f ", pom_path,
           " -DoutputDirectory=",
           install_dir),
    ignore.stdout = TRUE
  )
  
  if (!identical(run_get_deps, 0L))
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