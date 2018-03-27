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
  maven_dir <- getOption("maven.home", install_dir("maven"))
  maven_path <- list.files(maven_dir, full.names = TRUE, recursive = TRUE) %>%
    grep("/bin/mvn$", ., value = TRUE) %>%
    utils::head(1)
  if (is.null(maven_path))
    stop("Can't find Maven. Specify options(maven.home = ...) or run install_maven().",
         call. = FALSE)
  
  maven_path
}

#' Install Maven
#' 
#' This function installs Apache Maven.
#' 
#' @param dir (Optional) Directory to install maven in.
#'   Defaults to \code{maven/} under user's home directory.
#' 
#' @importFrom purrr %||%
#' @export
install_maven <- function(dir = NULL) {
  maven_dir <- dir %||% install_dir("maven")
  if (!dir.exists(maven_dir))
    dir.create(maven_dir)
  
  maven_path <- file.path(maven_dir, "apache-maven-3.5.2-bin.tar.gz")
  
  utils::download.file("http://apache.mirrors.lucidnetworks.net/maven/maven-3/3.5.2/binaries/apache-maven-3.5.2-bin.tar.gz", maven_path)
  
  status <- utils::untar(maven_path, compressed = "gzip",
                         exdir = maven_dir)
  if (!identical(status, 0L)) stop("Maven installation failed.", call. = FALSE)
  file.remove(maven_path)
  message("Maven installation succeeded.")
  invisible(NULL)
}

resolve_mleap_path <- function() {
  mleap_dir <- getOption("mleap.home", install_dir("mleap/mleap-0.9.4"))
  mleap_dir
}

#' Install MLeap runtime
#' 
#' @param dir (Optional) Directory to save the jars
#' @param restart_session Whether to restart R session after installation
#' @export
install_mleap <- function(dir = NULL, restart_session = TRUE) {
  mleap_dir <- dir %||% install_dir("mleap/mleap-0.9.4")
  if (!fs::dir_exists(mleap_dir))
    fs::dir_create(mleap_dir, recursive = TRUE)
  
  mvn <- resolve_maven_path()
  
  download_jars(mvn, "ml.combust.mleap:mleap-runtime_2.11:0.9.4", mleap_dir)
  download_jars(mvn, "ml.combust.mleap:mleap-spark_2.10:0.9.4", mleap_dir)
  
  if (restart_session && rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()
  
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