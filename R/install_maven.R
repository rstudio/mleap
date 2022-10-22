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
  if (maven_found(path = dir)) {
    message("Maven already installed.")
    return(invisible(NULL))
  }

  def_version <- get_session_defaults("installation", "maven", "version")
  version <- version %||% def_version

  maven_dir <- dir %||% install_dir("maven")
  if (!dir.exists(maven_dir)) {
    dir.create(maven_dir)
  }

  maven_path <- file.path(
    maven_dir,
    sprintf("apache-maven-%s-bin.tar.gz", version)
  )

  maven_link <- get_maven_download_link(version = version)
  
  download.file(maven_link, maven_path)

  checksum_url <- paste0(maven_link, ".sha512")

  if (!identical(
    digest(file = normalizePath(maven_path), algo = "sha512"),
    readChar(checksum_url, nchars = 128)
  )
  ) {
    file_delete(maven_path)
    stop("Maven installation failed. Unable to verify checksum.")
  }

  status <- untar(maven_path, exdir = maven_dir)
  
  if (!identical(status, 0L)) stop("Maven installation failed.", call. = FALSE)

  file_delete(maven_path)
  set_session_defaults(maven_home = maven_dir) 
  message("Maven installation succeeded.")
  invisible(NULL)
}


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
    } else {
      normalizePath(path_with_end, mustWork = FALSE)
    }
  }

  getOption("maven.install.dir", resolve_envpath(dirs[[.Platform$OS.type]]))
}

resolve_maven_path <- function() {
  maven_dir <- get_session_defaults("runtime", "maven", "home")
  maven_files <- dir_ls(maven_dir, recurse = TRUE, type = "file")
  
  if (identical(.Platform$OS.type, "windows")) {
    mvn_cmd <- "mvn.cmd"
    } else {
    mvn_cmd <- "mvn"
    }
  
  maven_path <- maven_files[path_file(maven_files) == mvn_cmd]
  
  if (!length(maven_path)) {
    stop("Can't find Maven. Specify options(maven.home = ...) or run install_maven().",
      call. = FALSE
    )
  }

  maven_path
}

maven_found <- function(path = NULL) {
  if(!is.null(path)) {
    dir_exists(path)
  } else {
    if (length(safely(resolve_maven_path)()$result)) TRUE else FALSE  
  }
}
