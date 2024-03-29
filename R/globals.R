.globals <- new.env(parent = emptyenv())

json_config <- system.file(file.path("extdata", "config.json"), package = "mleap")
config <- jsonlite::fromJSON(json_config)

.globals$default_maven_version <- config$setup$maven_version
.globals$default_mleap_version <- config$setup$mleap_version
.globals$default_maven_repo <- config$setup$maven_repo
.globals$maven_dir <- NULL
.globals$mleap_dir <- NULL

.globals$init_local_mleap <- TRUE

globalVariables(".")
