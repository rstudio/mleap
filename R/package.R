.globals <- new.env(parent = emptyenv())

config <- jsonlite::fromJSON(system.file(file.path("extdata", "config.json"), 
                               package = packageName()
                               )
                   )
.globals$default_maven_version <- config$maven_version
.globals$default_mleap_version <- config$mleap_version
.globals$default_maven_repo <- config$maven_repo

.globals$maven_dir <- NULL
.globals$mleap_dir <- NULL

utils::globalVariables(".")