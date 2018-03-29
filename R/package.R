.globals <- new.env(parent = emptyenv())

versions <- jsonlite::fromJSON(system.file(file.path("extdata", "default_versions.json"), 
                               package = packageName()
                               )
                   )
.globals$default_maven_version <- versions$maven
.globals$default_mleap_version <- versions$mleap

.globals$maven_dir <- NULL
.globals$mleap_dir <- NULL

utils::globalVariables(".")