.globals <- new.env(parent = emptyenv())
.globals$maven_dir <- install_dir("maven")
.globals$mleap_dir <- install_dir("mleap/mleap-0.9.4")

utils::globalVariables(".")