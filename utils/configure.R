library(purrr)
library(fs)
library(sparklyr)
devtools::load_all()


mleap_path <- path("internal/mleap-spark/")
if(!dir_exists(mleap_path)) {
  dir_create(mleap_path)
  download_jars(
    resolve_maven_path(),
    "ml.combust.mleap:mleap-spark_2.12:0.20.0",
    mleap_path,
    use_temp_cache = TRUE
  )
}


sparklyr_spec <- spark_default_compilation_spec()

mleap_files <- mleap_path %>% 
  dir_ls() %>% 
  path_abs()

mleap_spec <- sparklyr_spec %>% 
  map(~{
    x <- .x
    x$jar_dep <- mleap_files 
    x$embedded_srcs <- NULL
    x
  }) %>% 
  keep(~ .x$spark_version >= "2.0.0")  

compile_package_jars(spec = mleap_spec)


