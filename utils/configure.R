library(purrr)
library(fs)
library(sparklyr)
devtools::load_all()

base_folder <- "internal/mleap-spark"

versions <- list(
  list(scala = "2.12", mleap = "0.20.0"),
  list(scala = "2.11", mleap = "0.16.0")
)

for(i in seq_along(versions)) {
  version_folder <- "internal/mleap-spark/2.12"
  mleap_paackage <- "ml.combust.mleap:mleap-spark_2.12:0.20.0"
  download_jars(resolve_maven_path(),
                mleap_package,
                normalizePath(version_folder),
                use_temp_cache = TRUE
  )
}





sparklyr_spec <- spark_default_compilation_spec()

mleap_files <- base_folder %>% 
  dir_ls(recurse = TRUE) %>% 
  path_abs()

vers <- function(x) {
  c(x, 
    list(path = find_scalac(x$scala), 
         jars = path_abs(dir_ls(path(base_folder, x$scala)))
         )
  )
  }

scala_paths <- map(versions, vers)

mleap_spec <- sparklyr_spec %>% 
  keep(~ .x$spark_version >= "2.0.0")  %>% 
  map(~{
    x <- .x
    mt <- scala_paths[map_lgl(scala_paths, ~ x$scalac_path == .x$path)]
    x$jar_dep <- mt[[1]]$jars
    x$embedded_srcs <- NULL
    x
  }) 

compile_package_jars(spec = mleap_spec)




