library(purrr)
library(fs)
library(sparklyr)
devtools::load_all()


versions <- list(
  list(scala = "2.12", mleap = "0.20.0"),
  list(scala = "2.11", mleap = "0.16.0")
)

base_folder <- "internal/mleap-spark"
url_maven <- "https://repo1.maven.org/maven2"

download_jars <- function(scala_version, mleap_version, base_folder, url_maven) {
  jar_path <- path(base_folder, scala_version)
  
  if(!dir_exists(jar_path)) dir_create(jar_path)
  
  
  url_combust <- "ml/combust/"
  
  url_mleap_pkgs <- c("avro", "base", "executor-testkit", "executor", 
                      "runtime", "serving", "spark-base", "spark-extension",
                      "spark-testkit", "spark", "core"
  )
  
  url_mleap_vers <- paste0("mleap-", url_mleap_pkgs, "_", scala_version)
  
  url_mleap <- paste0(
    url_maven, "/",
    url_combust, 
    "mleap", "/", 
    url_mleap_vers, "/", 
    mleap_version, "/",
    url_mleap_vers, "-",
    mleap_version, ".jar"
  )
  
  url_arm <- paste0(url_maven, "/com/jsuereth/scala-arm_", 
                    scala_version,"/2.0/scala-arm_", 
                    scala_version,"-2.0.jar"
  )
  
  url_bundle <- paste0(url_maven, "/ml/combust/bundle/bundle-ml_", 
                       scala_version,"/", mleap_version,"/bundle-ml_", 
                       scala_version, "-",  mleap_version,".jar"
  )
  
  url_all <- c(url_mleap, url_arm, url_bundle)
  
  walk(
    url_all,
    ~ {
      jar_target <- path(jar_path, path_file(.x))
      if(!file_exists(jar_target)) download.file(.x, jar_target)
    }
  )
}

walk(
  versions,
  ~ download_jars(.x$scala, .x$mleap, base_folder, url_maven)
)


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
  keep(~ .x$spark_version >= "2.4.0")  %>% 
  map(~{
    x <- .x
    mt <- scala_paths[map_lgl(scala_paths, ~ x$scalac_path == .x$path)]
    x$jar_dep <- mt[[1]]$jars
    x$embedded_srcs <- NULL
    x
  }) 

compile_package_jars(spec = mleap_spec[[1]])




