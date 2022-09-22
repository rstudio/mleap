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

scala_version <- "2.12"
mleap_version <- "0.20.0"

url_combust <- "https://repo1.maven.org/maven2/ml/combust/"

url_mleap_pkgs <- c("avro", "base", "executor-testkit", "executor", 
                    "runtime", "serving", "spark-base", "spark-extension",
                    "spark-testkit", "spark", "core"
)

url_mleap_vers <- paste0("mleap-", url_mleap_pkgs, "_", scala_version)

url_mleap <- paste0(
  url_combust, 
  "mleap", "/", 
  url_mleap_vers, "/", 
  mleap_version, "/",
  url_mleap_vers, "-",
  mleap_version, ".jar"
  )

map(
  url_mleap,
  ~ download.file(.x, path("internal/mleap-spark", path_file(.x)))
)

download.file(
  "https://repo1.maven.org/maven2/com/jsuereth/scala-arm_2.12/2.0/scala-arm_2.12-2.0.jar",
  "internal/mleap-spark/scala-arm_2.12-2.0.jar"
)


download.file(
  "https://repo1.maven.org/maven2/ml/combust/bundle/bundle-ml_2.12/0.20.0/bundle-ml_2.12-0.20.0.jar",
  "internal/mleap-spark/bundle-ml_2.12-0.20.0.jar"
)


sparklyr_spec <- spark_default_compilation_spec()

mleap_files <- mleap_path %>% 
  dir_ls() %>% 
  path_abs()

mleap_spec <- sparklyr_spec %>% 
  map(~{
    x <- .x
    x$jar_dep <- mleap_files
    #x$jar_dep <- "ml.combust.mleap:mleap-spark_2.12:0.20.0"
    x$embedded_srcs <- NULL
    x
  }) %>% 
  keep(~ .x$spark_version >= "2.4.0")  

compile_package_jars(spec = mleap_spec[[2]])


