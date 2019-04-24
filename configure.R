#!/usr/bin/env Rscript
library(purrr)

path <- "internal/mleap-spark"
if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}

mleap:::download_jars(mleap:::resolve_maven_path(),
                      "ml.combust.mleap:mleap-spark_2.11:0.10.1",
                      normalizePath(path),
                      use_temp_cache = TRUE
                      )

spec <- sparklyr::spark_default_compilation_spec() %>%
  map(function(x) {
    x$jar_dep <- list.files(path, full.names = TRUE) %>% 
      map_chr(normalizePath)
    x
  }) %>%
  keep(~ .x$spark_version >= "2.0.0")

sparklyr::compile_package_jars(spec = spec)
