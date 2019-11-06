#!/usr/bin/env Rscript
library(purrr)
mleap:::download_jars(mleap:::resolve_maven_path(),
                      "ml.combust.mleap:mleap-spark_2.11:0.14.0",
                      normalizePath("internal/mleap-spark"),
                      use_temp_cache = TRUE
                      )

spec <- sparklyr::spark_default_compilation_spec() %>%
  map(function(x) {
    x$jar_dep <- list.files("internal/mleap-spark", full.names = TRUE) %>% 
      map_chr(normalizePath)
    x
  }) %>%
  keep(~ .x$spark_version >= "2.0.0")

sparklyr::compile_package_jars(spec = spec)
