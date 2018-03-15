library(purrr)
spec <- sparklyr::spark_default_compilation_spec() %>%
  map(function(x) {
    x$jar_dep <- list.files(sparklydeploy:::resolve_mleap_path(), full.names = TRUE) %>% 
      grep("(mleap|scala|bundle).+jar$", ., value = TRUE) %>%
      map_chr(normalizePath)
    x
  }) %>%
  keep(~ .x$spark_version >= "2.0.0")

sparklyr::compile_package_jars(spec = spec)
