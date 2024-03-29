library(purrr)
library(fs)
library(sparklyr)
devtools::load_all()


base_folder <- "internal/mleap-spark"

versions <- mleap_dep_versions_list()

# ----------- Completes installation grid, and downloads missing jars ----------

prep_versions <- map(
  versions, 
  ~{
    x <- .x
    version_path <- paste0(base_folder, "/", x$scala, "-", x$mleap)
    x$version_abs <- path_abs(version_path)
    x
  }
)

for(i in seq_along(versions)) {
  version_folder <- prep_versions[[i]]$version_abs
  maven_path <- prep_versions[[i]]$maven
  if(!dir_exists(version_folder)) {
    cat(paste0("Populating: ", version_folder, "\n"))
    dir_create(version_folder)
    maven_download_jars(resolve_maven_path(), 
                  maven_path, 
                  version_folder, 
                  use_temp_cache = TRUE
                  )
  }

}

# -------------- Installs missing local Spark versions -------------------------
siv <- spark_installed_versions()
unique_siv <- unique(siv$spark)
needed_versions <- map_chr(versions, ~.x$spark)

walk(
  needed_versions, 
  ~ if(!(.x %in% unique_siv)) spark_install(version = .x)
)

# ------- Creates sparklyr jar compilation spec and runs compile ---------------

base_spec <- spark_default_compilation_spec()
jar_path <- base_spec[[1]]$jar_path

mleap_spec <- prep_versions %>% 
  map(~{
    list(
      spark_version = .x$spark,
      spark_home = spark_home_dir(.x$spark),
      scalac_path = find_scalac(.x$scala),
      jar_name = .x$jar_name,
      jar_path = jar_path,
      jar_dep = dir_ls(.x$version_abs)
    )
  }) 


compile_package_jars(spec = mleap_spec)

