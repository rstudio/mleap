library(curl)
library(rvest)
library(purrr)
library(stringr)
library(dplyr)
library(readr)

devtools::load_all()

maven_repo <- get_session_defaults("installation", "maven", "repo")

combust_root <- c("ml", "combust")

package_roots <- c("bundle", "mleap")

base_urls <- paste0(maven_repo, paste0(combust_root, collapse = "/"), "/", package_roots)

get_links <- function(x)  {
  links <- curl(x) %>%
    read_html() %>%
    html_elements("a") %>%
    html_attr("href")
  ret <- links[links != "../"]
  ret <- ret[!str_detect(ret, "maven-metadata")]
  ret
}

base_links <- base_urls %>% 
  map(~ paste(.x, get_links(.x), sep = "/")) %>% 
  flatten() 

all_links <- base_links %>% 
  map(get_links) %>% 
  set_names(base_links)

mleap_packages <- all_links %>% 
  imap_dfr(~ {
    base_name <- .y
    for(i in seq_along(base_urls)) {
      base_name <- str_remove(base_name, base_urls[[i]])
    }
    pkg_name <- str_remove_all(base_name, "/")
    name_split <- str_split(pkg_name, "_")[[1]]
    maven <- .y %>% 
      str_remove(maven_repo) %>% 
      str_remove(base_name) %>% 
      str_replace_all("/", ".")
    
    mleap <- str_remove(.x, "/")
    tibble(
      mleap = mleap,
      scala = name_split[[2]], 
      name = str_remove(name_split[[1]], "mleap-"),
      package = paste(maven, pkg_name, mleap, sep = ":")
    )
  }) 

mleap_versions <- mleap_packages %>% 
  group_by(mleap, scala) %>% 
  summarise() %>% 
  group_by(mleap) %>% 
  mutate(default = ifelse(scala == max(scala), "yes", "no")) %>% 
  ungroup()

mleap_defaults <- mleap_versions[mleap_versions$default == "yes", ]

write_rds(mleap_packages, "inst/extdata/mleap_packages.rds")
write_rds(mleap_versions, "inst/extdata/mleap_versions.rds")
write_rds(mleap_defaults, "inst/extdata/mleap_defaults.rds")
