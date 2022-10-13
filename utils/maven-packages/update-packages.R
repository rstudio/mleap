library(curl)
library(rvest)
library(purrr)
library(stringr)
library(dplyr)

devtools::load_all()

maven_repo <- mleap_get_session_defaults("installation", "maven", "repo")



mleap_root <- c("ml", "combust", "mleap")
mleap_url <- paste0(maven_repo, paste0(mleap_root, collapse = "/"))



get_links <- function(x)  {
  links <- curl(x) %>%
    read_html() %>%
    html_elements("a") %>%
    html_attr("href")
  ret <- links[links != "../"]
  ret <- ret[!str_detect(ret, "maven-metadata")]
  
}

mleap_links <- get_links(mleap_url)
mleap_links <- mleap_links %>% 
  imap(~ get_links(paste0(mleap_url, "/", .x))) %>% 
  set_names(mleap_links)


maven_root <- paste0(mleap_root, collapse = ".")

mleap_entries <- mleap_links %>% 
  imap_dfr(~ {
    pkg_name <- str_remove(.y, "/")
    name_split <- str_split(pkg_name, "_")[[1]]
    mleap <- str_remove(.x, "/")
    tibble(
      mleap = mleap,
      scala = name_split[[2]], 
      name = str_remove(name_split[[1]], "mleap-"), 
      package = paste(maven_root, pkg_name, mleap, sep = ":")
    )
  }) 

mleap_versions <- mleap_entries %>% 
  group_by(mleap, scala) %>% 
  summarise() %>% 
  group_by(mleap) %>% 
  mutate(default = ifelse(scala == max(scala), "yes", "no")) %>% 
  ungroup()

mleap_defaults <- mleap_versions[mleap_versions$default == "yes", ]

mleap_defaults[mleap_defaults$mleap == "0.10.0", ]

