# AIDA - Michael Francke --------------------------------------------------

# the AIDA package contains several datasets for psychological experiments

url <- "https://github.com/michael-franke/aida-package/raw/master/data/%s"

aida_files <- c(
  simon = "data_ST_raw.rda" # simon task
)

aida_local <- file.path("data", sprintf("%s.%s", names(aida_files), tools::file_ext(aida_files)))
aida_urls <- sprintf(url, aida_files)

purrr::map2(aida_urls, aida_local, download.file)
