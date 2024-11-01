library(descr)
library(anesr)
# devtools::install_github("jamesmartherus/anesr")
data( package = "anesr")




library(tidyverse)
library(tidycensus)
# 64fb9609fe9ea164a260d685d224f9aa1eb23eea
census_api_key("64fb9609fe9ea164a260d685d224f9aa1eb23eea", overwrite = TRUE, install = TRUE)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")
