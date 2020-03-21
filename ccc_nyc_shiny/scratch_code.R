library(tidycensus)
library(tidyverse)
library(tmap)
library(sf)


file_path = "./ccc_nyc_shiny/data/resources/early_childhood"
file_name = "/ccc_nyc_early_childhood_centers.xlsx"


early_childhood_centers <- readxl::read_xlsx(
  path = paste0(file_path, file_name),
  col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric",
                "text", "text", "numeric", "numeric", "numeric", "numeric", "text",
                "text", "text", "text", "text")
  )

early_chood_ctrs_sf <- early_childhood_centers %>% st_as_sf(., coords = c("Long", "Lat"), crs = 4326)

tmap::qtm(early_chood_ctrs_sf)

# function(file_path, file_format, lat_col, long_col){
#   if (str_lower(file_format) == "excel"){
#     return(
#       
#     )
#   }
# }

early_chood_ctrs_sf <-readxl::read_xlsx(
  path = paste0(file_path, file_name),
  col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric",
                "text", "text", "numeric", "numeric", "numeric", "numeric", "text",
                "text", "text", "text", "text")
) %>% st_as_sf(., coords = c("Long", "Lat"), crs = 4326)
