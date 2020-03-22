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


access_score_by_geoid <- access_score_by_geoid %>% filter(., !is.na(categ))

n_distinct(access_score_by_geoid$category, na.rm = TRUE)
categories = access_score_by_geoid %>% filter(., !is.na(category)) %>%  unique(category)

number_of_categories = n_distinct(access_score_by_geoid$category)
number_of_categories

tibble(
  category = unique(access_score_by_geoid$category),
  percent = rep(1 / number_of_categories, length.out = number_of_categories)
)


readr::read_csv("./ccc_nyc_shiny/input/category_percents/category_percents.csv")

length(c("overall", unique(access_score_by_geoid$category)))

parent_path = "./ccc_nyc_shiny/"
file_path = paste0(parent_path, "data/nyc_geo")
file_name = "/nyc2010census_tract_nta_equiv.xlsx"
census_tract_to_nta <- readxl::read_xlsx(paste0(file_path, file_name))

?stringr::str_sub()

nyc_just_geoid_geom_sf %>% mutate(., fips_county_code = str_sub(GEOID, start = 3, end = 5)
                                  ,census_tract_2010 = str_sub(GEOID, start = 6)) %>% 
  left_join(., y = census_tract_to_nta,by = c("fips_county_code", "census_tract_2010"))

  