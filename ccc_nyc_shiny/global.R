# import needed libraries
library(opentripplanner)
library(DT)
library(geojsonsf)
library(htmltools)
library(readxl)
library(leaflet)
library(leaflet.providers)
library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(sf)
library(sp)
library(lwgeom)


# 
# census_api_key("9b7b36ca21156e2f5e04583421016793d575d7b0")

options(tigris_use_cache = TRUE)

#parent_path = "./ccc_nyc_shiny/"
parent_path = "./"

source(paste0(parent_path, "helpers.R"))


#---------------------- import census tract information from opendata nyc files
file_path = paste0(parent_path, "data/nyc_geo")
file_name = "/nyc_boro_to_fips_mapping.xlsx"
boro_cd_to_fips_mapping <- readxl::read_xlsx(paste0(file_path, file_name))

file_name = "/2010 Census Tracts.geojson"
file_geojson = paste0(file_path, file_name)
nyc_census_tracts_opendatanyc <- geojson_sf(file_geojson) %>% lwgeom::st_make_valid() %>% 
  sf::st_cast("MULTIPOLYGON") 

# done to conform census tract # to that of the pre-computed #s
nyc_census_tracts_opendatanyc <- nyc_census_tracts_opendatanyc %>% as_tibble() %>% 
  left_join(x=., y= boro_cd_to_fips_mapping) %>% 
  mutate(., GEOID = paste0(fips_state_county_code, ct2010)) %>% st_as_sf()

#nyc_census_blocks_opendatanyc %>% as_tibble()

#---------------------- import census tracts
# 1) define function for potential furture use
census_fun <- function(geography_type="tract", acs_year=2018) {
  # this function retrieves the shape file for NYC and
  return (
    tidycensus::get_acs(geography = geography_type, year = acs_year, geometry = FALSE,
                        variables = "B01003_001", # retrieves population
                        survey = "acs5", county = c(061, 047, 081, 085, 005)
                        , state = "NY")
    # crs => coordinate reference system
  )
}

# 2) utilize function to retrieve nyc census tract information
nyc_census_tract_population <- census_fun(geography_type = "tract", acs_year = 2018)
#nyc_census_tract_population

#---------------------- import pre-computed times
# 1) import transit times from the transit_walk subfoder
#    -> there is a separate file for each borough
data_dir <- paste0(parent_path, "data/uofc_precomputed_times/transit_walk")
csv_files <- fs::dir_ls(data_dir)
nyc_trvl_times <- csv_files %>% 
  map_dfr(readr::read_csv, col_types = "ccd") %>%
  # 36 is the FIPS code for NY state (first two digits), the 3-5th digits represent the 
  # county.
  filter(., str_sub(destination, 1, 5) %in% c("36061", "36047", "36081", "36085", "36005"))


# fix 0 minute travel times to 1 minute
nyc_trvl_times <- nyc_trvl_times %>% mutate(., minutes = if_else(condition = minutes == 0, 
                                                                 true = 1, false =  minutes))

# 2) create table with only GEOID and the geoid geometry

nyc_just_geoid_geom_sf <- nyc_census_tracts_opendatanyc %>% select(., GEOID, geometry)
resource_ct_geoid_sf <- nyc_just_geoid_geom_sf # for use later to add in resource counts

# 3) add to the "just geoid" tibble in order to include information on NTA, PUMA and borough linkage
# file_path = paste0(parent_path, "data/nyc_geo")
# file_name = "/nyc2010census_tract_nta_equiv.xlsx"
# 
# census_tract_to_nta_mapping <- readxl::read_xlsx(paste0(file_path, file_name))
# 
# 
# nyc_just_geoid_geom_sf <- nyc_just_geoid_geom_sf %>% 
#   mutate(., fips_county_code = str_sub(GEOID, start = 3, end = 5)
#            ,census_tract_2010 = str_sub(GEOID, start = 6)) %>% 
#   left_join(., y = census_tract_to_nta_mapping,by = c("fips_county_code", "census_tract_2010"))

#--------------------- import resource of interest
# 1) nyc food retail
# ---> OPEN -> LIST URL SOURCE OF INFORMATION 
nyc_food_retail <- read_csv(paste0(parent_path, "data/resources/retail_food/Retail_Food_Stores.csv"))%>% 
  filter(., County %in% c("New York", "Bronx", "Queens", "Kings", "Richmond"), `Square Footage` > 1000)%>%
  separate(., col = Location, into = c("street_address", "city_state", "lat_long"),sep = "\n") %>% 
  filter(., !is.na(lat_long)) %>% 
  separate(., col = "lat_long", into = c("long", "lat"), sep = ",") %>% 
  mutate(., long = str_replace(long, "\\(", ""), lat = str_replace(lat, "\\)", "")) %>% 
  mutate(., lat_nbr = as.numeric(lat), long_nbr = as.numeric(long)) %>% 
  st_as_sf(., coords = c("lat_nbr", "long_nbr"), crs=4326, agr = "constant")

resource_sf <- add_resource(nyc_food_retail, name_col = 'DBA Name', type_col = 'Operation Type', capacity_amt_col = 'Square Footage',
                            capacity_unit_col = "sqft", geom_col = "geometry", current_resource_tbl = NULL)

# 2) early childhood centers
file_path = paste0(parent_path, "data/resources/early_childhood")
file_name = "/ccc_nyc_early_childhood_centers.xlsx"

early_chood_ctrs_sf <-readxl::read_xlsx(
  path = paste0(file_path, file_name),
  col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric",
                "text", "text", "numeric", "numeric", "numeric", "numeric", "text",
                "text", "text", "text", "text")
) %>% st_as_sf(., coords = c("Long", "Lat"), crs = 4326)

resource_sf <- add_resource(new_resource_tbl = early_chood_ctrs_sf, name_col = 'Address', type_col = "SiteType",
                            capacity_amt_col = "Total Enrollment", capacity_unit_col = "enrollment", 
                            current_resource_tbl = resource_sf)

#--------------------- map resources to census areas
# this numeric vector is ordered the same as the rows on the nyc_just_geoid_geom_sf
resource_by_geoid_ct <- st_intersects(x=nyc_just_geoid_geom_sf, 
                                      y=nyc_food_retail, sparse = FALSE) %>%
  apply(., 1, sum)


resource_ct_geoid_sf$category <- "food retail"

resource_ct_geoid_sf$count <- resource_by_geoid_ct

colnames(resource_ct_geoid_sf) <- c("resource_geoid", "geometry", "category", "count")
resource_ct_geoid_sf

early_cc_by_geoid_ct <- st_intersects(x=nyc_just_geoid_geom_sf,
                                      y=early_chood_ctrs_sf, sparse = FALSE) %>% apply(., 1, sum)

resource_ct_by_geoid <- resource_ct_geoid_sf %>% as_tibble(.) %>% select(., resource_geoid,
                                                                         category, count)

new_row <- tibble(
  resource_geoid = nyc_just_geoid_geom_sf$GEOID,
  category = rep("early childhood centers", length.out = nrow(nyc_just_geoid_geom_sf)),
  count = early_cc_by_geoid_ct
)

resource_ct_by_geoid <- dplyr::bind_rows(resource_ct_by_geoid, new_row)

resouse_categories <- c(unique(resource_ct_by_geoid$category),"overall")

# Import resource percentages
category_percents <- readr::read_csv(paste0(parent_path, "input/category_percents/category_percents.csv"))

#invoke in-case user provided percentages do not add up to 0
if (sum(category_percents$percent) != 1){
  
  number_of_categories <- n_distinct(resource_ct_by_geoid$category)
  
  tibble(
    category = unique(resource_ct_by_geoid$category),
    percent = rep((1 / number_of_categories), length.out = number_of_categories)
  )
}

# -------------------- compute access score via distance weighting against number of resources
breaks <- c(0, 10, 20, 30, 40, 50, 60)
tags <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60 mins")

nyc_trvl_times_adj <- nyc_trvl_times %>%  filter(., minutes < 60)

just_geoids <- nyc_just_geoid_geom_sf %>% as_tibble() %>% select(., GEOID)

access_score_by_geoid <- 
  just_geoids %>% 
  left_join(., nyc_trvl_times_adj, by = c("GEOID" = "origin")) %>% 
  left_join(., y=resource_ct_by_geoid, by = c("destination" = "resource_geoid")) %>%
  mutate(., weighted_score = count / minutes) %>% 
  group_by(GEOID, category) %>% 
  summarise(weighted_score = sum(weighted_score))

### TO DO -> LOOK INTO WHY THERE'S A NA ROW re: category/access score
#access_score_by_geoid <- 
access_score_by_geoid$weighted_score <- access_score_by_geoid$weighted_score %>% replace_na(0)

# ny_water <- tigris::area_water("NY", "New York", class = "sf")
# ny_water <- sf::st_transform(ny_water, crs=4326)
# 
# st_erase <- function(x, y) {
#   sf::st_difference(x, sf::st_union(y))
# }
# ny_census_tracts_wo_water <- st_erase(nyc_census_tracts, ny_water)

# -------------------- build map with access score information
pal_pop <- colorNumeric("plasma", domain = nyc_census_tract_population$estimate)
pal_resource <- colorNumeric("magma", domain = resource_ct_by_geoid$count)


access_score_4_pal <- access_score_by_geoid %>% filter(., category == "early childhood center")
pal <- colorNumeric("viridis", domain = access_score_4_pal$weighted_score)




# nrow(resource_ct_geoid_sf)
# nrow(resource_by_geoid_ct)
# nrow(resource_sf)
# resource_by_geoid_ct

summary(resource_ct_by_geoid)
