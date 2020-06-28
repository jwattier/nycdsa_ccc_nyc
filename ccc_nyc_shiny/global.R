# import needed libraries
# library(opentripplanner)
library(DT)
# library(bbplot)
library(geojsonsf)
# library(tidycensus)
library(htmltools)
library(readxl)
library(leaflet)
library(leaflet.providers)
# library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(sf)
library(sp)
library(lwgeom)


# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

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
nyc_census_tracts_opendatanyc <- geojson_sf(file_geojson) %>% sf::st_make_valid() %>% 
  sf::st_cast("MULTIPOLYGON") 

# done to conform census tract # to that of the pre-computed #s
nyc_census_tracts_opendatanyc <- nyc_census_tracts_opendatanyc %>% as_tibble() %>% 
  left_join(x=., y= boro_cd_to_fips_mapping) %>% 
  mutate(., GEOID = paste0(fips_state_county_code, ct2010)) %>% st_as_sf()


#---------------------- import pre-computed times
# 1) import transit times from pre_processed subfolder
# which represents combining of a subset of the UoC pre-computted times for transit_walk travel mode
file_name = "/pre_process/nyc_trvl_times.csv.bz2"
nyc_trvl_times <- readr::read_csv(paste0(parent_path, file_name), col_types = "ccd")


# 2) create table with only GEOID and the geoid geometry

nyc_just_geoid_geom_sf <- nyc_census_tracts_opendatanyc %>% select(., GEOID, geometry)
resource_ct_geoid_sf <- nyc_just_geoid_geom_sf # for use later to add in resource counts

#---------------------- pull in census informaton from csv file
# Note this census information was retrieved via the data/census subfolder
# The code that produces this file is in the pre_process.R file
file_path = paste0(parent_path, "data/census")
file_name = "/nyc_census_tract_population.csv"

nyc_census_tract_population <- readr::read_csv(paste0(file_path, file_name),
                                               col_types = "cccdd")

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

resource_sf <- add_resource(nyc_food_retail, name_col = 'DBA Name', category_col = 'Operation Type', 
                            # capacity_amt_col = 'Square Footage',
                            # capacity_unit_col = "sqft", 
                            geom_col = "geometry", current_resource_tbl = NULL
                            )

# 2) early childhood centers
file_path = paste0(parent_path, "data/resources/early_childhood")
file_name = "/ccc_nyc_early_childhood_centers.xlsx"

early_chood_ctrs_sf <-readxl::read_xlsx(
  path = paste0(file_path, file_name),
  col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric",
                "text", "text", "numeric", "numeric", "numeric", "numeric", "text",
                "text", "text", "text", "text")
) %>% st_as_sf(., coords = c("Long", "Lat"), crs = 4326)

resource_sf <- add_resource(new_resource_tbl = early_chood_ctrs_sf, name_col = 'Address', category_col = "SiteType",
                            # capacity_amt_col = "Total Enrollment", capacity_unit_col = "enrollment", 
                            current_resource_tbl = resource_sf)

# 3) optional set of assets 
asset_dir <- paste0("./data/resources/custom")
asset_csv_files <- fs::dir_ls(asset_dir)
asset_files <- asset_csv_files %>% 
  map_dfr(readr::read_csv)

custom_resource_sf <-add_resource(
    new_resource_tbl = asset_files, name_col = "Description", category_col = "Category", 
    # capacity_amt_col = NA,
    # capacity_unit_col = NA, 
    geom_col = "latlong", current_resource_tbl = resource_sf)

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

resource_ct_by_geoid <- update_resource_ct_sf(current_resource_ct_table = resource_ct_by_geoid, 
                                              new_resource_sf = custom_resource_sf,
                                              census_geo_sf = nyc_just_geoid_geom_sf, 
                                              resource_category = "workforce development",
                                              travel_time_cutoff = 60)

resouse_categories <- c(unique(resource_ct_by_geoid$category))

# ----------------- Set up binning system of 0 to 15, 30 to 45, and 45 to 60
# Default setting will be a 100% attribution and then change based upon user selection/settings.
breaks <- c(0, 15, 30, 45, 60)
tags <- c("0-15", "15-30", "30-45", "45-60 mins")

nyc_trvl_times_adj <- nyc_trvl_times %>% 
  filter(., minutes <= 60) %>% 
  mutate(., minutes_bin = cut(minutes, breaks = breaks, include.lowest = TRUE, right = TRUE, labels = tags))

# factor table is pre-loaded with defaults of 1 per travel minute bin
prcnt_fctr_by_time_bin <- tibble(
  minutes_bin = tags,
  percent = rep(1, length(tags))
)


# -------------------- compute access score via distance weighting against number of resources


just_geoids <- nyc_just_geoid_geom_sf %>% as_tibble() %>% select(., GEOID)

access_score_by_geoid <- 
  just_geoids %>% 
  left_join(., nyc_trvl_times_adj, by = c("GEOID" = "origin")) %>%
  left_join(., resource_ct_by_geoid, by = c("destination" = "resource_geoid")) %>%
  left_join(., prcnt_fctr_by_time_bin, by = "minutes_bin") %>% 
  mutate(., weighted_score = if_else(is.na(minutes_bin),0, count * percent)) %>%
  group_by(GEOID, category) %>%
  summarise(weighted_score = sum(weighted_score))

access_score_4_pal <- access_score_by_geoid %>% filter(., category == "early childhood center")
pal <- colorNumeric("viridis", domain = access_score_4_pal$weighted_score)


### Output resources to file ## TODO
# file is located in the resources sub-directory
resource_fileg <- paste0(parent_path, "resources/resource_list.geojson")

  
  
  