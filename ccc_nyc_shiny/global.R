# import needed libraries
library(opentripplanner)
library(htmltools)
library(leaflet)
library(leaflet.providers)
library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
library(sf)
library(sp)
library(lwgeom)


#---------------------- import census tracts
# 1) define function for potential furture use
census_sf_fun <- function(geography_type="tract", acs_year=2018) {
  # this function retrieves the shape file for NYC and 
  return (
    tidycensus::get_acs(geography = geography_type, year = acs_year, geometry = TRUE, 
                        variables = "B01003_001", # retrieves population
                        survey = "acs5", county = c(061, 047, 081, 085, 005)
                        , state = "NY",cb = FALSE) %>% 
      st_transform(., crs=4326) # other map-based information follows the crs id of 4326
    # crs => coordinate reference system
  )
}

# 2) utilize function to retrieve nyc census tract information
nyc_census_tracts <- census_sf_fun(geography_type = "tract", acs_year = 2018)


#---------------------- import pre-computed times
# 1) import transit times from the transit_walk subfoder
#    -> there is a separate file for each borough
data_dir <- "./data/uofc_precomputed_times/transit_walk"
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

nyc_just_geoid_geom_sf <- nyc_census_tracts %>% select(., GEOID, geometry)
resource_ct_geoid_sf <- nyc_just_geoid_geom_sf # for use later to add in resource counts


#--------------------- import resource of interest
# 1) nyc food retail
# ---> OPEN -> LIST URL SOURCE OF INFORMATION 
nyc_food_retail <- read_csv("./data/resources/retail_food/Retail_Food_Stores.csv")%>% 
  filter(., County %in% c("New York", "Bronx", "Queens", "Kings", "Richmond"), `Square Footage` > 1000)%>%
  separate(., col = Location, into = c("street_address", "city_state", "lat_long"),sep = "\n") %>% 
  filter(., !is.na(lat_long)) %>% 
  separate(., col = "lat_long", into = c("long", "lat"), sep = ",") %>% 
  mutate(., long = str_replace(long, "\\(", ""), lat = str_replace(lat, "\\)", "")) %>% 
  mutate(., lat_nbr = as.numeric(lat), long_nbr = as.numeric(long)) %>% 
  st_as_sf(., coords = c("lat_nbr", "long_nbr"), crs=4326, agr = "constant")


#--------------------- map resources to census areas
# this numeric vector is ordered the same as the rows on the nyc_just_geoid_geom_sf
resource_by_geoid_ct <- st_intersects(x=nyc_just_geoid_geom_sf, 
                                      y=nyc_food_retail, sparse = FALSE) %>%
  apply(., 1, sum)


resource_ct_geoid_sf$category <- "food retail"

resource_ct_geoid_sf$count <- resource_by_geoid_ct

colnames(resource_ct_geoid_sf) <- c("resource_geoid", "geometry", "category", "resource_count")
resource_ct_geoid_sf

# -------------------- compute access score via distance weighting against number of resources
breaks <- c(0, 10, 20, 30, 40, 50, 60)
tags <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60 mins")

access_score_by_geoid <- nyc_census_tracts %>% 
  as_tibble() %>% #converting geometry object to normal tibble to allow easier combining with tvl times
  left_join(x=., y=nyc_trvl_times, by = c("GEOID" = "origin")) %>% 
  left_join(x=., y=resource_ct_geoid_sf, by = c("destination" = "resource_geoid")) %>% 
  # mutate(.,  minutes_bin = cut(minutes, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags)) %>% 
  mutate(., weighted_score = resource_count / minutes) %>% 
  group_by(GEOID, category) %>% 
  summarise(weighted_score = sum(weighted_score))

### TO DO -> LOOK INTO WHY THERE'S A NA ROW re: category/access score
access_score_by_geoid <- access_score_by_geoid %>% filter(., !is.na(category))
    
# nrow(resource_ct_geoid_sf)
# length(unique(nyc_trvl_times$origin))
# length(unique(nyc_trvl_times$destination))

# -------------------- build map with access score information
pal_pop <- colorNumeric("viridis", domain = ny_census_tracts_wo_water$estimate)

pal <- colorNumeric("viridis", domain = access_score_by_geoid$weighted_score)
# labels <- sprintf(
#   "<strong>%s</strong><br/>%g access score",
#   access_score_by_geoid$GEOID, access_score_by_geoid$weighted_score
# ) %>% lapply(htmltools::HTML) 

# nyc_census_no_wtr <- 
st_erase <- function(x, y) {
  sf::st_difference(x, sf::st_union(y))
}

ny_water <- tigris::area_water("NY", "New York", class = "sf")
ny_water <- sf::st_transform(ny_water, crs=4326)


ny_census_tracts_wo_water <- st_erase(nyc_census_tracts, ny_water)

access_score_map <- ny_census_tracts_wo_water %>% 
  as_tibble() %>% filter(., estimate != 0) %>% 
  left_join(x = ., y = access_score_by_geoid, by="GEOID") %>% 
  replace_na(list(category = "", weighted_score = 0)) %>% st_as_sf() %>% 
  leaflet() %>% 
  setView(lat = 40.7128, lng = -74.0060, zoom = 10) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(
              fillColor = ~pal(weighted_score),
              stroke = FALSE, 
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3", 
              fillOpacity = 0.7, 
              highlight = highlightOptions( 
                weight = 5,
                color = '#666',
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE)#,
              # label = labels,
              # labelOptions = labelOptions(
              #   style = list("font-weight" = "normal", padding = "3px 8px"),
              #   textsize = "15px",
              #   direction = "auto")
              ) %>% 
  addLegend(pal = pal, values = ~weighted_score, opacity = 0.7, title = "Access Score", 
            position = "bottomright")
               



