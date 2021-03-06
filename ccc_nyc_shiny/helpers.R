# This file is a series of "helper functions" defined so here
# so as to not clutter up the global.R or other files within the Shiny app
library(dplyr)
library(leaflet)
library(sf)
library(tidycensus)

############################################## 1. Resource Table Management

# 1A Add Resource to New or Pre-existing table
add_resource <- function(new_resource_tbl, name_col, category_col,
                         geometry_col = "geometry", current_resource_tbl=NULL
                         , lat_col = "Latitude", lon_col = "Longitude"){
  
  # add test to raise error if these are not in the column names
  selectedColumns <- c(name_col, category_col, lat_col, lon_col)
  
  if(geometry_col != "geometry"){
    new_resource_tbl <- new_resource_tbl %>% 
      select(all_of(selectedColumns)) %>% 
      sf::st_as_sf(.,coords = c(lon_col, lat_col), crs = 4326)
  } else {
    selectedColumns <- c(name_col, category_col, geometry_col)
    
    new_resource_tbl <- new_resource_tbl %>% 
      select(all_of(selectedColumns)) %>% 
      sf::st_as_sf(., crs = 4326)
  }
  
  colnames(new_resource_tbl) <- c("name", "category", "geometry")
  
  if (is.null(current_resource_tbl) == TRUE){
    return(new_resource_tbl)
  } else {
    return(rbind(current_resource_tbl, new_resource_tbl))
  }
}

# 2A Update Resource GeoJson file
update_resource_file <- function(resource_input){
  resource_file = paste0(parent_path, "resources/resource_list.geojson")
  
  if(file.exists(resource_file)){
    file.remove(resource_file)
  }
  
  sf::st_write(obj = resource_input, dsn = resource_file)
}

# 3A Read-In Resource GeoJson file
read_resource_file <- function(){
  resource_file = paste0(parent_path, "resources/resource_list.geojson")
  
  return(sf::st_read(dsn = resource_file))
}

find_tvl_radius <- function(geometry_sf, geo_id, travel_time_cutoff){
  geometry_sf %>% 
    as_tibble() %>% 
    filter(., GEOID == geo_id) %>% 
    select(., GEOID) %>% 
    inner_join(x=., y=nyc_trvl_times, by = c("GEOID" = "origin")) %>% 
    filter(., minutes <= travel_time_cutoff) %>% 
    select(., GEOID = destination, minutes) %>% 
    inner_join(., ny_census_tracts_wo_water, by = "GEOID") 
    # %>% 
    # st_as_sf() %>% 
    # leaflet::leaflet() %>% addProviderTiles("CartoDB.DarkMatter") %>% addPolygons()
    # 
}

show_resources_in_tvl_radius <- function(resources_sf, geo_id_vctr){
  # returns sf pertaining to geoids in the travel time range derived from "find tvl radius" function
  resources_sf %>% filter(., resource_geoid %in% geo_id_vctr) %>% 
    leaflet() %>% 
    addProviderTiles("CartoDB.DarkMatter") %>% addPolygons()
}

# Resource Count Table

update_resource_ct_sf <- function(current_resource_ct_table, new_resource_sf
                                  , census_geo_sf, resource_category,travel_time_cutoff = 60){
  # this function is meant to calculate access score for individual census tracts 
  # it serves as the prep function for displaying a table and a histogram of this information.
  
  return(
    sf::st_join(x = census_geo_sf, y = new_resource_sf, join = st_intersects, left = FALSE) %>% 
    group_by(GEOID, category) %>% count() %>% select(., resource_geoid = GEOID, category, count = n)
  )
  
}

update_resource_count_file <- function(resource_count_input){
  resource_count_file = paste0(parent_path, "resources/resource_count_by_geo.csv")
  
  if(file.exists(resource_count_file)){
    file.remove(resource_count_file)
  }
  
  readr::write_excel_csv(x = resource_count_input, path = resource_count_file)
}

read_resource_count_file <- function(){
  resource_ct_file = paste0(parent_path, "resources/resource_count_by_geo.csv")
  
  return(
    readr::read_csv(
      file = resource_ct_file, 
      col_types = list(col_character(), col_character(), col_integer())
      #, col_names = c("resource_geoid", "category", "count")
      )
    )
}

############################# Acces Score Mechanics

# Update Access Score Table
update_access_calc_tbl <- function(current_accesss_score_tbl, new_resource_category, resource_ct_tbl, travel_time_cutoff = 60){
  new_addition <- just_geoids %>% 
    left_join(., nyc_trvl_times_adj, by = c("GEOID" = "origin")) %>% 
    filter(., minutes <= travel_time_cutoff) %>% 
    left_join(., y=resource_ct_tbl, by = c("destination" = "resource_geoid")) %>%
    filter(., category == new_resource_category) %>% 
    left_join(., prcnt_fctr_by_time_bin, by = "minutes_bin") %>% 
    mutate(., weighted_score = if_else(is.na(minutes_bin),0, count * percent)) %>%
    group_by(GEOID, category) %>%
    summarise(weighted_score = sum(weighted_score))
    
  if (is.null(current_accesss_score_tbl) == TRUE){
    return(new_addition)
  } else{
    bind_rows(current_accesss_score_tbl, new_addition)
  }
}

# Update Access Score File
read_access_score_file <- function(){
  access_score_file = paste0(parent_path, "resources/access_score_by_geo.csv")
  
  return(
    readr::read_csv(
      file = access_score_file, 
      col_types = list(col_character(), col_character(), col_double())
      #, col_names = c("resource_geoid", "category", "count")
    )
  )
}

update_access_score_file <- function(access_score_input){
  access_score_file = paste0(parent_path, "resources/access_score_by_geo.csv")
  
  if(file.exists(access_score_file)){
    file.remove(access_score_file)
  }
  
  readr::write_excel_csv(x = access_score_input, path = access_score_file)
}

# rewrite later to take input from the find_tvl_radius funcitons 
calc_access_score_detail <- function(census_sf_object, geo_id, resource_category, travel_time_cutoff){
  # this function is meant to calculate access score for individual census tracts 
  # it serves as the prep function for displaying a table and a histogram of this information.
  #nyc_just_geoid_geom_sf %>% filter(., GEOID == geo_id ) %>%  as_tibble() %>% 
  resource_sf %>% filter(., GEOID == geo_id ) %>%  as_tibble() %>% 
    select(., GEOID) %>% inner_join(x=., y=nyc_trvl_times_adj, by = c("GEOID", "origin")) %>%
    filter(., minutes <= travel_time_cutoff) %>% inner_join(x=., y=resource_ct_by_geoid) %>% 
    filter(., category == resource_category) %>% 
    arrange(., desc(resource_count)) %>% 
    mutate(., weighted_score = resource_count / minutes)
}


# nyc_just_geoid_geom_sf %>% filter(., GEOID == "36005000100" ) %>%  as_tibble() %>% 
#   select(., GEOID) %>% inner_join(x=., y=nyc_trvl_times, by = c("GEOID" = "origin")) %>%
#   filter(., minutes <= 60) %>% inner_join(x=., y=resource_ct_by_geoid, by = c("destination" = "resource_geoid")) %>% 
#   filter(., category == "food retail") %>% group_by(minutes) %>% summarise(., resource_count = sum(count)) %>% 
#   arrange(., desc(resource_count)) %>% 
#   mutate(., weighted_score = resource_count / minutes)

# access_score_by_geoid
# 
# resource_ct_geoid_sf

#find_resources_in_tvl_radius(resources_sf = resource_ct_geoid_sf, geo_id_vctr = c("36005000100", "36005000200"))



# resource_ct_geoid_sf %>% filter(., resource_geoid %in% c("36005000100", "36005000200")) %>% 
#   leaflet() %>% 
#   addProviderTiles("CartoDB.DarkMatter") %>% addPolygons()

# add_resource(nyc_food_retail, name_col = 'DBA Name', type_col = 'Operation Type', capacity_amt_col = 'Square Footage',
#              capacity_unit_col = "sqft", geom_col = "geometry", current_resource_tbl = NULL)
# 
# add_resource(new_resource_tbl = early_chood_ctrs_sf, name_col = 'Address', type_col = "SiteType",
#              capacity_amt_col = "Total Enrollment", capacity_unit_col = "enrollment", geom_col = "geometry",
#              current_resource_tbl = NULL)

# find_tvl_radius(geometry_sf = nyc_just_geoid_geom_sf, geo_id = "36005000100", travel_time_cutoff = 60)
# 
# nyc_just_geoid_geom_sf %>% filter(., GEOID == "36005000100") %>%
#   as_tibble() %>% 
#   select(., GEOID) %>% 
#   inner_join(x=., y=nyc_trvl_times, by = c("GEOID" = "origin")) %>% 
#   filter(., minutes < 60) %>% 
#   select(., GEOID = destination, minutes) %>% 
#   inner_join(., ny_census_tracts_wo_water, by = "GEOID") %>% 
#   st_as_sf() %>% 
#   leaflet::leaflet() %>% addProviderTiles("CartoDB.DarkMatter") %>% addPolygons()


# nyc_food_retail$`Square Footage`
# table1 <- add_resource(nyc_food_retail, name_col = 'DBA Name', type_col = 'Operation Type', capacity_amt_col = 'Square Footage',
#                          capacity_unit_col = "sqft", geom_col = "geometry", current_resource_tbl = NULL
#                          )

# add_resource(nyc_food_retail, name_col = 'DBA Name', type_col = 'Operation Type', capacity_amt_col = 'Square Footage',
#              capacity_unit_col = "sqft", geom_col = "geometry", current_resource_tbl = table1
# )
# 
# name_col = 'DBA Name'
# type_col = 'Operation Type'
# capacity_amt_col = 'Square Footage'
# capacity_unit_col = "sqft"
# geom_col = "geometry"
# 
# nyc_food_retail %>% 
#   select(., name = name_col, type = type_col, capacity_amt = capacity_amt_col) %>%
#   mutate(., capacity_unit_col = rep(capacity_unit_col, length.out = nrow(nyc_food_retail)))
#   
# 
# 
# rep(capacity_unit_col, length.out = nrow(nyc_food_retail))
# colnames(nyc_food_retail)    
# 
# add_resource()