# This file is a series of "helper functions" defined so here
# so as to not clutter up the global.R or other files within the Shiny app
library(dplyr)
library(leaflet)
library(sf)


add_resource <- function(new_resource_tbl, name_col, type_col, capacity_amt_col, 
                         capacity_unit_col, geom_col = "geometry", current_resource_tbl=NULL){
  # current_resource_tbl is the tibble/data frame object 
  # name is the label provided to the location (e.g., the DBA name of a business)
  # type_col is the type category (e.g., Charter or DOE school in the case of schools) 
  # capacity_amt_col is the column where the "# of units" is provided re: how a location is measure (e.g., sqft or number of seates/doctors
  # capity_unit_col represents the unit of measure for the capacity_amt column 
  # geom_col is the location of the geometry column, assumed to be geometry
  
  new_addition <- new_resource_tbl %>% 
    select(., name = name_col, type = type_col, capacity_amt = capacity_amt_col) %>%
    mutate(., capacity_unit_col = rep(capacity_unit_col, length.out = nrow(new_resource_tbl)))
  
  if (is.null(current_resource_tbl)){
    return(
      new_addition
    )
  } else {
    return(rbind(current_resource_tbl, new_addition))
  }
  
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
#find_tvl_radius(geometry_sf = nyc_just_geoid_geom_sf, geo_id = "36005000100", travel_time_cutoff = 60)
# 

show_resources_in_tvl_radius <- function(resources_sf, geo_id_vctr){
  # returns sf pertaining to geoids in the travel time range derived from "find tvl radius" function
  resources_sf %>% filter(., resource_geoid %in% geo_id_vctr) %>% 
    leaflet() %>% 
    addProviderTiles("CartoDB.DarkMatter") %>% addPolygons()
}


# rewrite later to take input from the find_tvl_radius funcitons 
calc_access_score_detail <- function(sf_object, geo_id, resource_category, travel_time_cutoff){
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