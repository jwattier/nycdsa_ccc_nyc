library(geojsonsf)
library(tmap)
library(lwgeom)
library(htmltools)

sf::geojson_sf("")


?geojson_sf

getwd()
file = "Communit"

nyc_community_districts <- geojson_sf("Community Districts.geojson")

qtm(nyc_community_districts)

nyc_community_districts %>% lwgeom::st_make_valid() %>% sf::st_cast("MULTIPOLYGON")

nyc_community_districts %>% lwgeom::st_make_valid() %>%
  sf::st_cast("MULTIPOLYGON") %>% 
  leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::addPolygons(
    label = ~htmlEscape(boro_cd)
  )

nyc_community_districts <- nyc_community_districts %>% lwgeom::st_make_valid() %>%
  sf::st_cast("MULTIPOLYGON") 

nyc_census_tracts

st_crs(nyc_census_tracts)

st_intersects(x = nyc_census_tracts[1:10,], y = nyc_community_districts, sparse = FALSE) %>% 
  apply(., 1, sum)


nyc_census_tracts_opendatanyc <- geojson_sf("2010 Census Tracts.geojson")
nyc_census_tracts_opendatanyc %>% lwgeom::st_make_valid() %>% sf::st_cast("MULTIPOLYGON") %>% 
  qtm()

nyc_census_tracts_opendatanyc <- nyc_census_tracts_opendatanyc %>%  
  lwgeom::st_make_valid() %>% sf::st_cast("MULTIPOLYGON")

st_intersects(x = nyc_census_tracts_opendatanyc[1:10,], y = nyc_community_districts, sparse = FALSE) %>% 
  apply(., 1, sum)



nyc_census_blocks_opendatanyc <- geojson_sf("2010 Census Blocks.geojson") %>% lwgeom::st_make_valid() %>% 
  sf::st_cast("MULTIPOLYGON") 

nyc_census_blocks_opendatanyc %>% qtm()


# see how nyc census blocks give with 
b
resource_sf

st_intersects(x = nyc_census_tracts_opendatanyc, y= )

nyc_census_tracts_opendatanyc

length(unique(nyc_census_tracts_opendatanyc$boro_ct2010))
