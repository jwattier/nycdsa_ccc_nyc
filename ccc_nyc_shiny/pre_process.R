library(tidyverse)
library(tidycensus)

# This file is meant to be run prior to the starting or deploying of a Shiny app.

# Certain pre-processing steps that do not change often 
#(e.g., subsetting of travel times from pre-computed Open Trip Planner by university of Chicago)

# The intent is to run those once (or fewer times) via this pre-processing file.

############################################
# 1. Pre-Process of University of Chicago pre-computed times through OpenTripPlanner
data_dir <- paste0("./data/uofc_precomputed_times/transit_walk")
csv_files <- fs::dir_ls(data_dir)
nyc_trvl_times <- csv_files %>% 
  map_dfr(readr::read_csv, col_types = "ccd") %>%
  # 36 is the FIPS code for NY state (first two digits), the 3-5th digits represent the 
  # county.
  filter(., str_sub(destination, 1, 5) %in% c("36061", "36047", "36081", "36085", "36005"))


# fix 0 minute travel times to 1 minute
nyc_trvl_times <- nyc_trvl_times %>% mutate(., minutes = if_else(condition = minutes < 1, 
                                                                 true = 1, false =  minutes))

# output file to pre_process folder
readr::write_excel_csv(nyc_trvl_times, path = "./pre_process/nyc_trvl_times.csv.bz2")

############################################
# 2. Pull in census data ahead of time (in lieu of utilizing API key within application, which
# throws an error when deploying to shared website)

# 2a) define function for potential furture use
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

# 2b) utilize function to retrieve nyc census tract information
nyc_census_tract_population <- census_fun(geography_type = "tract", acs_year = 2018)

file_path = "./data/census/"
file_name = "nyc_census_tract_population.csv"

write.csv(nyc_census_tract_population, file = paste0(file_path, file_name),
          row.names = FALSE)
