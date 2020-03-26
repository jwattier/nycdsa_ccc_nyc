library(tidyverse)

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
