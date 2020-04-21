# NYCDSA - Capstone Project

This readme file is an explainer on the components of my capstone project for the NYC Data Science Academy.

## Project Goal

The capstone project involved supporting the Citzen's Committee for Children of New York (CCC NY) effort to create a framework to streamline accessibility analysis for the New York City (NYC) metropolitan area utilizing open source technologies.

## Project File Structure

1. Data Inputs
2. Preprocess
3. Shiny-specific files

### I. Data Inputs

The data input files represent a combination of i) geospatial files, ii) population or demongraphic information, iii) pre-computed travel times or iv) community resource files.

Geospatial files facilitate the visual projection of NYC based upon the underlying census area divisions. These files are in the form of GeoJson files obtained from NYC Open Data. The census areas currently represent the census tract level but in future iterations may be updated to be at the more granular census block level.

Population or other demographic information files were derived from census information obtained through the R package tidycensus (<https://walkerke.github.io/tidycensus/>). Currently the only population-related statistic utilized for supplementary analysis is the population count of census tracts from the 5-year American Communities Survey from 2018.

Pre-computed travel times represent mass-transit travel times between individual census tracts. These travel times were obtained from the University of Chicago's "access"-related project (<https://access.readthedocs.io/en/latest/resources.html>). Mass transit times were derived from OpenTripPlanner's API based upon a Monday 1:30 PM mass-transit routing option. The only transportation method excluded from this query was ferry.

Community resource files represent the location-based information of various items of interests, such as early childhoold centers, job placement sites or food retail. Typically the source of these files is either NYC Open Data, New York Open Data (the state repository) or the CCC NY's asset repository (<https://data.cccnewyork.org/>).

These combination of files help support accessibility analysis at the census tract level to determine how relatively which areas are better served for a given communal resource of interest. A higher accessibility rating is typically driven by having more resources that are closer to a given census area. A more recent accessibility score also factors a "conjestion" factor based upon the population near a resource as a penalty to an area's accessibility rating (<https://saxon.harris.uchicago.edu/~jsaxon/raam.pdf>).

### 2. Preprocess

There are two areas where processes or other calculations are performed in-advance and not "on-the-fly" within the Shiny app.

The first area of preprocessing is the pre_process.R file. Pre-processing measures here represent code executed...(section to be completed later :-)

### 3. Shiny-specific files

Placeholder for section to be completed later.
