#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output, session) {
  
  output$perc_table <- DT::renderDataTable({
    DT::datatable(category_percents)
    })

  output$accessMap<- renderLeaflet({
    ny_census_tracts_wo_water %>%
      as_tibble() %>% filter(., estimate != 0) %>%
      left_join(x = ., y = access_score_by_geoid, by="GEOID") %>%
      filter(., category == input$select_category) %>%
      replace_na(., list(category = "", weighted_score = 0)) %>%
      st_as_sf() %>%
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
    })
  
  ### Data for the exploratory tab
  # observe({
  #   nta_codes <- unique(nyc_just_geoid_geom_sf %>% 
  #                         filter(nyc_just_geoid_geom_sf$borough_name) %>% .$nta_code)
  #   updateSelectizeInput(
  #     session = ssession,
  #     inputId = "nta",
  #     choices = nta_codes,
  #     selected = nta_codes[1]
  #   )
  # })
    
})
