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
  # 
  # # Reactive expression for the access scores subsetted to what
  # # resource category the user selected.
  # # filetered_access_score <- reactive({
  # #   access_score_by_geoid %>% filter(., category == input$select_category)
  # # })
  # 
  # # This reactive expression represents the pallette function,
  # # which changes as the user makes selections in UI.
  # access_colorpal <- reactive({
  #   
  #   access_scores <- access_score_by_geoid %>% filter(., category == input$select_category)
  #   
  #   colorNumeric("viridis", domain = access_scores$weighted_score)
  # })
  # 
  # # BASE MAP
  # # this section draws the portion of the map that is not subject to change
  # ouput$accessMap <- renderLeaflet({
  #     leaflet() %>% 
  #     setView(lat = 40.7128, lng = -74.0060, zoom = 10) %>% 
  #     addProviderTiles("CartoDB.Positron") 
  # })
  # 
  # # observer for instance where the resource category is changed
  # observe({
  #   pal <- access_colorpal()
  #   
  #   # access_scores <- filetered_access_score()
  #   
  #   # nyc_access_score_sf <- ny_census_tracts_wo_water %>% 
  #   #   as_tibble() %>% filter(., estimate != 0) %>% 
  #   #   left_join(x = ., y = access_scores, by="GEOID") %>% 
  #   #   replace_na(., list(category = "", weighted_score = 0)) %>% 
  #   #   st_as_sf() 
  #   
  #   nyc_access_score_sf <- ny_census_tracts_wo_water %>% 
  #       as_tibble() %>% filter(., estimate != 0) %>%
  #       left_join(x = ., y = access_score_by_geoid, by="GEOID") %>%
  #       filter(., category == input$select_category) %>%
  #       replace_na(., list(category = "", weighted_score = 0)) %>%
  #       st_as_sf()
  #   
  #   leafletProxy("accessMap", session, data = nyc_access_score_sf) %>% 
  #     clearShapes() %>% 
  #     addPolygons(
  #       fillColor = ~pal(weighted_score),
  #       stroke = FALSE, 
  #       weight = 2,
  #       opacity = 1,
  #       color = "white",
  #       dashArray = "3", 
  #       fillOpacity = 0.7, 
  #       highlight = highlightOptions( 
  #         weight = 5,
  #         color = '#666',
  #         dashArray = "",
  #         fillOpacity = 0.7,
  #         bringToFront = TRUE))
  #   })

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

})
