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
  
  # I. first page visualizations
  
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
  
  # II. Second page visualizations 
  
  ### Data for the exploratory tab
  # observe({
  #   nta_codes <- unique(nyc_just_geoid_geom_sf %>%
  #                         filter(nyc_just_geoid_geom_sf$borough_name == input$borough) %>% 
  #                           .$nta_code)
  #   updateSelectizeInput(
  #     session = session,
  #     inputId = "nta",
  #     choices = nta_codes,
  #     selected = nta_codes[1]
  #   )
  # })
    
    # filteredArea <- reactive({
    #   nyc_just_geoid_geom_sf %>% 
    #     as_tibble() %>% 
    #     filter(., borough_name == input$borough) %>% select(., GEOID) %>% 
    #     inner_join(., ny_census_tracts_wo_water)
    # })
    
    
    # filteredPopulation <- reactive({
    #   filteredArea() %>% summarise(population = sum(estimate))
    #   })
    # 
    # output$population <- renderValueBox({
    #   valueBox(
    #     value = filteredPopulation(),
    #     subtitle = "Total Area Population",
    #     icon = icon("user-check")
    #   )
    # })
    
    # output$popMap <- renderLeaflet({
    #   filteredArea() %>% 
    #     st_as_sf() %>% 
    #     leaflet() %>% 
    #     addProviderTiles("CartoDB.Positron") %>% 
    #     setView(lat = 40.7128, lng = -74.0060, zoom = 10) %>%
    #     addPolygons(
    #       fillColor = ~pal_pop(estimate),
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
    #         bringToFront = TRUE)#,
    #       # label = labels,
    #       # labelOptions = labelOptions(
    #       #   style = list("font-weight" = "normal", padding = "3px 8px"),
    #       #   textsize = "15px",
    #       #   direction = "auto")
    #     ) %>%
    #     addLegend(pal = pal_pop, values = ~estimate, opacity = 0.7, title = "Population",
    #               position = "bottomright")
    # })
    
    # output$resourceMap <- renderLeaflet({
    #   resource_sf %>% 
    #     leaflet() %>% 
    #     addProviderTiles("CartoDB.DarkMatter") %>% 
    #     leaflet::addMarkers()
    #     
    # })
    
    ## Third spot for looking at individual areas
    ### First we want to look at - for a given census tract the 
    ### other census tracts that are within an hour's travel time
    # output$trvlTimeMap <- renderLeaflet({
    #   nyc_just_geoid_geom_sf %>% filter(., GEOID == "36005000100") %>%
    #     as_tibble() %>%
    #     select(., GEOID) %>%
    #     inner_join(x=., y=nyc_trvl_times, by = c("GEOID" = "origin")) %>%
    #     filter(., minutes < 60) %>%
    #     select(., GEOID = destination, minutes) %>%
    #     inner_join(., ny_census_tracts_wo_water, by = "GEOID") %>%
    #     st_as_sf() %>%
    #     leaflet::leaflet() %>% addProviderTiles("CartoDB.DarkMatter") %>% addPolygons()
    #   
    # })
    
    # # map to show resource count within travel area
    # output$
})
