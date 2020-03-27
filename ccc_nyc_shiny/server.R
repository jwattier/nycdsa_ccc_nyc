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


  output$accessMap<- renderLeaflet({

    nyc_census_tracts_opendatanyc %>%
      as_tibble() %>%
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
    #     # label = labels,
    #     # labelOptions = labelOptions(
    #     #   style = list("font-weight" = "normal", padding = "3px 8px"),
    #     #   textsize = "15px",
    #     #   direction = "auto")
      ) %>%
      addLegend(pal = pal, values = ~weighted_score, opacity = 0.7, title = "Access Score",
                position = "bottomright")
    })
  
  # II. Second page visualizations 
  
  ### Data for the exploratory tab
  # update PUMA code based upon boro
  observe({
    puma_codes <- unique(nyc_census_tracts_opendatanyc %>%
                          filter(nyc_census_tracts_opendatanyc$boro_name == input$borough) %>%
                            .$puma)
    

    updateSelectizeInput(
      session = session,
      inputId = "puma",
      choices = puma_codes,
      selected = puma_codes[1]
    )


  })
  
  # update census tracts based upon PUMA 
  observe({
    census_tracts <- unique(nyc_census_tracts_opendatanyc %>%
                              filter(nyc_census_tracts_opendatanyc$puma == input$puma) %>%
                              .$GEOID)
    updateSelectizeInput(
      session = session,
      inputId = "census_area",
      choices = census_tracts,
      selected = census_tracts[1]
    )
  })
  
    
    filteredArea <- reactive({
      nyc_census_tracts_opendatanyc %>%
        filter(., puma == input$puma)
    })
    
    
    
    # filteredResourcePalette <- reactive({
    #   filtered_resource_ct <- filteredArea() %>% as_tibble() %>% select(., GEOID) %>% 
    #     inner_join(x=., y = resource_ct_geoid_sf, by = c("GEOID" = "resource_geoid"))
    #   
    #   colorNumeric("plasma", domain = filtered_resource_ct$count)
    #     
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


    #    
    # output$filteredMap <- renderLeaflet({
    #   # filteredArea() %>% as_tibble() %>% 
    #   #   inner_join(x=., y=nyc_census_tract_population, by="GEOID") %>% 
    #   #   st_as_sf() %>% 
    #   #   leaflet() %>%
    #   #   addProviderTiles("CartoDB.Positron") %>%
    #   #   addPolygons(
    #   #     fillColor = ~pal_pop(estimate),
    #   #     stroke = FALSE,
    #   #     weight = 2,
    #   #     opacity = 1,
    #   #     color = "white",
    #   #     dashArray = "3",
    #   #     fillOpacity = 0.7,
    #   #     highlight = highlightOptions(
    #   #       weight = 5,
    #   #       color = '#666',
    #   #       dashArray = "",
    #   #       fillOpacity = 0.7,
    #   #       bringToFront = TRUE)#,
    #   #     # label = labels,
    #   #     # labelOptions = labelOptions(
    #   #     #   style = list("font-weight" = "normal", padding = "3px 8px"),
    #   #     #   textsize = "15px",
    #   #     #   direction = "auto")
    #   #   ) %>%
    #   # addLegend(pal = pal_pop, values = ~estimate, opacity = 0.7, title = "Population",
    #   #           position = "bottomright")
    # })
    # 
    # # map to show resources within the puma area selected
    # # class(resource_ct_geoid_sf)
    # # colnames(resource_ct_geoid_sf)
    output$filteredAccessMap <- renderLeaflet({
      filteredArea() %>%  as_tibble() %>% #filter(., estimate != 0) %>%
        left_join(x = ., y = access_score_by_geoid, by="GEOID") %>%
        filter(., category == input$select_category) %>%
        replace_na(., list(category = "", weighted_score = 0)) %>%
        st_as_sf() %>%
        leaflet() %>%
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
    
    output$resourceMap <- renderLeaflet({
      
      filteredArea_w_resource_ct <- filteredArea() %>% as_tibble() %>% select(., GEOID) %>%
        inner_join(x=., y = resource_ct_by_geoid, by = c("GEOID" = "resource_geoid")) %>%
        filter(., category == input$select_category) %>% select(., GEOID, count) %>%
        inner_join(x=., y=nyc_census_tracts_opendatanyc, by="GEOID") 
      
      
      pal_resource <- colorNumeric("Blues", domain = filteredArea_w_resource_ct$count)

      filteredArea_w_resource_ct %>% 
        st_as_sf() %>%
        leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal_resource(count),
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
            bringToFront = TRUE))%>%
        addLegend(pal = pal_resource, values = ~count, opacity = 0.7, title = "Resource Count",
                      position = "bottomright")
     })
    # 
    # # map to show the area outside of the community district that can access its communal resources
    output$expand_cov_map <- renderLeaflet({
      pal_minutes <- colorNumeric("Reds", domain = c(0, 60))
      
      filteredArea() %>% as_tibble() %>% select(., GEOID) %>%
        inner_join(x=., y = resource_ct_by_geoid, by = c("GEOID" = "resource_geoid")) %>%
        filter(., category == input$select_category, count != 0) %>% 
        select(., GEOID, count) %>%
        inner_join(., y= nyc_trvl_times, by=c("GEOID" = "destination")) %>%
        filter(., minutes <= 60) %>%
        group_by(origin) %>% 
        summarise(minutes = min(minutes)) %>% 
        inner_join(x=., y=nyc_census_tracts_opendatanyc, by = c("origin" = "GEOID")) %>%
        st_as_sf() %>%
        leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal_minutes(minutes),
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
            bringToFront = TRUE)
        )%>%
        addLegend(pal = pal_minutes, values = ~minutes, opacity = 0.7, title = "Travel Time (Minutes)",
                  position = "bottomright")
    })
 
    # 
    # # Section for previewing the resource file upload
    # # reactive expression to wait for upload and then render Table to display results
    # 
    # resource_input_data <- reactive({
    # #   # input$file1 will be NULL initially. After the user selects
    # #   # and uploads a file, it will be a data frame with 'name',
    # #   # 'size', 'type', and 'datapath' columns. The 'datapath'
    # #   # column will contain the local filenames where the data can
    # #   # be found.
    # #   req(input$resource_file)
    # #   
    # #   inFile <- input$resource_file
    # #   
    # #   ext <- reader::get.ext(inFile$datapath)
    # #   
    # #   switch(ext,
    # #     "csv" = readr::read_csv(file = inFile$datapath),
    # #     "xls" = readxl::read_excel(path = inFile$datapath),
    # #     "xlsx" = readxl::read_excel(path = inFile$datapath),
    # #     validate("Invalid file; Please upload a .csv, .xls, or .xlsx file")
    # #   )
    # })
    # 
    # output$new_resource_contents <- renderTable({
    # #   head(resource_input_data())
    # #   
    # })
    # 
    # 
    # # Section for displaying cumulative resource information
    # 
    # resource_tbl <- eventReactive(input$upload_resource, {
    # #   input_table <- resource_input_data()
    # #   
    # #   add_resource(
    # #     new_resource_tbl = input_table, name_col = "Location", type_col = "Category", capacity_amt_col = NA,
    # #     capacity_unit_col = NA, geom_col = "latlong", current_resource_tbl = resource_sf)
    # })
    # 
    # output$asset_listing <- DT::renderDataTable({
    # #   #DT::datatable(resource_sf)
    # #   if(is_empty(resource_tbl())){
    # #     DT::datatable(resource_sf)
    # #   } else{
    # #   DT::datatable(resource_tbl())
    # #     }
    # })
    # 
    # resource_ct_by_geoid_tbl <- reactive({
    #   # new_category <- resource_input_data() %>% .$Category %>% unique() 
    #   # 
    #   # new_resource_tbl <- resource_tbl()
    #   # 
    #   # update_resource_ct_sf(current_resource_ct_table = resource_ct_by_geoid, new_resource_sf = new_resource_tbl,
    #   #                       census_geo_sf = nyc_just_geoid_geom_sf, resource_category = new_category,
    #   #                       travel_time_cutoff = 60)
    # })
    # 
    # output$resource_ct_tbl <- DT::renderDataTable({
    #   # resource_ct_by_geoid_tbl()
    # })
    # 
    # output$resource_point_map <- renderLeaflet({
    #   # resource_tbl() %>% leaflet() %>% addTiles() %>% 
    #   #   addMarkers(
    #   #     clusterOptions = markerClusterOptions()
    #   #   )
    # })
    # 
    # access_score_by_geoid <- reactive({
    #   # new_category <- resource_input_data() %>% .$Category %>% unique()
    #   # 
    #   # new_resource_ct_info <- resource_ct_by_geoid_tbl()
    #   # 
    #   # update_access_calc_tbl(current_accesss_score_tbl = access_score_by_geoid, new_resource_category = new_category,
    #   #                        new_resource_ct_tbl = new_resource_ct_info, travel_time_cutoff = 60)
    # })
    # 
    # output$access_score_tbl <- DT::renderDataTable({
    #   # access_score_by_geoid()
    # })
    # 
    # ## Third spot for looking at individual areas
    # ### First we want to look at - for a given census tract the 
    # ### other census tracts that are within an hour's travel time
  zone_around_census_tract <- reactive({
    nyc_census_tracts_opendatanyc %>% filter(., GEOID == input$census_area) %>%
      as_tibble() %>%
      select(., GEOID) %>%
      inner_join(x=., y=nyc_trvl_times, by = c("GEOID" = "origin")) %>%
      filter(., minutes <= 60) %>%
      select(., GEOID = destination, minutes) %>%
      inner_join(.,  nyc_census_tracts_opendatanyc, by = "GEOID")
  })
  
    output$trvlTimeMap <- renderLeaflet({
      
      zone_around_census_tract_tbl <- zone_around_census_tract()
      
      origin_point <- input$census_area
      
      labels <-sprintf(
        "Census Tract <strong>%s</strong><br/>%g minutes from origin",
        zone_around_census_tract_tbl$GEOID, zone_around_census_tract_tbl$minutes
      ) %>% lapply(htmltools::HTML)
      
      pal_minutes <- colorNumeric("Reds", domain = c(0, 60))
      
      zone_around_census_tract_tbl %>% 
        st_as_sf() %>%
        leaflet::leaflet() %>% addProviderTiles("CartoDB.Positron") %>%  
        addPolygons(
          fillColor = ~pal_minutes(minutes),
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
              bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction)
          )%>%
        addLegend(pal = pal_minutes, values = ~minutes, opacity = 0.7, title = "Travel Time (Minutes)",
                  position = "bottomright")
    })
    
    output$trvlTimeDT <- DT::renderDataTable({
      #columns_to_subset <- c("GEOID", "minutes", "puma", "ntacode", "ntaname", "boro_name")
      zone_around_census_tract_tbl <- zone_around_census_tract() %>% 
        select(., `Census Tract` = GEOID, Minutes = minutes, Puma = puma, 'NTA Code' = ntacode, 
               'NTA Name' = ntaname, Borough = boro_name)
      
      DT::datatable(zone_around_census_tract_tbl)
    })

    isochrone_w_ct_reactive <- reactive({
      resources_ct_tbl <- resource_ct_by_geoid %>% filter(., category == input$select_category)

      zone_around_census_tract() %>%
        inner_join(., resources_ct_tbl, by = c("GEOID" = "resource_geoid"))
    })
    
    
    output$resources_within_travel_time <- renderLeaflet({
      resource_category <- input$select_category
      
      resources_ct_tbl <- resource_ct_by_geoid %>% filter(., category == input$select_category) 
      
      isochrone_w_ct <- zone_around_census_tract() %>% 
        inner_join(., resources_ct_tbl, by = c("GEOID" = "resource_geoid"))
      
      
      pal_resources <- colorNumeric("Blues", domain = isochrone_w_ct$count)
      
      labels <-sprintf(
        "Census Tract <strong>%s</strong><br/> has %s of resource category %s",
        isochrone_w_ct$GEOID, isochrone_w_ct$count, resource_category
      ) %>% lapply(htmltools::HTML)
      
      isochrone_w_ct %>%  
        st_as_sf() %>%
        leaflet::leaflet() %>% addProviderTiles("CartoDB.DarkMatter") %>%  
        addPolygons(
          fillColor = ~pal_resources(count),
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
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction)
        )%>%
        addLegend(pal = pal_resources, values = ~count, opacity = 0.7, title = "Resource Count",
                  position = "bottomright")
    })
    
    output$resources_within_travel_time_DT <- DT::renderDataTable({
      resource_count_around_census_tract <- isochrone_w_ct_reactive() %>% 
        select(., `Census Tract` = GEOID, `Resource Count` = count, `Resource Category` = category,
               Puma = puma, 'NTA Code' = ntacode, 'NTA Name' = ntaname, Borough = boro_name)
      
      DT::datatable(resource_count_around_census_tract)
    })
    
    
        # 
    # 
    # 
    # 
    # # # output$
    # output$access_score_detail <- DT::renderDataTable({
    # #   DT::datatable(weighted_score_table )
    # })
    # # 
    # output$access_score_chart <- renderPlot(
    # #   access_score_by_geoid %>% 
    # #     ggplot(., mapping = aes(x = weighted_score, color = GEOID, alpha = GEOID, fill=GEOID)) + 
    # #     geom_histogram() +
    # #     scale_color_focus("36005000100", color_focus = "red",color_other = "black") +
    # #     scale_fill_focus("36005000100", color_focus = "red", color_other = "black") +
    # #     scale_alpha_focus("36005000100") + 
    # #     ggtitle("Chart of Weighted score by GEOID")
    # )
})
