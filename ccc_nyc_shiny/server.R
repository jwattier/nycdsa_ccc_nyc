#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

weighted_score_table <- nyc_just_geoid_geom_sf %>% filter(., GEOID == "36005000100" ) %>%  as_tibble() %>%
  select(., GEOID) %>% inner_join(x=., y=nyc_trvl_times, by = c("GEOID" = "origin")) %>%
  filter(., minutes <= 60) %>% inner_join(x=., y=resource_ct_by_geoid, by = c("destination" = "resource_geoid")) %>%
  filter(., category == "food retail") %>% group_by(minutes) %>% summarise(., resource_count = sum(count)) %>%
  arrange(., desc(resource_count)) %>%
  mutate(., weighted_score = resource_count / minutes) %>% filter(., resource_count >0)

shinyServer(function(input, output, session) {
  
  # I. first page visualizations
  
  output$perc_table <- DT::renderDataTable({
    DT::datatable(category_percents)
    })

  output$accessMap<- renderLeaflet({
    #access_score_by_geoid_tbl <- access_score_by_geoid()
    
    # nyc_census_tracts_opendatanyc %>%
    #   as_tibble() %>% #filter(., estimate != 0) %>%
    #   left_join(x = ., y = access_score_by_geoid_tbl, by="GEOID") %>%
    #   filter(., category == input$select_category) %>%
    #   replace_na(., list(category = "", weighted_score = 0)) %>%
    #   st_as_sf() %>%
    #   leaflet() %>%
    #   setView(lat = 40.7128, lng = -74.0060, zoom = 10) %>%
    #   addProviderTiles("CartoDB.Positron") %>%
    #   addPolygons(
    #     fillColor = ~pal(weighted_score),
    #     stroke = FALSE,
    #     weight = 2,
    #     opacity = 1,
    #     color = "white",
    #     dashArray = "3",
    #     fillOpacity = 0.7,
    #     highlight = highlightOptions(
    #       weight = 5,
    #       color = '#666',
    #       dashArray = "",
    #       fillOpacity = 0.7,
    #       bringToFront = TRUE)#,
    #     # label = labels,
    #     # labelOptions = labelOptions(
    #     #   style = list("font-weight" = "normal", padding = "3px 8px"),
    #     #   textsize = "15px",
    #     #   direction = "auto")
    #   ) %>%
    #   addLegend(pal = pal, values = ~weighted_score, opacity = 0.7, title = "Access Score",
    #             position = "bottomright")
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
       
    output$filteredMap <- renderLeaflet({
      filteredArea() %>% as_tibble() %>% 
        inner_join(x=., y=nyc_census_tract_population, by="GEOID") %>% 
        st_as_sf() %>% 
        leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal_pop(estimate),
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
      addLegend(pal = pal_pop, values = ~estimate, opacity = 0.7, title = "Population",
                position = "bottomright")
    })
    
    # map to show resources within the puma area selected
    # class(resource_ct_geoid_sf)
    # colnames(resource_ct_geoid_sf)
    output$resourceMap <- renderLeaflet({
      # pal_fun <- filteredResourcePalette()
      # 
      filteredArea() %>% as_tibble() %>% select(., GEOID) %>% 
        inner_join(x=., y = resource_ct_by_geoid, by = c("GEOID" = "resource_geoid")) %>% 
        filter(., category == input$select_category) %>% select(., GEOID, count) %>% 
        inner_join(x=., y=nyc_census_tracts_opendatanyc, by="GEOID") %>% 
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
    
    # map to show the area outside of the community district that can access its communal resources
    output$expand_cov_map <- renderLeaflet({
      filteredArea() %>% as_tibble() %>% select(., GEOID) %>% 
        inner_join(x=., y = resource_ct_by_geoid, by = c("GEOID" = "resource_geoid")) %>% 
        filter(., category == input$select_category, count != 0) %>% select(., GEOID, count) %>%
        inner_join(., y= nyc_trvl_times, by=c("GEOID" = "destination")) %>% 
        filter(., minutes < 60) %>%  distinct(., origin) %>% 
        inner_join(x=., y=nyc_census_tracts_opendatanyc, by = c("origin" = "GEOID")) %>% 
        st_as_sf() %>% 
        leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        addPolygons(stroke = FALSE, fillColor = "blue") 
      
    })
    
    # Section for previewing the resource file upload
    # reactive expression to wait for upload and then render Table to display results
    
    resource_input_data <- reactive({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      req(input$resource_file)
      
      inFile <- input$resource_file
      
      ext <- reader::get.ext(inFile$datapath)
      
      switch(ext,
        "csv" = readr::read_csv(file = inFile$datapath),
        "xls" = readxl::read_excel(path = inFile$datapath),
        "xlsx" = readxl::read_excel(path = inFile$datapath),
        validate("Invalid file; Please upload a .csv, .xls, or .xlsx file")
      )
    })
    
    output$new_resource_contents <- renderTable({
      head(resource_input_data())
      
    })
    

    # Section for displaying cumulative resource information
    
    resource_tbl <- eventReactive(input$upload_resource, {
      input_table <- resource_input_data()
      
      add_resource(
        new_resource_tbl = input_table, name_col = "Location", type_col = "Category", capacity_amt_col = NA,
        capacity_unit_col = NA, geom_col = "latlong", current_resource_tbl = resource_sf)
    })

    output$asset_listing <- DT::renderDataTable({
      #DT::datatable(resource_sf)
      if(is_empty(resource_tbl())){
        DT::datatable(resource_sf)
      } else{
      DT::datatable(resource_tbl())
        }
    })
    
    resource_ct_by_geoid_tbl <- reactive({
      new_category <- resource_input_data() %>% .$Category %>% unique() 
      
      new_resource_tbl <- resource_tbl()
      
      update_resource_ct_sf(current_resource_ct_table = resource_ct_by_geoid, new_resource_sf = new_resource_tbl,
                            census_geo_sf = nyc_just_geoid_geom_sf, resource_category = new_category,
                            travel_time_cutoff = 60)
    })
    
    output$resource_ct_tbl <- DT::renderDataTable({
      resource_ct_by_geoid_tbl()
    })
    
    output$resource_point_map <- renderLeaflet({
      resource_tbl() %>% leaflet() %>% addTiles() %>% 
        addMarkers(
          clusterOptions = markerClusterOptions()
        )
    })
    
    access_score_by_geoid <- reactive({
      new_category <- resource_input_data() %>% .$Category %>% unique()
      
      new_resource_ct_info <- resource_ct_by_geoid_tbl()
      
      update_access_calc_tbl(current_accesss_score_tbl = access_score_by_geoid, new_resource_category = new_category,
                             new_resource_ct_tbl = new_resource_ct_info, travel_time_cutoff = 60)
    })
    
    output$access_score_tbl <- DT::renderDataTable({
      access_score_by_geoid()
    })
    
    ## Third spot for looking at individual areas
    ### First we want to look at - for a given census tract the 
    ### other census tracts that are within an hour's travel time
    output$trvlTimeMap <- renderLeaflet({
      nyc_census_tracts_opendatanyc %>% filter(., GEOID == "36005000100") %>%
        as_tibble() %>%
        select(., GEOID) %>%
        inner_join(x=., y=nyc_trvl_times, by = c("GEOID" = "origin")) %>%
        filter(., minutes < 60) %>%
        select(., GEOID = destination, minutes) %>%
        inner_join(.,  nyc_census_tracts_opendatanyc, by = "GEOID") %>%
        st_as_sf() %>%
        leaflet::leaflet() %>% addProviderTiles("CartoDB.Positron") %>% addPolygons()

    })
    
    

   
    # output$
    output$access_score_detail <- DT::renderDataTable({
      DT::datatable(weighted_score_table )
    })
    
    output$access_score_chart <- renderPlot(
      access_score_by_geoid %>% 
        ggplot(., mapping = aes(x = weighted_score, color = GEOID, alpha = GEOID, fill=GEOID)) + 
        geom_histogram() +
        scale_color_focus("36005000100", color_focus = "red",color_other = "black") +
        scale_fill_focus("36005000100", color_focus = "red", color_other = "black") +
        scale_alpha_focus("36005000100") + 
        ggtitle("Chart of Weighted score by GEOID")
    )
})
