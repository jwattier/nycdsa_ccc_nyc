#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyServer(function(input, output, session) {
  
  # I. first page visualizations


  output$accessMap<- renderLeaflet({
    
    access_score_by_geoid <- access_score_by_geoid_eventReactive()

    nyc_census_tracts_opendatanyc %>%
      as_tibble() %>%
      left_join(x = ., y = access_score_by_geoid, by="GEOID") %>%
      filter(., category == input$select_category) %>%
      drop_na(category) %>% 
      # replace_na(., list(category = "", weighted_score = 0)) %>%
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
  
  # Visual to display table of percent allocaitons
  # # output$
  # Section to update the percentage splits
  perc_factor_tbl_reactive <- reactive({
    tibble(
      minutes_bin = tags,
      percent = c(input$first_bin, input$second_bin, input$third_bin, input$fourth_bin)
    )
  })
  
  
  output$perc_factor_trvl_time_bins <- DT::renderDataTable({
    DT::datatable(perc_factor_tbl_reactive())
  })
  
  access_score_by_geoid_eventReactive <- eventReactive(input$updateBttn, {
    perc_factor_tbl <- perc_factor_tbl_reactive()
    resource_ct_tbl <- resource_ct_file()  %>% 
      mutate(., resource_geoid = as.character(resource_geoid))
    
    just_geoids %>% 
      left_join(., nyc_trvl_times_adj, by = c("GEOID" = "origin")) %>%
      left_join(., resource_ct_tbl, by = c("destination" = "resource_geoid")) %>%
      left_join(., perc_factor_tbl, by = "minutes_bin") %>% 
      mutate(., weighted_score = if_else(is.na(minutes_bin),0, as.numeric(count * percent))) %>%
      group_by(GEOID, category) %>%
      summarise(weighted_score = sum(weighted_score))
  },
  # this helps to initialize this table instead of waiting for the user to clock on the Update Access Score
  # button
  ignoreNULL = FALSE
  )
  
  ### message notifcation for button click
  observeEvent(input$updateBttn,{
    sendSweetAlert(
      session = session,
      title = "Success!",
      text = "Access Score has been updated based upon new travel time factors",
      type = "success"
    )
  }
  )
  
  
  # II. Second page visualizations 
  
  ### Data for the exploratory tab
  # update PUMA code based upon boro
  observe({
    puma_codes <- unique(nyc_census_tracts_opendatanyc %>%
                          filter(nyc_census_tracts_opendatanyc$boro_name %in% input$borough) %>%
                            .$puma
                         )
    
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "puma",
      choices = puma_codes,
      selected = puma_codes[1]
    )  

  })
  
  # change the unique list of resources 
  observe({
    resource_categories <- unique(resource_ct_file() %>%
                           .$category
                           )
    
    updateSelectizeInput(
      session = session,
      inputId = "select_category",
      choices = resource_categories,
      selected = resource_categories[1]
    )  
    
  })
  
  # update census tracts based upon PUMA 
  observe({
    census_tracts <- unique(nyc_census_tracts_opendatanyc %>%
                              filter(nyc_census_tracts_opendatanyc$puma %in% input$puma) %>%
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
        filter(., puma %in% input$puma)
    })
    
    

    output$filteredMap <- renderLeaflet({
      filtered_population <- filteredArea() %>% as_tibble() %>%
        inner_join(x=., y=nyc_census_tract_population, by="GEOID")
      
      pal_population <- colorNumeric("magma", domain = filtered_population$estimate)
      
      labels <-sprintf(
        "Census Tract <strong>%s</strong><br/>%g number of people <br/>ACS 5-year 2018",
        filtered_population$GEOID, filtered_population$estimate
      ) %>% lapply(htmltools::HTML)
      
      filtered_population %>% 
        st_as_sf() %>%
        leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal_population(estimate),
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
            direction = "auto")
        ) %>%
      addLegend(pal = pal_population, values = ~estimate, opacity = 0.7, title = "Population",
                position = "bottomright")
      })
    
    
    # # map to show resources within the puma area selected
    
    output$filteredAccessMap <- renderLeaflet({
      access_score_by_geoid <- access_score_by_geoid_eventReactive()
      
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
    
    ### Section for Analysis Attempt
    output$scatter_plot <- renderPlot({
      access_score_by_geoid <- access_score_by_geoid_eventReactive()
      
      filteredArea() %>% as_tibble() %>%
        inner_join(x=., y=nyc_census_tract_population, by="GEOID") %>% 
        inner_join(x=., y=access_score_by_geoid, by="GEOID") %>% 
        rename(., population = estimate, access_score = weighted_score, resource_category = category) %>% 
        filter(., !is.na(access_score), !is.na(resource_category)) %>% 
        ggplot(., mapping = aes(x = population, y = access_score, color = resource_category, group = resource_category)) +
        geom_point() +
        geom_smooth(method="lm", se = FALSE) +
        ggtitle("Analysis of Access Score vs Demographic Information") +
        xlab("Population") +
        ylab("Access Score") +
        theme(legend.title=element_blank())
    })
 
    # 
    # # Section for previewing the resource file upload
    # # reactive expression to wait for upload and then render Table to display results
 
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    resource_input_data <- reactive({

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
    
    ### message notifcation for button click
    observeEvent(input$resource_file,{
        sendSweetAlert(
        session = session,
        title = "Success!",
        text = "Resource File Has Been Uploaded",
        type = "success"
        )
      }
    )
    
    ### update column options for selecting name, longitude, and latitude 
    observe({
      new_resource_data <- resource_input_data()
      file_columns <- colnames(new_resource_data)
 
      updateSelectizeInput(
        session = session,
        inputId = "new_resource_category",
        choices = file_columns,
        selected = file_columns[1]
      )
           
      updateSelectizeInput(
        session = session,
        inputId = "new_resource_description",
        choices = file_columns,
        selected = file_columns[1]
      )
      
      updateSelectizeInput(
        session = session,
        inputId = "new_resource_longtitude",
        choices = file_columns,
        selected = file_columns[1]
      )
      
      updateSelectizeInput(
        session = session,
        inputId = "new_resource_latitude",
        choices = file_columns,
        selected = file_columns[1]
      )
    })
    
    # 
    output$new_resource_contents <- renderTable({
      head(resource_input_data())

    })
    
    ### parse new file for purpose of displaying on map
    new_resource_processed <- reactive({
      resource_input_data() %>%
        mutate(locations = stringr::str_locate_all(Description, ":")) %>% 
        unnest_wider(locations, c(1)) %>% 
        select(., Location, Latitude, Longitude, Category, first_index = locations11, second_index = locations12, Description) %>% 
        mutate(Name = str_trim(str_sub(.$Description, start= first_index + 1, end= second_index - first_index))) %>% 
        mutate(Name_Plus = paste(Location, Name, sep = " : "))
    })
    
    output$new_resource_map <- leaflet::renderLeaflet({
      new_resource_processed() %>% 
      # resource_input_data() %>% 
        leaflet() %>% 
        addTiles() %>% 
        addMarkers(
          clusterOptions = markerClusterOptions,
          label = ~htmlEscape(Name_Plus)
        )
    })
    # 
    # 
    # # Section for displaying cumulative resource information
    # 
    observeEvent(input$addResource, {
      
      #1 Add resource to resource table
      input_table <- resource_input_data()
      resource_file <- read_resource_file()

      new_resource_tbl <- add_resource(
        new_resource_tbl = input_table, 
        name_col = input$new_resource_description, 
        category_col = input$new_resource_category, 
        geometry_col = "latlong", current_resource_tbl = resource_file)
      
      update_resource_file(resource_input = new_resource_tbl)
      
      #2 Update resource count by geo id
      resource_ct_tbl <- read_resource_count_file()
      
      new_resource_ct_by_geoid <- update_resource_ct_sf(current_resource_ct_table = resource_ct_tbl, 
                                                        new_resource_sf = new_resource_tbl,
                                                        census_geo_sf = nyc_just_geoid_geom_sf, 
                                                        resource_category = input$new_resource_category,
                                                        travel_time_cutoff = 60)
      
      update_resource_count_file(resource_count_input = new_resource_ct_by_geoid)

      #3 Update Access Score Table
      
      new_access_score_tbl <- update_access_calc_tbl(
        current_accesss_score_tbl = access_score_by_geoid,
        new_resource_category = input$new_resource_category,
        resource_ct_tbl = new_resource_ct_by_geoid
      )
      
      update_access_score_file(access_score_input =  new_access_score_tbl)
      
      ### message notification for button click
      show_alert(
        title = "Success!",
        text = "New Resource has been added to the analysis!",
        type = "success"
      )
      
      })
    
    resource_file <- reactiveFileReader(1000, session, "./resources/resource_list.geojson", sf::st_read)
     
    output$asset_listing <- DT::renderDataTable({
      # resource_file <- read_resource_file()
      
      DT::datatable(resource_file())
      
      
      
    })
    
    
    # resource_ct_file <- reactiveFileReader(1000, session, "./resources/resource_count_by_geo.csv", readr::read_csv)
    resource_ct_file <- reactiveFileReader(1000, session, "./resources/resource_count_by_geo.csv", readr::read_csv)
    
    
    output$resource_count_by_geo <- DT::renderDataTable({
      DT::datatable(resource_ct_file())
    })
    
    
    
    access_score_file <- reactiveFileReader(1000, session, "./resources/access_score_by_geo.csv", readr::read_csv)
    
    access_score_file_cleaned <- reactive({
      access_score_file() %>% 
        mutate(., GEOID = as.character(GEOID)) %>%
        filter(., !is.na(category) == TRUE) %>% as_tibble()
    })
    
    output$access_score_by_geo <- DT::renderDataTable({
      DT::datatable(access_score_file_cleaned())
    })
    
######### DOWNLOAD FILE SECTION #########
    
    ### Resource Location Download
    output$downloadResourceList <- downloadHandler(
      filename = function(){
        paste("Resource List ",as.character(lubridate::today()), ".geojson", sep = "")
      },
      content = function(file){
       sf::st_write(resource_file(), file)
      }
    )
    
    ### Resource Count by GeoID Download
    output$downloadResourceCtbyGEOID <- downloadHandler(
      filename = function(){
        paste("Resource Count by GEOID ",as.character(lubridate::today()), ".csv", sep = "")
      },
      content = function(file){
        write_csv(resource_ct_file(), file)
      }
    )
    
    ### Access Score Download
    output$downloadAccessScore <- downloadHandler(
      filename = function(){
        paste("Access Score ",as.character(lubridate::today()), ".csv", sep = "")
      },
      content = function(file){
        write_csv(access_score_file_cleaned(), file)
      }
    )
    
    combined_output_file <- reactive({
      
      access_score_by_geoid <- access_score_by_geoid_eventReactive()
      
      nyc_census_tracts_opendatanyc %>% 
        as_tibble() %>% 
        select(., boro_name, fips_country_code, ntaname, ntacode, puma_code = puma,  census_tract_nbr = GEOID) %>% 
        left_join(x = ., y = access_score_by_geoid, by="GEOID") %>% 
        drop_na(category)
    })
    
    output$downloadCombinedOutputFile <- downloadHandler(
      filename = function(){
        paste("Combined GEO Info w Access Score ",as.character(lubridate::today()), ".csv", sep = "")
      },
      content = function(file){
        write_csv(combined_output_file(), file)
      }
    )

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
            direction = "auto")
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
            direction = "auto")
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
    
    output$histogram_accesss <- renderPlot({
        breaks <- c(0, 10, 20, 30, 40, 50, 60)
        tags <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60 mins")
        # nyc_census_tracts_opendatanyc %>% 
        #   as_tibble() %>% select(., GEOID) %>% 
        #   filter(., GEOID == "36061024500") %>% 
        #   inner_join(., y=nyc_trvl_times_adj, by = c("GEOID" = "origin")) %>%
        #   left_join(., y=resource_ct_by_geoid, by = c("destination" = "resource_geoid")) %>% 
        isochrone_w_ct_reactive() %>% 
          mutate(.,  minutes_bin = cut(minutes, breaks = breaks, include.lowest = TRUE, 
                                       right = FALSE, labels = tags)) %>% 
          ggplot(data = ., mapping = aes(x = minutes_bin, y = count)) + 
          geom_col(fill="#1380A1") +
          geom_hline(yintercept = 0, size=1, colour = "#333333") +
          # bbc_style() +
          labs(
            title = "# of Resources by Travel Time"
          )
        }
        )
    
    # output$numberOfResourcesBox <- renderValueBox({
    #   isochrone_w_ct_info <- isochrone_w_ct_reactive()
    #   
    #   sum_total <- sum(isochrone_w_ct_info$count)
    #   
    #   valueBox(
    #     value = sum_total, subtitle = "Number of Resources w/in an Hour", color = "orange"
    #   )
    # })
    # 
    # access_score <- reactive({
    #   access_score_by_geoid %>% filter(., GEOID == input$census_area, category == input$select_category) %>% select(., weighted_score)
    # })
    # 
    # output$accessScoreBox <- renderValueBox({
    #   valueBox(
    #     value = access_score(), subtitle = "Access Score", color = "blue"
    #   )
    # })
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
