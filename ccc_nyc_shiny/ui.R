#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#parent_path = "./ccc_nyc_shiny/"
parent_path = "./"

source(paste0(parent_path, "helpers.R"))

shinyUI(
  dashboardPagePlus(
    title = "CCC New York Analysis Dashboard",
    dashboardHeaderPlus(
      title = "CCC NYC Analysis",
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "gears",
      left_menu = tagList(
        dropdownBlock(
          id = "mydropdown",
          title = "Travel Time Weighting Factors",
          icon = icon("globe"),
          actionButton("updateBttn", "Update Access Score"),
          sliderInput("first_bin", "0 to 15 Mins Factor", min = 0, max = 1, 
                      value = 1, step = 0.10, round = FALSE, ticks = TRUE
          ),
          sliderInput("second_bin", "15 to 30 Mins Factor", min = 0, max = 1, 
                      value = 1, step = 0.10, round = FALSE, ticks = TRUE
          ),
          sliderInput("third_bin", "30 to 45 Mins Factor", min = 0, max = 1, 
                      value = 1, step = 0.10, round = FALSE, ticks = TRUE
          ),
          sliderInput("fourth_bin", "45 to 60 Mins Factor", min = 0, max = 1, 
                      value = 1, step = 0.10, round = FALSE, ticks = TRUE
          )
        ),
        dropdownBlock(
          id = "downloadDropDown",
          title = "Download Files",
          icon = icon("globe"),
          downloadBttn("downloadResourceList", "Download Resource List"),
          downloadBttn("downloadResourceCtbyGEOID", "Download Resource Count by GEOID"),
          downloadBttn("downloadAccessScore", "Download Access Data"),
          downloadBttn("downloadCombinedOutputFile", "Download Combined Geo and Access Score Data")
          )
        )
      ),
    dashboardSidebar(
        sidebarMenu(
          menuItem(text = "Map", tabName = "map", icon = icon("map")),
          menuItem(text = "Explore", tabName = "explore", icon = icon("chart-area")),
          menuItem(text = "Resource Upload", tabName = "resource_upload", icon=icon("file")),
          menuItem(text = "Resource Inventory", tabName = "asset_inventory", icon = icon("map")),
          menuItem(text = "Deep Dive", tabName = "deep_dive", icon = icon("chart-area")),
          shinyWidgets::pickerInput(
            inputId = "borough",
            label = "Borough:",
            selected = unique(nyc_census_tracts_opendatanyc$boro_name),
            choices = unique(nyc_census_tracts_opendatanyc$boro_name),
            multiple = TRUE,
            options = list(`actions-box` = TRUE,
                           `select-all-text` = "All Boroughs")
          ),
          shinyWidgets::pickerInput(
            inputId = "puma",
            label = "PUMA:",
            selected = unique(nyc_census_tracts_opendatanyc$puma)[1],
            choices = unique(nyc_census_tracts_opendatanyc$puma),
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          selectizeInput(inputId="census_area",
                         label="Census Tract:",
                         choices = sort(unique(nyc_census_tracts_opendatanyc$GEOID))
                         ),
          selectizeInput(inputId="select_category",
                         label="Resource Category:",
                         choices = resouse_categories
                         )
        )
      ),
    dashboardBody(
      tabItems(
            tabItem(tabName = "map",
                    fluidPage(
                          box(
                            width = 12, status = "info", solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Access Score for NYC",
                            leafletOutput("accessMap", width = "100%", height = 600)
                        ))
                    ),
            tabItem(tabName = "resource_upload",
                    fluidPage(
                      tabBox(
                        title = "New Asset", width = 12,
                        tabPanel("File Upload",
                          fileInput(
                          "resource_file", 
                          "Choose CSV or Excel file",
                          multiple = FALSE,
                          accept = c("text/csv", 
                                     "text/comma-separated-values, text/plain",
                                     ".csv", ".xlsx", ".xls")
                          ),
                          shinyWidgets::useSweetAlert(),
                          #  textInput("new_resource_category", "Resource Label:"),
                          selectizeInput(
                            "new_resource_category",
                            "Resource Category Column:",
                            choices = NULL,
                            selected = NULL
                                         ),
                          selectizeInput(
                            "new_resource_description", 
                            "Resource Name/Description Column:",
                            choices = NULL,
                            selected = NULL
                            ),
                          selectizeInput(
                            "new_resource_longtitude", 
                            "Longitude Column:",
                            choices = NULL,
                            selected = NULL
                          ),
                          selectizeInput(
                            "new_resource_latitude", 
                            "Latitude Column:",
                            choices = NULL,
                            selected = NULL
                          ),
                          actionButton("addResource", "Add Resource to Resource Table"),
                          tableOutput("new_resource_contents")
                          ),
                        tabPanel("New Resource Map",
                          leafletOutput("new_resource_map")
                        )
                      )
                    )
            ),
            tabItem(tabName = "asset_inventory",
                    fluidPage(
                      tabsetPanel(
                        type = "tabs",
                        ## title = "Asset Information", width = 12,
                        tabPanel("Asset Listing", 
                                 DT::dataTableOutput("asset_listing"),
                                 ),
                        tabPanel("Resource by Census Tract", 
                                 DT::dataTableOutput("resource_count_by_geo"),
                                 ),
                        # tabPanel("Access Score by Census Tract",
                        #          fluidPage(
                        #            DT::dataTableOutput("access_score_by_geo"),
                        #            downloadBttn("downloadAccessDate", "Download Access Data")
                        #            )
                        #          ),
                        tabPanel(  "Access Score by Census Tract",
                                  
                        # dropdownButton(
                        #   downloadBttn("downloadAccessScore", "Download Access Data"),
                        #   
                        #      
                        #   circle = TRUE, status = "primary",
                        #   icon = icon("gear"), width = "300px",
                        #   
                        #   tooltip = tooltipOptions(title = "Click to download Access Score Data")
                        #     ),
                          DT::dataTableOutput("access_score_by_geo"),
                          )
                        )
                      )
                    ),
            tabItem(tabName = "per_split",
                    fluidPage(
                      box(
                        width = 12, status = "info", solidHeader = TRUE,
                        collapsible = FALSE,
                        title = "Percent Factor for Travel Time Bins",
                        DT::dataTableOutput("perc_factor_trvl_time_bins")
                      ))
              
            ),

            tabItem(tabName = "access_score_by_geoid",
                    fluidPage(
                      DT::dataTableOutput("access_score_tbl")
                    )),
            tabItem(tabName = "explore",
        #             # fluidRow(
        #             #     valueBoxOutput("population")
        #             # ),
                    fluidRow(
                        box(
                            width = 12, status = "info", solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Access Score for PUMA",
                            leafletOutput("filteredAccessMap")
                        )
                    ),
                    fluidRow(
                        box(
                            width = 12, status = "info", solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Demographic Information for PUMA",
                            leafletOutput("filteredMap")
                            )
                        )
                    ,
                    fluidRow(
                        box(
                            width = 12, status = "info", solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Resource Info for PUMA",
                            leafletOutput("resourceMap")
                        )
                    ),
                    fluidRow(
                        box(
                            width = 12, status = "info", solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Coverage Area for Assets in PUMA",
                            leafletOutput("expand_cov_map")
                            )
                        )
                    ),
        tabItem(tabName = "demogrph_analysis",
          fluidPage(
            box(
              width = 12, status = "info", solidHeader = TRUE,
              collapsible = TRUE,
              title = "Demographic Analysis - Citywide",
              plotOutput("scatter_plot"))
            )
          ),
      
            tabItem(tabName = "deep_dive",
                    # fluidRow(
                    #   valueBoxOutput("accessScoreBox"),
                    #   valueBoxOutput("numberOfResourcesBox")
                    # ),
                    fluidRow(
                        # box(
                        #     width = 12, status = "info", solidHeader = TRUE,
                        #     collapsible = TRUE,
                        #     title = "Travel Time Radius",
                        #     leafletOutput("trvlTimeMap", width = "100%", height = 600)
                        # )
                      box(
                        width = 12, status = "info", solidHeader = TRUE,
                        collapsible = TRUE,
                        title = "Travel Time within an Hour",
                        tabsetPanel(type = "pills",
                          tabPanel("Map",
                                  # h5("Represents census areas that are within an hour's travel time of the origin census selected."),
                            leafletOutput("trvlTimeMap")
                            ),
                          tabPanel("Data",
                                   DT::dataTableOutput("trvlTimeDT")
                                   )
                          )
                        )
                    ),
                    br(),
                    fluidRow(
                      box(
                        width = 12, status = "info", solidHeader = TRUE,
                        collapsible = TRUE,
                        title = "Resources within an Hour",
                        tabsetPanel(type = "pills",
                          tabPanel("Map",
                            leafletOutput("resources_within_travel_time")
                          ),
                          tabPanel("Data",
                                   DT::dataTableOutput("resources_within_travel_time_DT")
                                   )
                        )
                          )
                        ),
                    br(),
                    fluidRow(
                      box(
                        width = 12, status = "info", solidHeader = TRUE,
                        collapsible = TRUE,
                        title = "Resource Count by Travel Time Bucket",
                        plotOutput("histogram_accesss")
                      )
                    )
                      )
                    )
                    
        # ,
        #             br(),
        #             fluidRow(
        #                 box(
        #                     # Output is a display of the allocations percentages should that option be selected
        #                     width = 12, status = "info",solidHeader = TRUE,
        #                     title = "Access Score Detail",
        #                     DT::dataTableOutput("access_score_detail")
        #                 )                     
        #             ),
        #             br(),
        #             fluidRow(
        #                 box(
        #                     # Output is a display of the allocations percentages should that option be selected
        #                     width = 12, status = "info",solidHeader = TRUE,
        #                     title = "Access Score Chart",
        #                     plotOutput("access_score_chart")
        #                 )   
        #             ))
        #     )
             
        ))
)


