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
library(shiny)


# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = "CCC NYC Analysis"),
    dashboardSidebar(
        sidebarMenu(
          selectizeInput(inputId="borough",
                         label="Borough:",
                         choices = unique(nyc_census_tracts_opendatanyc$boro_name)
          ),
          selectizeInput(inputId="puma",
                         label="PUMA:",
                         choices = unique(nyc_census_tracts_opendatanyc$puma)
                         ),
          selectizeInput(inputId="census_area",
                         label="Census Tract:",
                         choices = unique(nyc_census_tracts_opendatanyc$GEOID)
                         ),
            menuItem(text = "Map", tabName = "map", icon = icon("map")),
            selectizeInput(inputId="select_category",
                           label="Resource Category:",
                           choices = resouse_categories),
            #,
            menuItem(text = "Explore", tabName = "explore", icon = icon("chart-area")),
            menuItem(text = "Access Score", tabName = "access_score"),
            menuItem(text = "Asset Inventory", tabName = "asset_inventory"),
            menuItem(text = "Deep Dive", tabName = "deep_dive", icon = icon("chart-area"))
            )
        ),
    dashboardBody(
      tabItems(
        #     tabItem(tabName = "map",
        #             fluidRow(
        #                 box(
        #                     width = 12, status = "info", solidHeader = TRUE,
        #                     title = "Access Score NYC", 
        #                     leafletOutput("accessMap", width = "100%", height = 600)
        #                 ))
        #             ),
        #     tabItem(tabName = "resource",
        #                 fluidPage(
        #                   tableOutput("new_resource_contents")
        #                   )
        #             ),
        #             # fluidRow(
        #             #     box(
        #             #         # Output is a display of the allocations percentages should that option be selected
        #             #     width = 8, status = "info",solidHeader = TRUE,
        #             #     title = "Percent Weight for Resources",
        #             #     DT::dataTableOutput("perc_table")
        #             #     ))
        #             # )
        #     tabItem(tabName = "asset_inventory",
        #             fluidPage(
        #               tabBox(title = "Community Asset Info", id = "asset_tabset",
        #                      tabPanel("Asset List", DT::dataTableOutput("asset_listing")),
        #                      tabPanel("Asset Counts", DT::dataTableOutput("resource_ct_tbl")),
        #                      tabPanel("Asset Map", leafletOutput("resource_point_map"))
        #               )
        #             )),
        #     tabItem(tabName = "access_score_by_geoid",
        #             fluidPage(
        #               DT::dataTableOutput("access_score_tbl")
        #             )),
            tabItem(tabName = "explore",
        #             # fluidRow(
        #             #     valueBoxOutput("population")
        #             # ),
        #             fluidRow(
        #                 box(
        #                     width = 12, status = "info", solidHeader = TRUE,
        #                     title = "Access Score for PUMAs",
        #                     leafletOutput("filteredAccessMap")
        #                 )   
        #             ),
        #             fluidRow(
        #                 box(
        #                     width = 12, status = "info", solidHeader = TRUE,
        #                     title = "Demographic Information for PUMAs",
        #                     leafletOutput("filteredMap")
        #                     )
        #                 )
        #             ,
        #             fluidRow(
        #                 box(
        #                     width = 12, status = "info", solidHeader = TRUE,
        #                     title = "Resource Info for PUMAs",
        #                     leafletOutput("resourceMap")
        #                 )
        #             ),
                    fluidRow(
                        box(
                            width = 12, status = "info", solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Coverage Area for Assets in PUMAs",
                            leafletOutput("expand_cov_map")
                            )
                        )
                    ),
            tabItem(tabName = "deep_dive",
                    fluidRow(
                        # box(
                        #     width = 12, status = "info", solidHeader = TRUE,
                        #     collapsible = TRUE,
                        #     title = "Travel Time Radius",
                        #     leafletOutput("trvlTimeMap", width = "100%", height = 600)
                        # )
                      shinydashboardPlus::flipBox(
                        id = 1,
                        main_img = "New_York_City_community_districts.svg.png",
                        header_img = "New_York_City_community_districts.svg.png",
                        front_title = "Map Travel Time Radius",
                        back_title = "Table Travel Time Radius",
                        width = 12,
                        leafletOutput("trvlTimeMap", width = "100%", height = 600),
                        back_content = DT::dataTableOutput("trvlTimeDT")
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
                          tabPanel("Tab2",
                                   verbatimTextOutput("Test Output")
                                   )
                        )
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

