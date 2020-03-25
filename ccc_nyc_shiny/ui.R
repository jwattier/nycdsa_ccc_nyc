#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = "CCC NYC Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(text = "Map", tabName = "map", icon = icon("map")),
            selectizeInput(inputId="select_category",
                           label="Resource Category:",
                           choices = resouse_categories),
            #,
            menuItem(text = "Explore", tabName = "explore", icon = icon("chart-area")),
            selectizeInput(inputId="puma",
                            label="Puma:",
                            choices = unique(nyc_census_tracts_opendatanyc$puma)),
            # selectizeInput(inputId="nta",
            #                label="NTA:",
            #                choices = unique(nyc_just_geoid_geom_sf$nta_code)),
            menuItem(text = "Deep Dive", tabName = "deep_dive", icon = icon("chart-area"))
            )
        ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "map",
                    fluidRow(
                        box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Access Score NYC", 
                            leafletOutput("accessMap", width = "100%", height = 600)
                        )),
                    br(),
                    fluidRow(
                        box(
                            # Output is a display of the allocations percentages should that option be selected
                        width = 12, status = "info",solidHeader = TRUE,
                        title = "Percent Weight for Resources",
                        DT::dataTableOutput("perc_table")
                        )
                        )
                    ),
                    # fluidRow(
                    #     box(
                    #         # Output is a display of the allocations percentages should that option be selected
                    #     width = 8, status = "info",solidHeader = TRUE,
                    #     title = "Percent Weight for Resources",
                    #     DT::dataTableOutput("perc_table")
                    #     ))
                    # )
            tabItem(tabName = "explore",
                    # fluidRow(
                    #     valueBoxOutput("population")
                    # ),
                    fluidRow(
                        box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Demographic Information for PUMAs",
                            leafletOutput("filteredMap")
                            )
                        )
                    ,
                    fluidRow(
                        box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Resource Info for PUMAs",
                            leafletOutput("resourceMap")
                        )
                    )
                    ),
            tabItem(tabName = "deep_dive",
                    fluidRow(
                        box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Travel Time Radius",
                            leafletOutput("trvlTimeMap", width = "100%", height = 600)
                        )
                    ),
                    br(),
                    fluidRow(
                        box(
                            # Output is a display of the allocations percentages should that option be selected
                            width = 12, status = "info",solidHeader = TRUE,
                            title = "Access Score Detail",
                            DT::dataTableOutput("access_score_detail")
                        )                     
                    ),
                    br(),
                    fluidRow(
                        box(
                            # Output is a display of the allocations percentages should that option be selected
                            width = 12, status = "info",solidHeader = TRUE,
                            title = "Access Score Chart",
                            plotOutput("access_score_chart")
                        )   
                    ))
            )
            )
        )
)

