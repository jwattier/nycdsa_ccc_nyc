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
            menuItem(text = "Explore", tabName = "explore", icon = icon("chart-area")),

            selectizeInput(inputId="borough",
                           label="Borough:",
                           choices = unique(nyc_just_geoid_geom_sf$borough_name)),
            selectizeInput(inputId="nta",
                           label="NTA:",
                           choices = unique(nyc_just_geoid_geom_sf$nta_code))
            )
        ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "map",
                    fluidRow(
                        box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Access Score NYC", 
                            leafletOutput("accessMap", width = "100%", height = 600)
                        ),
                        box(
                            # Output is a display of the allocations percentages should that option be selected
                        width = 4, status = "info", title = "Percent Splits for Overall Score",
                        DT::dataTableOutput("perc_table")
                        )
                        )
                    ),
            tabItem(tabName = "explore",
                    fluidRow(
                        valueBoxOutput("population")
                    ),
                    fluidRow(
                        box(
                            leafletOutput("popMap")
                            )
                        )
                    )
            )
            )
        )
)
