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
            menuItem(text = "Explore", tabName = "explore", icon = icon("chart-area")),
            selectizeInput(inputId="select_category",
                           label="Resource Category:",
                           choices = resouse_categories),
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
                    fluidPage(
                        h4("Heatmap of Accessiblity"),
                        leafletOutput("accessMap"),
                        br(),
                        # Output is a display of the allocations percentages should that option be selected
                        h4("Category Percentages"),
                        DT::dataTableOutput("perc_table")
                        )
                    ),
            tabItem(tabName = "explore",
                    fluidPage(
                        h4("Exploration of Specific Areas"),
                        br(),
                        leafletOutput("popMap")
                        )
                    )
            )
        )
    )
)
