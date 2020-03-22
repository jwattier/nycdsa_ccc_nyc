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
shinyUI(fluidPage(

    # Application title
    titlePanel("NYC Access Map"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId="select_category",
                        label="Resource Category:",
                        choices = resouse_categories)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h4("Heatmap of Accessiblity"),
            leafletOutput("accessMap"),
            
            # Output is a display of the allocations percentages should that option be selected
            h4("Category Percentages"),
            DT::dataTableOutput("perc_table")
        )
    )
))
