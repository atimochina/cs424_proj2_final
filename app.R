
#libraries
library(ggplot2)
library(lubridate)
library(leaflet)
library(stringr)
library(shiny)
library(shinydashboard)
library(dplyr)
library(hrbrthemes)
library(gcookbook)
library(tidyverse)

# loading data from preprocessed file 
# that was run to save data in RData format
load("atlantic_new.RData")

#================================ UI ===================================
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "Project 2 - Against the Wind", titleWidth = 265),
    
    sidebar <- dashboardSidebar(
        width = 265,
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("info"))
        )  
    ), # end sidebar
    
    body <- dashboardBody(

        h2("Wind Speed and Pressure Information of Atlantic and Pacific Hurricanes", align = "center"),
        fluidRow( 
            column(6,
                   box(title="Maximum Wind Speed in a Year", plotOutput("max_line_graph",), width = 12, height = 400)
            ),
            column(6,
                   box(title="Minimum Pressure in a Year", plotOutput("min_line_graph",), width = 12, height = 400)
                
            )
        ),
        hr(style="border-color: black; border-width:5px"),
        h2("Map Visualization Based on Reactive Lists of Atlantic and Pacific Hurricanes", align = "center"),
        fluidRow( 
            #ATLANTIC HALF
            column(6, 
                   #MAP and LIST SECTION
                   column(8,
                          column(2,
                                 box(title = "Options Atlantic",
                                     width = 12, height = 450),
                                 box(title = "Filter Atlantic",
                                     width = 12, height = 450)
                          ),
                          column(3,
                                 box(title = "Atlantic Hurricanes List",
                                     tableOutput("list_atlantic"), width = 12, height = 920)
                                 
                          ),
                          column(7,
                                 box(title = "Atlantic Hurricanes Map",
                                     leafletOutput("atlantic_map"), width = 12, height = 920)
                          )
                   ),
                   #OVERVIEW PART
                   column(4,
                          fluidRow(
                              box(title="Overview of Atlantic Hurricanes By Year", plotOutput("atlantic_plot1"), width = 12, height = 450)
                          ),
                          fluidRow(
                              box(title="Overview of Atlantic Hurricanes By Category", plotOutput("atlantic_plot2"), width = 12, height = 450)
                          )
                   )

            ),
            #PACIFIC HALF
            column(6, 
                   #MAP and LIST SECTION
                   column(8,
                          column(2,
                                 box(title = "Options Pacific",
                                     width = 12, height = 450),
                                 box(title = "Filter Pacific",
                                     width = 12, height = 450)
                          ),
                          column(3,
                                 box(title = "Pacific Hurricanes List",
                                     tableOutput("list_pacific"), width = 12, height = 920)
                                 
                          ),
                          column(7,
                                 box(title = "Pacific Hurricanes Map",
                                     leafletOutput("pacific_map"), width = 12, height = 920)
                          )
                   ),
                   #OVERVIEW PART
                   column(4,
                          fluidRow(
                              box(title="Overview of Pacific Hurricanes By Year", plotOutput("pacific_plot1"), width = 12, height = 450)
                          ),
                          fluidRow(
                              box(title="Overview of Pacific Hurricanes By Category", plotOutput("pacific_plot2"), width = 12, height = 450)
                          )
                   )
            )
            
            
        )
    )
    
) # end dashboardPage

#================================ SERVER ===================================

server <- function(input, output) {
   
    # ====== MAP ====== Needs reactive for maps
    # Atlantic
    output$atlantic_map <- renderLeaflet({
        m <- m <- leaflet(dfAtlantic) %>%
            addTiles() %>%
            addProviderTiles(providers$CartoDB.Voyager) %>%
            addLegend("bottomright", pal = pal, values = df5$Name, opacity = 1) %>%
            addCircleMarkers(data = df5,
                             lng = ~Longitude,
                             lat = ~Latitude,
                             color = ~pal(df5$Name),
                             fillOpacity = 0.5,
                             popup = (paste(df5$Name, "<br>",
                                            df5$`Max Wind`, "mph")),
                             radius = df5$`Max Wind`/8)
    })
    # Pacific
    

}

shinyApp(ui = ui, server = server)