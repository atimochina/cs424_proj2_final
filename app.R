
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
load("pacific_new.RData")

#================================ UI ===================================
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "Project 2 - Against the Wind", titleWidth = 265),
    
    sidebar <- dashboardSidebar(
        width = 265,
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("chart-bar")),
            menuItem("Map View", tabName = "map", icon = icon("map-marked-alt")),
            menuItem("About", tabName = "about", icon = icon("info"))
            
            # NEED TO FIX 
            # drop down boxes - Year, Name, Date (Month and Day)
            #selectInput("Name", "Select Name", c("Summary", listNameAtlantic, listNamePacific), selected = "Summary"),
            #selectInput("Month", "Select Month", c("Summary", listMonthAtlantic, listMonthPacific), selected = "Summary"),
            #selectInput("Day", "Select Day", c("Summary", listDayAtlantic, listDayPacific), selected = "Summary"),
            #selectInput("Year", "Select Year", c("Summary", listYearAtlantic, listYearPacific), selected = "Summary")
        )
    ), # end sidebar
    
    body <- dashboardBody(
        tabItems(
            
            # OVERVIEW TAB
            tabItem(
                tabName = "overview",
                h2("Overview of Atlantic and Pacific Hurricane Data"),
                fluidRow(
                    box(plotOutput("plot1",), width = 7),
                    box(plotOutput("plot2",), width = 5)
                ),
                fluidRow(
                    box(plotOutput("plot3",), width = 7),
                    box(plotOutput("plot4",), width = 5)
                ),
            ),
            
            # MAP VIEW TAB
            tabItem(tabName = "map",
                    # LINE GRAPH 1
                    fluidRow(
                        box(title = "Atlantic Line Graph", width = 12)
                    ),
                    
                    # SINGLE MAP WITH ATLANTIC LIST/OPTIONS AND PACIFIC LIST/OPTIONS
                    fluidRow(
                        box(title = "Atlantic Map Options", width = 1),
                        box(title = "Atlantic Hurricanes List", width = 2),
                        box(title = "Atlantic+Pacific Map", width = 6, leafletOutput("atlantic_map", height = 250)), # Testing map reactive for a bit
                        box(title = "Pacific Map Options", width = 1),
                        box(title = "Pacific Hurricanes List", width = 2)
                    ),
                    
                    # LINE GRAPH 2
                    fluidRow(
                        box(title = "Pacific Line Graph", width = 12)
                    )
                    
            ),
            
            # ABOUT TAB
            tabItem(
                tabName = "about",
                h2("Project Details"),
                h3("Dashboard by Angela Timochina, Amy Ngo, and Desiree Murray for CS 424 at UIC"),
                h3("Data from the Atlantic hurricane database (HURDAT2) 1851-2018 and the Northeast and North Central Pacific hurricane database (HURDAT2) 1949-2018  at http://www.nhc.noaa.gov/data/#hurdat"),
                h3("Created using RStudio and Shiny with shinydashboard, ggplot2, lubridate, stringr, dplyr and leaflet libraries")
            )
        )
    ) # end dashboardBody
    
) # end dashboardPage
#================================ SERVER ===================================

server <- function(input, output) {
    # ======== Reactive ========
    # Selection (selectA) - select all, unselect all
    # Note: Select All means all buttons in option(below) is selected or not
    selectReact <- reactive({
        #if(selectA == `Select All`){
        #    return
        #} else {
        #    return
        #}
    })
    # Show By (optionA) - Top Ten Overall, Since 2005
    #optionReact <- reactive({
    #    if(optionA == `Top Ten Overall` && optionA != `Since 2005`){
    #        return
    #    }
    #    else if (optionA != `Top Ten Overall` && optionA == `Since 2005`){
    #        return
    #    }
    #    else if (optionA != `Top Ten Overall` && optionA != `Since 2005`){
    #        return
    #    }
    #})
    # Order By (filterA) - Chronologically, Alphabetically, Max Wind Speed, Minimum Pressure
    filterReact <- reactive({
        #
        if(filterA == `Chronologically`){
            return (dfAtlantic[sort(dfAtlantic$Date, factorsAsCharacter = TRUE)])
        }
        else if(filterA == `Alphabetically`){
            return (dfAtlantic[sort(dfAtlantic$Name, factorsAsCharacter = TRUE)])
        }
        else if(filterA == `Max Wind Speed`){
            return (dfAtlantic[sort(dfAtlantic$`Max Wind`, decreasing = FALSE)])
        }
        else if(filterA == `Minimum Pressure`){
            return (dfAtlantic[sort(dfAtlantic$`Min Pressure`, decreasing = FALSE)])
        }
    })
    #ATLANTIC OVERVIEW PLOTS
    output$plot1 <- renderPlot({
        ggplot(dfAtlantic, aes(x=Year)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(title = "Atlantic Hurricanes By Year",
            subtitle = "1851-present",
            y= "Number of Hurricanes", x = "Year")
    })
    
    output$plot2 <- renderPlot({
        ggplot(dfAtlantic, aes(x=dfAtlantic$`Hurricane Category`)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(title = "Atlantic Hurricanes By Category",
            subtitle = "1851-present",
            y= "Number of Hurricanes", x = "Hurricane Category")
    })
    
    #PACIFIC OVERVIEW PLOTS
    output$plot3 <- renderPlot({
        ggplot(dfPacific, aes(x=Year)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(title = "Pacific Hurricanes By Year",
            subtitle = "1949-present",
            y= "Number of Hurricanes", x = "Year")
    })
    
    output$plot4 <- renderPlot({
        ggplot(dfPacific, aes(x=dfPacific$`Hurricane Category`)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(title = "Pacific Hurricanes By Category",
            subtitle = "1949-present",
            y= "Number of Hurricanes", x = "Hurricane Category")
    })
    
    # ====== MAP ====== Needs reactive for maps
    # Atlantic
    output$atlantic_map <- renderLeaflet({
        #optionData <- optionReact
        filterData <- filterReact
        # temporary color - however, getting "non-numeric argument to binary operator"
        pal <- colorNumeric(
            palette = "Blues",
            domain = dfAtlantic$`Max Wind`)
        
        m <- m <- leaflet(dfAtlantic) %>%
            addTiles() %>%
            addProviderTiles(providers$CartoDB.Voyager) %>%
            addLegend("bottomright", pal = pal
                      , values = dfAtlantic$Name, opacity = 1) %>%
            addCircleMarkers(data = dfAtlantic,
                             lng = ~Longitude,
                             lat = ~Latitude,
                             color = ~pal(dfAtlantic$Name),
                             fillOpacity = 0.5,
                             popup = (paste(dfAtlantic$Name, "<br>",
                                            dfAtlantic$`Max Wind`, "mph")),
                             radius = dfAtlantic$`Max Wind`/8)
    })
    
}

shinyApp(ui = ui, server = server)
