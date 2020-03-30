
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
            menuItem("About", tabName = "about", icon = icon("info")),
            
        )
    ), # end sidebar
    
    body <- dashboardBody(
        tabItems(
            
            # OVERVIEW TAB
            tabItem(
                tabName = "overview",

                #h3("Overview of Atlantic and Pacific Hurricane Data"),
                fluidRow(
                    valueBox(textOutput("numATL"), "Atlantic Hurricanes Recorded Since 1851", icon = icon("tint"), color = "light-blue", width = 3),
                    valueBox(textOutput("numPAC"), "Pacific Hurricanes Recorded Since 1949", icon = icon("tint"), color = "light-blue", width = 3),
                    box(htmlOutput("catInfo1"), status = "primary", width = 4),
                    box(htmlOutput("catInfo2"), status = "primary", width = 2)
                ),
                
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
                        box(width = 12,
                            plotOutput("line1"))
                    ),
                    
                    # SINGLE MAP WITH ATLANTIC LIST/OPTIONS AND PACIFIC LIST/OPTIONS
                    fluidRow(
                        box(title = "Atlantic Map Options", width = 1, 
                            selectInput("NameA", "Select Name", c(listNameAtlantic)),
                            selectInput("FilterA", "Select Filter", c("Chronologically", "Alphabetically", "Max Wind Speed", "Minimum Pressure")),
                            selectInput("ListA", "Select List", c("2018 Hurricanes", "Since 2005", "All Hurricanes", "Top 10", listNameAtlantic))
                        ),
                        box(title = "Atlantic Hurricanes List", width = 2),
                        box(title = "Atlantic+Pacific Map", width = 6),
                        box(title = "Pacific Map Options", width = 1, 
                            selectInput("NameP", "Select Name", c(listNamePacific)),
                            selectInput("FilterP", "Select Filter", c("Chronologically", "Alphabetically", "Max Wind Speed", "Minimum Pressure")),
                            selectInput("ListP", "Select List", c("2018 Hurricanes", "Since 2005", "All Hurricanes", "Top 10", listNamePacific))
                        ),
                        box(title = "Pacific Hurricanes List", width = 2)
                    ),
                    
                    # LINE GRAPH 2
                    fluidRow(
                        box( width = 12,
                             plotOutput("line2"))
                    )
                    
            ),
            
            # ABOUT TAB
            tabItem(
                tabName = "about",
                h2("Project Details"),
                h3("Dashboard by Angela Timochina, Amy Ngo, and Desiree Murray for CS 424 at UIC"),
                h3("Created using RStudio and Shiny with shinydashboard, ggplot2, lubridate, stringr, dplyr and leaflet libraries"),
                h3("Data from the Atlantic hurricane database (HURDAT2) 1851-2018 and the Northeast and North Central Pacific hurricane database (HURDAT2) 1949-2018  at http://www.nhc.noaa.gov/data/#hurdat"),
                h3("Saffir-Simpson Hurricane Wind Scale information from National Hurricane Center - https://www.nhc.noaa.gov/aboutsshws.php")
                
            )
        )
    ) # end dashboardBody
    
) # end dashboardPage
#================================ SERVER ===================================

server <- function(input, output) {
   
    #INFOBOXES
    output$numATL <- renderText(length(unique(dfAtlantic$Name)))
    output$numPAC <- renderText(length(unique(dfPacific$Name)))
    
    output$catInfo1 <- renderUI(HTML(
        paste(
            "<b>Hurricane Classification</b>", br(),
            "The Saffir-Simpson Hurricane Wind Scale classifies hurricanes – Western Hemisphere tropical cyclones – 
            that exceed the intensities of tropical depressions <b>TD</b> (<38 mph) and tropical storms <b>TS</b> (39-73 mph)
            – into five categories distinguished by the intensities of their sustained winds."
        )
    ))
    
    output$catInfo2 <- renderUI(HTML(
        paste(
            "Category 1 <b>(C1)</b>: 74-95 mph", br(),
            "Category 2 <b>(C2)</b>: 96-110 mph", br(),
            "Category 3 <b>(C3)</b>: 111-129 mph", br(),
            "Category 4 <b>(C4)</b>: 130-156 mph", br(),
            "Category 5 <b>(C5)</b>: >156 mph"
        )
    ))

    # ======== Reactive Atlantic ========
    # Name of hurricanes
    nameAReact <- reactive({
       return (dfAtlantic[dfAtlantic$Name == input$NameA,])
    })
    # Filter By (Filter) - Chronologically, Alphabetically, Max Wind Speed, Minimum Pressure
    filterAReact <- reactive({
        #
        if(FilterA == `Chronologically`){
            return (dfAtlantic[sort(dfAtlantic$Date, factorsAsCharacter = TRUE)])
        }
        else if(FilterA == `Alphabetically`){
            return (dfAtlantic[sort(dfAtlantic$Name, factorsAsCharacter = TRUE)])
        }
        else if(FilterA == `Max Wind Speed`){
            return (dfAtlantic[sort(dfAtlantic$`Max Wind`, decreasing = FALSE)])
        }
        else if(FilterA == `Minimum Pressure`){
            return (dfAtlantic[sort(dfAtlantic$`Min Pressure`, decreasing = FALSE)])
        }
    })
    # List - Top Ten Overall, Since 2005, etc.
    listAReact <- reactive({
        if(input$ListA == "2018 Hurricanes"){
            return (dfAtlantic[dfAtlantic$Year == 2018]) # hurricanges in 2018
        }
        else if(input$ListA == "Since 2005"){
            return (dfAtlantic[dfAtlantic$Year >= 2005]) # hurricanes in 2005 and after
        }
        else if(input$ListA == "All Hurricanes"){
            return (dfAtlantic) # all hurricanes
        }
        else if(intput$ListA == "Top 10"){
            return (dfAtlantic10) # return top 10 specific hurricane dataframe
        }
        else{
            return (dfAtlantic[dfAtlantic$Name == input$ListA]) #specific hurricanes
        }
    })
    
    # ======== Reactive Pacific ========
    # Name of hurricanes
    namePReact <- reactive({
        return (dfPacific[dfPacific$Name == input$NameP,])
    })
    # Filter By (Filter) - Chronologically, Alphabetically, Max Wind Speed, Minimum Pressure
    filterPReact <- reactive({
        #
        if(FilterP == `Chronologically`){
            return (dfPacific[sort(dfPacific$Date, factorsAsCharacter = TRUE)])
        }
        else if(FilterP == `Alphabetically`){
            return (dfPacific[sort(dfPacific$Name, factorsAsCharacter = TRUE)])
        }
        else if(FilterP == `Max Wind Speed`){
            return (dfAPacific[sort(dfPacific$`Max Wind`, decreasing = FALSE)])
        }
        else if(FilterP == `Minimum Pressure`){
            return (dfPacific[sort(dfPacific$`Min Pressure`, decreasing = FALSE)])
        }
    })
    # List - Top Ten Overall, Since 2005, etc.
    listPReact <- reactive({
        if(input$ListP == "2018 Hurricanes"){
            return (dfPacific[dfPacific$Year == 2018]) # hurricanges in 2018
        }
        else if(input$ListP == "Since 2005"){
            return (dfPacific[dfPacific$Year >= 2005]) # hurricanes in 2005 and after
        }
        else if(input$ListP == "All Hurricanes"){
            return (dfPacific) # all hurricanes
        }
        else if(intput$ListP == "Top 10"){
            return (dfPacific10) # return top 10 specific hurricane dataframe
        }
        else{
            return (dfPacific[dfPacific$Name == input$ListP]) #specific hurricanes
        }
    })
    # =============================================

    
    #ATLANTIC OVERVIEW PLOTS
    output$plot1 <- renderPlot({
        ggplot(dfAtlantic, aes(x=Year)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(title = "Atlantic Hurricanes By Year",
                                                                                          subtitle = "1851-present",
                                                                                          y= "Number of Hurricanes", x = "Year")
    })
    
    output$plot2 <- renderPlot({
        ggplot(dfAtlantic, aes(x=dfAtlantic$`Hurricane Category`)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(title = "Atlantic Hurricanes By Classification",
                                                                                                                     subtitle = "1851-present",
                                                                                                                     y= "Number of Hurricanes", x = "Hurricane Classification")
    })
    
    #PACIFIC OVERVIEW PLOTS
    output$plot3 <- renderPlot({
        ggplot(dfPacific, aes(x=Year)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(title = "Pacific Hurricanes By Year",
                                                                                         subtitle = "1949-present",
                                                                                         y= "Number of Hurricanes", x = "Year")
    })
    
    output$plot4 <- renderPlot({
        ggplot(dfPacific, aes(x=dfPacific$`Hurricane Category`)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(title = "Pacific Hurricanes By Classification",
                                                                                                                   subtitle = "1949-present",
                                                                                                                   y= "Number of Hurricanes", x = "Hurricane Classification")
    })
    

    #LINE GRAPH PLOTS
    #max wind
    output$line1 <- renderPlot({
        ggplot() +
            geom_line(data = atlanticDaysOfYearDF[!is.na(atlanticDaysOfYearDF$`Max Wind`),],aes(x = days, y = `Max Wind`, group = 1, color = "Atlantic"))+
            geom_line(data = pacificDaysOfYearDF[!is.na(pacificDaysOfYearDF$`Max Wind`),],aes(x = days, y = `Max Wind`, group = 1, color = "Pacific"))+
            scale_x_discrete(breaks=c("001","032","061","092","122","153","183","214","245","275","306","336"))+
            labs(x = "Days in Year", y = "Wind Speed", title = "Maximum Wind Speed of Hurricane vs. Day in a Year") +
            theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 60, hjust = 1))
    })
    #min pressure
    output$line2 <- renderPlot({
        ggplot() +
            geom_line(data = atlanticDaysOfYearDF[!is.na(atlanticDaysOfYearDF$`Min Pressure`),],aes(x = days, y = `Min Pressure`, group = 1, color = "Atlantic"))+
            geom_line(data = pacificDaysOfYearDF[!is.na(pacificDaysOfYearDF$`Min Pressure`),] ,aes(x = days, y = `Min Pressure`, group = 1, color = "Pacific"))+
            scale_x_discrete(breaks=c("001","032","061","092","122","153","183","214","245","275","306","336"))+
            labs(x = "Days in Year", y = "Wind Speed", title = "Minimum Pressure of Hurricane vs. Day in a Year") +
            theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 60, hjust = 1))
    })
    

    # ====== MAP ====== Needs reactive for maps
    # Atlantic
    output$atlantic_map <- renderLeaflet({
        #nameData <- nameReact
        filterData <- filterReact
        
        m <- m <- leaflet(dfAtlantic) %>%
            addTiles() %>%
            addProviderTiles(providers$CartoDB.Voyager) %>%
            addLegend("bottomright", pal = pal, values = dfAtlantic$Name, opacity = 1) %>%
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
