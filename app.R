
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

load("pacific.RData")
load("atlantic.RData")

ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "Project 2 - Against the Wind", titleWidth = 265),
    
    sidebar <- dashboardSidebar(
        width = 265,
        sidebarMenu(
            menuItem("Atlantic Hurricanes", tabName = "atlantic", icon = icon("tint")),
            menuItem("Pacific Hurricanes", tabName = "pacific", icon = icon("tint")),
            menuItem("About", tabName = "about", icon = icon("info")),
            
            # drop down boxes - Year, Name, Date (Month and Day)
            selectInput("Name", "Select Name", c("Summary", listNameAtlantic, listNamePacific), selected = "Summary"),
            selectInput("Month", "Select Month", c("Summary", listMonthAtlantic, listMonthPacific), selected = "Summary"),
            selectInput("Day", "Select Day", c("Summary", listDayAtlantic, listDayPacific), selected = "Summary"),
            selectInput("Year", "Select Year", c("Summary", listYearAtlantic, listYearPacific), selected = "Summary")
        )  
    ), # end sidebar
    
    body <- dashboardBody(
        tabItems(
            tabItem(
                tabName = "atlantic",
                h2("Overview of Atlantic Hurricanes Since 2005"),
                fluidRow(
                    box(title="By Year", plotOutput("atlanticplot1",), width = 4),
                    box(title="By Classification", plotOutput("atlanticplot2",), width = 4),
                    box(title="By Hurricane Category", plotOutput("atlanticplot3",), width = 4)
                ),
                h2("Map of Atlantic Hurricanes in 2018"),
                fluidRow(
                    box(leafletOutput("map1", height = 700), width = 12, height = 700)
                ),
                h2("Chart of Atlantic Hurricanes"),
                fluidRow(
                    box(tableOutput("tableAtlantic"), width = 12, height = 500),
                    box(dateRangeInput("date",strong("Date range"), start = "2005-01-01", end = "2018-12-31", min = "2005-01-01", max = "2018-12-31"))
                ),
            ),
            tabItem(tabName = "pacific",
                h2("Overview of Pacific Hurricanes Since 2005"),
                fluidRow(
                    box(title="By Year", plotOutput("pacificplot1",), width = 4),
                    box(title="By Classification", plotOutput("pacificplot2",), width = 4),
                    box(title="By Hurricane Category", plotOutput("pacificplot3",), width = 4)
                ),
                h2("Chart of Pacific Hurricanes"),
                fluidRow(
                    box(tableOutput("tablePacific"), width = 12, height = 500)
                ),
                h2("Map of Pacific Hurricanes in 2018"),
                fluidRow(
                    box(leafletOutput("map2", height = 700), width = 12, height = 700)
                )
            ),
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


server <- function(input, output) {
    
    
    
    # ====== Reactive function ====== Needs to be fixed! This is currently using Atlantic df
    reactiveFunc <- reactive({
        
        # all summary
        if(input$Name == "Summary" & input$Month == "Summary" & input$Day == "Summary" & input$Year == "Summary")
            return (df)
        # specific Year
        else if (input$Name == "Summary" & input$Month == "Summary" & input$Day == "Summary" & input&Year != "Summary")
            return (df[df$Year == input$Year,])
        # specific Month
        else if (input$Name == "Summary" & input$Month != "Summary" & input$Day == "Summary" & input&Year == "Summary")
            return (df[df$Month == input$Month,])
        # specific Day
        else if (input$Name == "Summary" & input$Month == "Summary" & input$Day != "Summary" & input&Year == "Summary")
            return (df[df$Day == input$Day,])
        # specific Year and Day
        else if (input$Name == "Summary" & input$Month == "Summary" & input$Day != "Summary" & input$Year != "Summary")
            return (df[df$Year == input$Year,]) & (df[df$Day == input$Day])
        # specific Year and Month
        else if (input$Name == "Summary" & input$Month != "Summary" & input$Day == "Summary" & input$Year != "Summary")
            return (df[df$Year == input$Year,]) & (df[df$Month == input$Month])
        # specific Month and Day
        else if (input$Name == "Summary" & input$Month != "Summary" & input$Day != "Summary" & input$Year == "Summary")
            return (df[df$Month == input$Month,]) & (df[df$Day == input$Day])
        # specific Month, Day and Year
        else if (input$Name == "Summary" & input$Month != "Summary" & input$Day != "Summary" & input$Year != "Summary")
            return (df[df$Month == input$Month,]) & (df[df$Day == input$Day]) & (df[df$Year == input$Year])
        # specific name, month, day and year
        else
            return (df[df$Name == input$Name]) & (df[df$Month == input$Month,]) & (df[df$Day == input$Day]) & (df[df$Year == input$Year])
    })
    
    
    # ==================================================================
    # ====== color palette ======
    pal25 <- c(
        "dodgerblue2", "#E31A1C", # red
        "green4",
        "#6A3D9A", # purple
        "#FF7F00", # orange
        "black", "gold1",
        "skyblue2", "#FB9A99", # lt pink~~
        "palegreen2",
        "#CAB2D6", # lt purple
        "#FDBF6F", # lt orange
        "gray70", "khaki2",
        "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
        "darkturquoise", "green1", "yellow4", "yellow3",
        "darkorange4", "brown"
    )
    
    pal <- colorFactor(
        palette = pal25,
        domain = df5$Name
    )
    
    pal2 <- colorFactor(
        palette = pal25,
        domain = dfPacific5$Name
    )
    
    # ==================================================================
    # ====== outputs for ATLANTIC OVERVIEW ======
    #By Year
    output$atlanticplot1 <- renderPlot({
        ggplot(df3, aes(x=Year)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(y= "Number of Hurricanes", x = "Year")
    })
    
    #By Classification
    output$atlanticplot2 <- renderPlot({
        ggplot(df3, aes(x=df3$`Status of System`)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(y= "Number of Hurricanes", x = "Hurricane Classification")
    })
    
    #By Hurricane Category
    output$atlanticplot3 <- renderPlot({
        ggplot(df4, aes(x=df4$`Hurricane Category`)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(y= "Number of Hurricanes", x = "Hurricane Category")
    })
    
    # ========== Reactive Chart ==========
    #output$tableAtlantic <- renderTable(reactiveFunc())
    
    # ==================================================================
    # ====== outputs for PACIFIC OVERVIEW ======
    #By Year
    output$pacificplot1 <- renderPlot({
        ggplot(dfPacific3, aes(x=Year)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(y= "Number of Hurricanes", x = "Year")
    })
    
    #By Classification
    output$pacificplot2 <- renderPlot({
        ggplot(dfPacific3, aes(x=dfPacific3$`Status of System`)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(y= "Number of Hurricanes", x = "Hurricane Classification")
    })
    
    #By Hurricane Category
    output$pacificplot3 <- renderPlot({
        ggplot(dfPacific4, aes(x=dfPacific4$`Hurricane Category`)) + geom_bar(fill = "#617a89") +theme_ipsum() +labs(y= "Number of Hurricanes", x = "Hurricane Category")
    })
    
    # ====== Reactive Tables ====== Needs to be fixed!
    
    #output$tableAtlantic <- renderTable({
        #
    #})
    
    #output$tablePacific <- renderTable({
    #
    #})
    
    
    # ====== MAP ====== Needs reactive for maps
    # Atlantic
    output$map1 <- renderLeaflet({
        m <- m <- leaflet(df5) %>%
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
    output$map2 <- renderLeaflet({
        m <- leaflet(dfPacific5) %>%
            addTiles() %>%
            addProviderTiles(providers$CartoDB.Voyager) %>%
            addLegend("bottomright", pal = pal2, values = dfPacific5$Name, opacity = 1) %>%
            addCircleMarkers(data = dfPacific5,
                             lng = ~Longitude,
                             lat = ~Latitude,
                             color = ~pal2(dfPacific5$Name),
                             fillOpacity = 0.5,
                             popup = (paste(dfPacific5$Name, "<br>",
                                            dfPacific5$`Max Wind`, "mph")),
                             radius = dfPacific5$`Max Wind`/8)
    })
    
}

shinyApp(ui = ui, server = server)