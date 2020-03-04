#libraries
library(ggplot2)
library(lubridate)
library(leaflet)
library(stringr)
library(dplyr)

# ============= PACIFIC DATA =============
dfPacific <- read.table("data/northPacific_clean.csv", header = TRUE, stringsAsFactors = FALSE, sep = c(",","[","]"))
dfPacific$X23 <- NULL
names(dfPacific)[1] <- "Name"
names(dfPacific)[2] <- "Basin/ATCF/Year"
names(dfPacific)[3] <- "Date"
names(dfPacific)[4] <- "Time"
names(dfPacific)[5] <- "Record Identifier"
names(dfPacific)[6] <- "Status of System"
names(dfPacific)[7] <- "Latitude/Hemisphere"
names(dfPacific)[8] <- "Longitude/Hemisphere"
names(dfPacific)[9] <- "Max Wind" #in knots
names(dfPacific)[10] <- "Min Pressure" #in millibars
names(dfPacific)[11] <- "34kt Wind Radii in NE Qaudrant"
names(dfPacific)[12] <- "34kt Wind Radii in SE Qaudrant"
names(dfPacific)[13] <- "34kt Wind Radii in SW Qaudrant"
names(dfPacific)[14] <- "34kt Wind Radii in NW Qaudrant"
names(dfPacific)[15] <- "50kt Wind Radii in NE Qaudrant"
names(dfPacific)[16] <- "50kt Wind Radii in SE Qaudrant"
names(dfPacific)[17] <- "50kt Wind Radii in SW Qaudrant"
names(dfPacific)[18] <- "50kt Wind Radii in NW Qaudrant"
names(dfPacific)[19] <- "64kt Wind Radii in NE Qaudrant"
names(dfPacific)[20] <- "64kt Wind Radii in SE Qaudrant"
names(dfPacific)[21] <- "64kt Wind Radii in SW Qaudrant"
names(dfPacific)[22] <- "64kt Wind Radii in NW Qaudrant"

# ====== change date format ======
# example: 18510625 -> 1851-06-25 -> Jun 25, 1851 (abbreviated)
dfPacific$Date <- as.Date(as.character(dfPacific$Date), "%Y %m %d") # makes date in to 1851-06-25
dfPacific$Date <- format(as.Date(dfPacific$Date), "%b %d %Y")

# ====== Basin, ATCF, Year ======
# separate data frames - these columns will be on the far right of the table
dfPacific$Basin <- as.character(substr(dfPacific$`Basin/ATCF/Year`, 1, 2))
dfPacific$ATCF <- as.numeric(substr(dfPacific$`Basin/ATCF/Year`, 3, 4))
dfPacific$Year <- as.numeric(substr(dfPacific$`Basin/ATCF/Year`, 5, 8))

dfPacific$`Basin/ATCF/Year` <- NULL # remove column

# ====== change time in two columns (Hr:0 - 23, Min: 0 - 59) ======

dfPacific$Time <- str_pad(dfPacific$Time, 4, pad = "0") # extra 0's padding if time is not 4 digits
dfPacific$Hour <- as.numeric(substr(dfPacific$Time, 1, 2)) # hour
dfPacific$Minute <- as.numeric(substr(dfPacific$Time, 3, 4)) # minute

dfPacific$Time <- NULL # remove column

# ====== separate Hemisphere direction from Latitude/Longtitude ====== 
# also change Lat and Long column type

# Lat: -90 to 90
dfPacific$Latitude <- as.numeric(substr(dfPacific$`Latitude/Hemisphere`, 1, 5)) # decimal
dfPacific$HemiNS <- as.character(substr(dfPacific$`Latitude/Hemisphere`, 6, 6)) # direction

# Long: -180 to 180
dfPacific$Longitude <- as.numeric(substr(dfPacific$`Longitude/Hemisphere`, 1, 6)) # decimal
dfPacific$HemiEW <- as.character(substr(dfPacific$`Longitude/Hemisphere`, 7, 7)) # direction

# remove columns
dfPacific$`Latitude/Hemisphere` <- NULL
dfPacific$`Longitude/Hemisphere` <- NULL

# ===== unique dates =====

dates2 <- data.frame(str_split_fixed(dfPacific$Date, " ", 3))
dfPacific$Month <- as.character(dates2$X1)
dfPacific$Day <- as.numeric(dates2$X2)

# ====== remove whitespace from Name col ======
dfPacific$Name <- gsub('\\s+', '', dfPacific$Name)

# ====== make longitudes negative ======
dfPacific$Longitude <- dfPacific$Longitude * -1

# ====== create dataframes for graphs of Atlantic overview ======
# dfPacific2 - hurricanes since 2005, complete
dfPacific2 <- subset(dfPacific, Year>='2005')

# dfPacific3 - hurricanes since 2005, unique
dfPacific3 <- select(dfPacific2, 'Name', 'Year', 'Status of System', 'Max Wind')
dfPacific3 <- dfPacific3[order(dfPacific3[,1], -dfPacific3[,4]),] #order by Name asc and Max Wind desc
dfPacific3 <- dfPacific3[!duplicated(dfPacific3$Name),] #only keep first hurricane entry which is its max (by wind speed)

# dfPacific4 - actual hurricanes since 2005, unique
dfPacific4 <- subset(dfPacific3, dfPacific3$`Max Wind`>73)
dfPacific4 <- dfPacific4[order(dfPacific4[,4]),] #order by max wind asc
dfPacific4$'Hurricane Category' <- cut(dfPacific4$`Max Wind`, c(74,96,111,130,157), include.lowest = T) #add col for Category
levels(dfPacific4$'Hurricane Category') = c("cat 1", "cat 2", "cat 3", "cat 4", "cat 5")

# dfPacific5 - hurricanes in 2018 only
dfPacific5 <- subset(dfPacific, Year=='2018')

# list of months, days, and years -- using unique
listYearPacific <- as.numeric(unique(dfPacific$Year))
listMonthPacific <- as.character(unique(dfPacific$Month))
listDayPacific <- sort(as.numeric(unique(dfPacific$Day)), decreasing = FALSE)
listNamePacific <- as.character(unique(dfPacific$Name))
