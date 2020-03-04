#libraries
library(ggplot2)
library(lubridate)
library(leaflet)
library(stringr)
library(dplyr)

# ============= ATLANTIC DATA =============
df <- read.table("data/atlantic_clean.csv", header = TRUE, stringsAsFactors = FALSE, sep = c(",","[","]"))
df$X23 <- NULL
names(df)[1] <- "Name"
names(df)[2] <- "Basin/ATCF/Year"
names(df)[3] <- "Date"
names(df)[4] <- "Time"
names(df)[5] <- "Record Identifier"
names(df)[6] <- "Status of System"
names(df)[7] <- "Latitude/Hemisphere"
names(df)[8] <- "Longitude/Hemisphere"
names(df)[9] <- "Max Wind" #in knots
names(df)[10] <- "Min Pressure" #in millibars
names(df)[11] <- "34kt Wind Radii in NE Qaudrant"
names(df)[12] <- "34kt Wind Radii in SE Qaudrant"
names(df)[13] <- "34kt Wind Radii in SW Qaudrant"
names(df)[14] <- "34kt Wind Radii in NW Qaudrant"
names(df)[15] <- "50kt Wind Radii in NE Qaudrant"
names(df)[16] <- "50kt Wind Radii in SE Qaudrant"
names(df)[17] <- "50kt Wind Radii in SW Qaudrant"
names(df)[18] <- "50kt Wind Radii in NW Qaudrant"
names(df)[19] <- "64kt Wind Radii in NE Qaudrant"
names(df)[20] <- "64kt Wind Radii in SE Qaudrant"
names(df)[21] <- "64kt Wind Radii in SW Qaudrant"
names(df)[22] <- "64kt Wind Radii in NW Qaudrant"

# ====== change date format ======
# example: 18510625 -> 1851-06-25 -> Jun 25, 1851 (abbreviated)
df$Date <- as.Date(as.character(df$Date), "%Y %m %d") # makes date in to 1851-06-25
df$Date <- format(as.Date(df$Date), "%b %d %Y")

# ====== Basin, ATCF, Year ======
# separate data frames - these columns will be on the far right of the table
df$Basin <- as.character(substr(df$`Basin/ATCF/Year`, 1, 2))
df$ATCF <- as.numeric(substr(df$`Basin/ATCF/Year`, 3, 4))
df$Year <- as.numeric(substr(df$`Basin/ATCF/Year`, 5, 8))

df$`Basin/ATCF/Year` <- NULL # remove column

# ====== change time in two columns (Hr:0 - 23, Min: 0 - 59) ======

df$Time <- str_pad(df$Time, 4, pad = "0") # extra 0's padding if time is not 4 digits
df$Hour <- as.numeric(substr(df$Time, 1, 2)) # hour
df$Minute <- as.numeric(substr(df$Time, 3, 4)) # minute

df$Time <- NULL # remove column

# ====== separate Hemisphere direction from Latitude/Longtitude ====== 
# also change Lat and Long column type

# Lat: -90 to 90
df$Latitude <- as.numeric(substr(df$`Latitude/Hemisphere`, 1, 5)) # decimal
df$HemiNS <- as.character(substr(df$`Latitude/Hemisphere`, 6, 6)) # direction

# Long: -180 to 180
df$Longitude <- as.numeric(substr(df$`Longitude/Hemisphere`, 1, 6)) # decimal
df$HemiEW <- as.character(substr(df$`Longitude/Hemisphere`, 7, 7)) # direction

# remove columns
df$`Latitude/Hemisphere` <- NULL
df$`Longitude/Hemisphere` <- NULL

for (i in 1:length(df$HemiEW)) {
  if(df$HemiEW[i] == "W")
    df$Longitude[i] = df$Longitude[i]*-1
  if(df$HemiNS[i] == "S")
    df$Latitude[i] = df$Latitude[i]*-1
  
}

# ============= unique dates =============

dates <- data.frame(str_split_fixed(df$Date, " ", 3))
df$Month <- as.character(dates$X1)
df$Day <- as.numeric(dates$X2)

# ====== remove whitespace from Name col ======
df$Name <- gsub('\\s+', '', df$Name)

# ====== create dataframes for graphs of Atlantic overview ======
# df2 - hurricanes since 2005, complete
df2 <- subset(df, Year>='2005')

# df3 - hurricanes since 2005, unique
df3 <- select(df2, 'Name', 'Year', 'Status of System', 'Max Wind')
df3 <- df3[order(df3[,1], -df3[,4]),] #order by Name asc and Max Wind desc
df3 <- df3[!duplicated(df3$Name),] #only keep first hurricane entry which is its max (by wind speed)

# df4 - actual hurricanes since 2005, unique
df4 <- subset(df3, df3$`Max Wind`>73)
df4 <- df4[order(df4[,4]),] #order by max wind asc
df4$'Hurricane Category' <- cut(df4$`Max Wind`, c(74,96,111,130,157), include.lowest = T) #add col for Category
levels(df4$'Hurricane Category') = c("cat 1", "cat 2", "cat 3", "cat 4", "cat 5")

# df5 - hurricanes in 2018 only
df5 <- subset(df, Year=='2018')

# list of months, days, and years -- using unique
listYearAtlantic <- as.numeric(unique(df$Year))
listMonthAtlantic <- as.character(unique(df$Month))
listDayAtlantic <- sort(as.numeric(unique(df$Day)), decreasing = FALSE)
listNameAtlantic <- as.character(unique(df$Name))
