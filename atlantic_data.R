#new processing of atlantic data
#libraries
library(ggplot2)
library(lubridate)
library(leaflet)
library(stringr)
library(dplyr)

# ============= ATLANTIC DATA =============
df <- read.table("data/atlantic_clean.csv", header = TRUE, stringsAsFactors = FALSE, sep = c(",","[","]"),row.names = NULL)
df$X23 <- NULL
names(df)[1] <- "Number of Entries"
names(df)[2] <- "Name"
names(df)[3] <- "Basin/ATCF/Year"
names(df)[4] <- "Date"
names(df)[5] <- "Time"
names(df)[6] <- "Record Identifier"
names(df)[7] <- "Status of System"
names(df)[8] <- "Latitude/Hemisphere"
names(df)[9] <- "Longitude/Hemisphere"
names(df)[10] <- "Max Wind" #in knots
names(df)[11] <- "Min Pressure" #in millibars
names(df)[12] <- "34kt Wind Radii in NE Qaudrant"
names(df)[13] <- "34kt Wind Radii in SE Qaudrant"
names(df)[14] <- "34kt Wind Radii in SW Qaudrant"
names(df)[15] <- "34kt Wind Radii in NW Qaudrant"
names(df)[16] <- "50kt Wind Radii in NE Qaudrant"
names(df)[17] <- "50kt Wind Radii in SE Qaudrant"
names(df)[18] <- "50kt Wind Radii in SW Qaudrant"
names(df)[19] <- "50kt Wind Radii in NW Qaudrant"
names(df)[20] <- "64kt Wind Radii in NE Qaudrant"
names(df)[21] <- "64kt Wind Radii in SE Qaudrant"
names(df)[22] <- "64kt Wind Radii in SW Qaudrant"
names(df)[23] <- "64kt Wind Radii in NW Qaudrant"

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
dates <- NULL

# ====== remove whitespace from Name col ======
df$Name <- gsub('\\s+', '', df$Name)

#======== Naming Unnames Hurricanes ======
#got through the entire list of hurricanes
df$`Number of Entries` <- as.numeric(as.character(df$`Number of Entries`))
count = 1
finish = df$`Number of Entries`[1]

for(i in 1:length(df$`Number of Entries`)){
  
  if(i <= finish){
    if(df$Name[i] == 'UNNAMED'){
      df$NewName[i] <- paste('HURRICANE ',count)
    }
    else{
      df$NewName[i] <- df$Name[i]
    }
  }
  if(i == finish){
    if(df$NewName[i] == paste('HURRICANE ', count)){
      count = count + 1
    }
    finish = finish + df$`Number of Entries`[i+1]
  }
}
df$Name <- df$NewName
df$NewName <- NULL

#==== Categorizing hurricanes based on max wind speed ======
df$'Hurricane Category' <- cut(df$`Max Wind`, c(0,74,96,111,130,157), include.lowest = T)
levels(df$'Hurricane Category') = c(NA,1, 2, 3, 4, 5)
df$`Hurricane Category` <- as.numeric(df$`Hurricane Category`)

dfAtlantic <- df

# list of months, days, and years -- using unique
listYearAtlantic <- as.numeric(unique(df$Year))
listMonthAtlantic <- as.character(unique(df$Month))
listDayAtlantic <- sort(as.numeric(unique(df$Day)), decreasing = FALSE)
listNameAtlantic <- as.character(unique(df$Name))

save(dfAtlantic, listYearAtlantic, listNameAtlantic,
     listMonthAtlantic, listDayAtlantic, file = "atlantic_new.RData")
