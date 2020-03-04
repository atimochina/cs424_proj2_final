#libraries
library(ggplot2)
library(lubridate)
library(leaflet)
library(stringr)

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
