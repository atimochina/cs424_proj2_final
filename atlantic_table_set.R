#load df - Atlantic
load("atlantic_new.RData")

# =========== Hurricane Since 2005 (2005 - 2018) - Bar graph ===========
# Atlantic
Atlantic2005_2018 <- 
  filter(dfAtlantic, Year >= 2005 ) %>%
  ggplot(aes(x = Year)) + geom_bar() + ggtitle("Hurricanes Since 2005") + ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram graph - NA could mean C5?
AtlanticCategory <- filter(dfAtlantic, Year >= 2005) %>% 
  ggplot(aes(Year, fill = `Hurricane Category` ) ) +
  geom_bar(position="dodge") + ggtitle("Hurricanes Per Year of Category") + ylab("Count") + 
  theme(plot.title = element_text(hjust = 0.5))

# =========== Hurricane Per Year ===========
# Atlantic
ggplot(dfAtlantic) + geom_bar(aes(Year)) + ggtitle("Hurricanes Per Year") + ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5))


# =========== Total number of hurricanes each year - Double Line graph ===========
# x = days of the year, y = wind speed
# ?? can't change format in test variable
#day <- as.Date(dfAtlantic$Date, "%Y-%m-%d")

#dfAtlantic$`Day of Year`

# Wind Speed