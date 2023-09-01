# Load Libraries ---------------------------------------------------------------
library(tidyverse)
library(plotly)

# Import Data -------------------------------------------------------------
Shipwrecks <- read_csv("~/Programming/Portfolio/Sunken Ships/Shipwrecks and Sunken Ships.csv", na = '') # replace empty cells with NA

# Investigating the Data --------------------------------------------------
# How many shipwrecks are listed?
count(Shipwrecks)
# 3,959

# Can we use ship names as a unique identifier?
Shipwrecks$Ship[duplicated(Shipwrecks$Ship)]
# No, 241 ship names are duplicates. A lot of wrecks do not even include a ship name.

# Are there any duplicate rows?
duplicates <- duplicated(Shipwrecks)
Shipwrecks[duplicates, ]
# Looks like there are four duplicate rows. Let's remove them.
Shipwrecks <- unique(Shipwrecks)
count(Shipwrecks)
# Four shipwrecks have been removed. Now we are down to 3,955 unique wrecks.

# We don't want to exclude any documented wrecks, even if they are missing a name, so let's add an id column to uniquely identify each wreck
Shipwrecks <- rowid_to_column(Shipwrecks, "Ship_Id")

# Standardizing Dates -----------------------------------------------------
# The date column has dates in different formats. We want to extract the year from as many of these as possible. At a glance, most dates seem to be in the day, month, year order
Shipwrecks <- Shipwrecks %>%
  mutate(Year_Sunk = ifelse(!is.na(lubridate::dmy(Shipwrecks$`Sunk date`)), as.character(year(lubridate::dmy(Shipwrecks$`Sunk date`))), # Extract instances matching DD-MM-YYYY format
                            ifelse(!is.na(lubridate::my(Shipwrecks$`Sunk date`)), as.character(year(lubridate::my(Shipwrecks$`Sunk date`))), NA))) # Extract instances matching MM-YYYY format
# Century wasn't specified in some 20th century years. R converted some to 21st century. Must convert those instances back to 20th century.
Shipwrecks$Year_Sunk <- ifelse(as.numeric(Shipwrecks$Year_Sunk) >= 2000, as.character(as.numeric(Shipwrecks$Year_Sunk) - 100), Shipwrecks$Year_Sunk)

# Extract any remaining instances of YYYY from date sunk
Shipwrecks <- Shipwrecks %>%
  mutate(Year_Sunk = ifelse(is.na(Shipwrecks$Year_Sunk) & str_detect(Shipwrecks$`Sunk date`, "\\d{4}"), str_extract(Shipwrecks$`Sunk date`, "\\d{4}"), Year_Sunk)) 

# At this point, we could scrub the remaining NAs and manually add a year or approximate year for a few of them, but at this point I want to move on with the project and am just going to leave them all as NA

# Convert years to numeric
Shipwrecks$Year_Sunk <- as.numeric(Shipwrecks$Year_Sunk)

# Remove shipwreck from 2200 BC that was misread as year 2200 AD
Shipwrecks <- Shipwrecks %>%
  mutate(Year_Sunk = ifelse(Shipwrecks$Year_Sunk == 2200, NA, Shipwrecks$Year_Sunk))

# Cleaning Coordinates -------------------------------------------------------
# Separate coordinates into latitude and longitude
Shipwrecks <- Shipwrecks %>%
  separate_wider_delim(Coordinates, " ", names = c("Latitude", "Longitude"), too_few = "align_start", too_many = "drop")

# Create a new table with shipwrecks that have coordinates and convert coordinates from DMS to decimal format
Shipwreck_Coords <- Shipwrecks %>%
  filter(!is.na(Year_Sunk) & !is.na(Latitude) & !is.na(Longitude)) %>%
  mutate(Lat_Date = ifelse(grepl("N", Latitude, fixed = TRUE), as.numeric(word(Latitude, 1, sep = "°")),
                           ifelse(grepl("S", Latitude, fixed = TRUE), -1 * as.numeric(word(Latitude, 1, sep = "°")), NA))) %>%
  mutate(Long_Date = ifelse(grepl("E", Longitude, fixed = TRUE), as.numeric(word(Longitude, 1, sep = "°")),
                            ifelse(grepl("W", Longitude, fixed = TRUE), -1 * as.numeric(word(Longitude, 1, sep = "°")), NA))) %>%
  mutate(Lat_Minutes = ifelse(grepl("N", Latitude, fixed = TRUE), sub(".*°", "", Latitude), 
                              ifelse(grepl("S", Latitude, fixed = TRUE), sub(".*°", "", Latitude), NA))) %>%
  mutate(Lat_Minutes = ifelse(grepl("′", Lat_Minutes, fixed = TRUE), as.numeric(word(Lat_Minutes, 1, sep = "′")), 0)) %>%
  mutate(Long_Minutes = ifelse(grepl("E", Longitude, fixed = TRUE), sub(".*°", "", Longitude), 
                               ifelse(grepl("W", Longitude, fixed = TRUE), sub(".*°", "", Longitude), NA))) %>%
  mutate(Long_Minutes = ifelse(grepl("′", Long_Minutes, fixed = TRUE), as.numeric(word(Long_Minutes, 1, sep = "′")), 0)) %>%
  mutate(Lat_Seconds = ifelse(grepl("N", Latitude, fixed = TRUE), sub(".*′", "", Latitude), 
                              ifelse(grepl("S", Latitude, fixed = TRUE), sub(".*′", "", Latitude), NA))) %>%
  mutate(Lat_Seconds = ifelse(grepl("″", Lat_Seconds, fixed = TRUE), as.numeric(word(Lat_Seconds, 1, sep = "″")), 0)) %>%
  mutate(Long_Seconds = ifelse(grepl("E", Longitude, fixed = TRUE), sub(".*′", "", Longitude), 
                               ifelse(grepl("W", Longitude, fixed = TRUE), sub(".*′", "", Longitude), NA))) %>% 
  mutate(Long_Seconds = ifelse(grepl("″", Long_Seconds, fixed = TRUE), as.numeric(word(Long_Seconds, 1, sep = "″")), 0)) %>%
  mutate(Lat = ifelse(grepl("N", Latitude, fixed = TRUE), Lat_Date + Lat_Minutes / 60 + Lat_Seconds / 3600,
                      ifelse(grepl("S", Latitude, fixed = TRUE), Lat_Date + -1 * Lat_Minutes / 60 + -1 * Lat_Seconds / 3600, NA))) %>%
  mutate(Long = ifelse(grepl("E", Longitude, fixed = TRUE), Long_Date + Long_Minutes / 60 + Long_Seconds / 3600,
                       ifelse(grepl("W", Longitude, fixed = TRUE), Long_Date + -1 * Long_Minutes / 60 + -1 * Long_Seconds / 3600, NA))) %>%
  select(Ship_Id, Ship, Year_Sunk, Lat, Long)

# Years With Most Shipwrecks ----------------------------------------------
# Create table of top 10 years with most shipwrecks
Years_With_Most_Shipwrecks <- Shipwrecks %>%
  filter(!is.na(Year_Sunk)) %>% # Don't include unknown years
  group_by(Year_Sunk) %>%
  summarise(Total_Wrecks = n()) %>%
  arrange(desc(Total_Wrecks)) %>%
  head(10) # Limit table to top 10 results

# Plots -------------------------------------------------------------------
# Bar plot showing number of shipwrecks per year from 1850 to 2000 with major wars highlighted
Shipwrecks_Per_Year <- ggplot(Shipwrecks, aes(x = Year_Sunk)) +
  geom_bar(fill = "steelblue") +
  annotate("rect", xmin = 1861 - 0.5, xmax = 1865 + 0.5, ymin = 0, ymax = 50, alpha = 0.3, fill = "red") + # American Civil War
  annotate("rect", xmin = 1914 - 0.5, xmax = 1918 + 0.5, ymin = 0, ymax = 110, alpha = 0.3, fill = "red") + # World War I
  annotate("rect", xmin = 1939 - 0.5, xmax = 1945 + 0.5, ymin = 0, ymax = 350, alpha = 0.3, fill = "red") + # World War II
  xlim(1850, 2000) +
  labs(title = "Number of Shipwrecks Per Year (1850-2000)", x = "Year", y = "Number of Shipwrecks") +
  theme(plot.title = element_text(face = "bold", size = 14)) +
  annotate(geom = "text", x = 1863, y = 60, label = "Civil War (1861-1865)") +
  annotate(geom = "text", x = 1916, y = 120, label = "World War I (1914-1918)") +
  annotate(geom = "text", x = 1942, y = 360, label = "World War II (1939-1945)")

# Define world map attributes
World_Map <- list(
  scope = 'world',
  showland = TRUE,
  landcolor = toRGB("LightBlue"),
  showcountries = TRUE,
  showocean = TRUE,
  oceancolor = toRGB("gray95"),
  showlakes = TRUE,
  lakecolor = toRGB("gray95"),
  showrivers = TRUE,
  rivercolor = toRGB("gray95"))

# Filter coordinates for shipwrecks that occurred between 1850-2000
Shipwreck_Coords_1850_2000 <- Shipwreck_Coords %>%
  filter(Year_Sunk >= 1850 & Year_Sunk <= 2000)

# Overlay shipwreck coordinates on world map
Shipwreck_Map <- plot_geo(Shipwreck_Coords_1850_2000, lat = ~Lat, lon = ~Long) %>%
  add_markers(text = ~paste(Ship, paste("Year Sunk: ", Year_Sunk), sep = "<br />"),
              color = ~Year_Sunk, colors = "Reds", symbol = I("square"), size = I(8), hoverinfo = "text") %>%
  colorbar(title = "Year Sunk") %>%
  layout(title = "Shipwrecks Around the World", geo = World_Map)

# View Outputs ------------------------------------------------------------
View(Years_With_Most_Shipwrecks)
Shipwreck_Map
Shipwrecks_Per_Year
