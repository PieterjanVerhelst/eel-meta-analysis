# Add eel release positions and date-time to detection dataset
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)
library(lubridate)


# 1. Read eel meta data
eel <- read_csv("./data/interim/eel_meta_data.csv")

eel <- select(eel, 
              animal_project_code, 
              scientific_name, 
              release_date_time, 
              tag_id, 
              release_location, 
              release_latitude, 
              release_longitude)


eel$release_location <- factor(eel$release_location)

# 2. Remove eel from saeftinghe
eel <- eel[!(eel$animal_project_code == "2015_phd_verhelst_eel" & eel$tag_id == "A69-1601-58620"),]


# 3. Read file with release location and station
release <- read_csv("./data/external/release_locations_stations.csv")

release$release_location <- factor(release$release_location)
release$release_station <- factor(release$release_station)

# 4. Merge release station with eel data
eel <- left_join(eel, release, by = "release_location")

# 5. Process eel dataset column names
eel$receiver_id <- "none"

eel <- select(eel,
              animal_project_code, 
              scientific_name, 
              release_date_time, 
              tag_id, 
              release_station,
              receiver_id,
              release_latitude, 
              release_longitude)

eel <- rename(eel,
              date_time = release_date_time,
              station_name = release_station,
              deploy_latitude = release_latitude,
              deploy_longitude = release_longitude)

# 6. Merge eel releases to the detection dataset
data <- rbind(data, eel)


# 7. Substitute code space into 'Vemco' format
# 2011_Loire
data$tag_id <- gsub("R04K", "A69-1206", data$tag_id)

# 2017_Fremur
data$tag_id <- gsub("S256", "A69-1105", data$tag_id)







