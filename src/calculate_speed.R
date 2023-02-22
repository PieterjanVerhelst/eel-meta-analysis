# Calculate migration speeds between consecutive detection stations
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)
library(magrittr)
library(lubridate)
library(here)
library(actel)
#browseVignettes("actel")

# Source functions
source("./src/calculate_speed_function.R")
source("./src/calculate_sourcedistance_function.R")

# Read residency dataset per animal project
residency <- read_csv("./data/interim/residencies/residency_noordzeekanaal.csv")
residency$...1 <- NULL

# Load distance matrix
# Make sure the first column is not containing the station names
distance_matrix <- read.csv("./data/external/distance_matrices/distancematrix_noordzeekanaal.csv",  row.names = 1, check.names=FALSE)

# Calculate speed without taking into account different tag_id
#speed <- movementSpeeds(residency, "last to first", distance_matrix)

# Turn dataset into list per tag_id
residency_list <- split(residency , f = residency$acoustic_tag_id)
#sapply(residency_list, function(x) max(x$detections))

# Calculate speed per tag_id
speed_list <- lapply(residency_list,
                     function(x) {
                       movementSpeeds(x, "last to first", distance_matrix)
                     })
#speed_list[[1]]

# Turn lists back into dataframe
#speed <- do.call(rbind.data.frame, speed)
speed <- plyr::ldply (speed_list, data.frame)
speed$.id <- NULL

# Calculate total swim distance per acoustic_tag_id
speed <- speed %>% 
  group_by(acoustic_tag_id) %>%
  mutate(totaldistance_m=cumsum(coalesce(swimdistance_m, 0)))

# Set row with release station at 0 total distance
speed <- speed %>% 
  group_by(acoustic_tag_id) %>% 
  mutate(totaldistance_m = ifelse(row_number() == 1 | all(is.na(totaldistance_m)), 0, totaldistance_m)) %>%
  ungroup()

# Replace the remaining NA values with previous non-NA value
speed$totaldistance_m <- zoo::na.locf(speed$totaldistance_m)

# Get upstream stations for each releasing station
upstream_stations <- read_csv(here("data", "external", "station_order.csv"))
upstream_stations_tidy <- upstream_stations %>%
  separate_rows(.data$upstream_stations, sep = ", ")

# check upstream stations 
walk(unique(upstream_stations_tidy$upstream_stations), ~ 
       assert_that(. %in% names(distance_matrix),
                   msg = paste(., "is not present in the distance matrix."))
)

# Get release station for each fish
release_stations <- 
  speed %>%
  distinct(acoustic_tag_id, station_name) %>%
  mutate(is_a_releasing_station = str_detect(station_name, pattern = "rel")) %>%
  filter(is_a_releasing_station == TRUE) %>%
  select(-is_a_releasing_station) %>%
  rename(release_station = station_name)

# Check that all fishes have one and only one release_station
nrow(release_stations) == length(unique(speed$acoustic_tag_id))

# Add release station to speed
speed <- speed %>%
  left_join(release_stations, by = "acoustic_tag_id")

# Calculate the distance to the station from a 'source' station
nested_speed <- speed %>%
  group_by(acoustic_tag_id) %>%
  nest()

speed <- nested_speed %>%
  mutate(mov = map(data,
                   distanceSource, 
                   distance.method = "last to first",
                   dist.mat = distance_matrix,
                   upstream = upstream_stations_tidy)) %>%
  select(acoustic_tag_id, mov) %>%
  unnest(cols = c(mov))

# Set 'NA' to '0'
speed <- speed %>% 
  replace_na(list(distance_to_source_m = 0))

                     

# Write csv
write.csv(speed, "./data/interim/speed/speed_noordzeekanaal.csv")


