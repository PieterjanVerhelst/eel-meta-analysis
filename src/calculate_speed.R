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
animal_project_id <- "nedap_meuse"
residency <- read_csv(
  sprintf("./data/interim/residencies/residency_%s.csv", animal_project_id)
)
residency$...1 <- NULL
residency$acoustic_tag_id <- factor(residency$acoustic_tag_id)

# Clean residency data according to 'animal_project_code' ####
# ESGL ####
# False detection in project ESGL after 2016-02-15 and the detection of eel A69-1601-38319 at station A on 2016-01-02 06:59:09
residency <- residency[!(residency$animal_project_code == "ESGL" & residency$arrival >= '2016-02-15 00:00:00'),]
residency <- residency[!(residency$animal_project_code == "ESGL" & residency$acoustic_tag_id == "A69-1601-38319" &
                           residency$arrival >= '2016-01-02 06:59:09' &
                           residency$arrival <= '2016-01-02 07:59:09'),]

# 2011_warnow ####
# False detections of station W3 in 2011 warnow project: detections at W3 same day as release, while W3 is ca. 25 km downstream of release
# see github issue https://github.com/PieterjanVerhelst/eel-meta-analysis/issues/20
residency$date <- as.Date(residency$arrival)
residency <- residency %>%
  group_by(acoustic_tag_id) %>%
  mutate(start_date = dplyr::first(date)) %>%
  filter((station_name != "W3") | (station_name == "W3" & date != start_date)) %>%
  filter((station_name != "W4") | (station_name == "W4" & date != start_date))

residency$date <- NULL
residency$start_date <- NULL

#Seven eels within the 2011_warnow project had dubious detections at station W3. They seem to 'jump' from upper stations to W3 and back, spanning over 20 km over very short time periods which is highly unlikely for eels. Hence, these detections at station W3 are considered false detections and need to be removed. The eels having such false detections are 539, 542, 555, 570, 620, 633 and 649.
#https://github.com/PieterjanVerhelst/eel-meta-analysis/issues/25

residency$arrival_numeric <- as.numeric(residency$arrival)  # Set numeric: works easier to remove line

residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-539" &
                           residency$arrival_numeric == 1309160880),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-542" &
                           residency$station_name == "W3"),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-555" &
                           residency$arrival_numeric > 1309812238 &
                           residency$arrival_numeric < 1310376350),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-570" &
                           residency$arrival_numeric == 1311038460),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-570" &
                           residency$arrival_numeric == 1311053160),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-570" &
                           residency$arrival_numeric == 1311064800),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-570" &
                           residency$arrival_numeric > 1311064899 &
                           residency$arrival_numeric < 1311282481),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-620" &
                           residency$arrival_numeric == 1319453082),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-633" &
                           residency$arrival_numeric == 1319043271),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-649" &
                           residency$arrival_numeric > 1319670143 &
                           residency$arrival_numeric < 1319732079),]

residency$arrival_numeric <- NULL # Remove column

# Remove three eels that identified as American eel (Anguilla rostrata)
residency <- residency %>%
  filter(animal_project_code == "2011_Warnow" & acoustic_tag_id != "A69-1601-582",
         animal_project_code == "2011_Warnow" & acoustic_tag_id != "A69-1601-632",
         animal_project_code == "2011_Warnow" & acoustic_tag_id != "A69-1601-634")

# 2013_albertkanaal ####
# Remove wrong release location in 2013_albertkanaal
# No need to adjust exact release location, since eels were released next to receiver, leading to detection right after release
# --> exact release positions was HH5 instead of rel_albertkanaal2
residency <- subset(residency, station_name != "rel_albertkanaal2")


# nedap_meuse ####
# For 58 eels from the NEDAP Meuse project tagged in 2013 the release date is wrong: this should be 2013-09-11 instead of 2013-11-09.

residency$arrival_numeric <- as.numeric(residency$arrival)  # Set arrival to numeric as this makes filtering/subsetting easier

wrong_release_dates <- residency[(residency$station_name == "rel_nedap_meuse" & residency$arrival_numeric == 1383951600),] # Isolate  records with wrong release dates
unique(wrong_release_dates$acoustic_tag_id)   # Gives number of unique tag IDs
wrong_release_dates$arrival <- '2013-09-11 00:00:00'  # Correct release date
wrong_release_dates$departure <- '2013-09-11 00:00:00'  # Correct release date


residency <- residency[!(residency$station_name == "rel_nedap_meuse" & residency$arrival_numeric == 1383951600),]  # Remove records with wrong release dates
residency <- rbind(residency, wrong_release_dates)  # Bind records with correct release dates

residency$arrival_numeric <- NULL   # Remove column with arrival numeric which became now redundant


#####


# Load distance matrix
# Make sure the first column is not containing the station names
distance_matrix <- read.csv(
  sprintf("./data/external/distance_matrices/distancematrix_%s.csv",
          animal_project_id),
  row.names = 1, 
  check.names=FALSE
)

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
  separate_rows(upstream_stations, sep = ", ")

# check upstream stations are in distance matrix
upstream_stations_vect <- upstream_stations_tidy %>%
  filter(animal_project_code == animal_project_id) %>%
  distinct(upstream_stations) %>%
  pull(upstream_stations)

upstream_stations_error <- upstream_stations_vect[!upstream_stations_vect %in% names(distance_matrix)]
assert_that(all(upstream_stations_vect %in% names(distance_matrix)),
            msg = paste0("upstream stations not in the distance matrix: ",
                        paste(upstream_stations_error, collapse = ","),
                        ".",
                        collapse = ""
            )
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
write.csv(speed, sprintf("./data/interim/speed/speed_%s.csv",animal_project_id))
