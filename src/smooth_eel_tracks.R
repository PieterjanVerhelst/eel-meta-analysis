# Eel track smoothing and residence time calculation
# By Damiano Oldoni
# Check .Rmd file in ./docs for documentation

library(readr)
library(assertthat)
library(magrittr)
library(dplyr)
library(lubridate)
library(tibble)
library(purrr)


# Source functions
source("./src/get_nearest_stations.R")
source("./src/get_timeline.r")


# Load data
data <- read_csv("./data/interim/detection_data.csv")
data$...1 <- NULL

# Animal project codes
#unique(data$animal_project_code)
#[1] "2012_leopoldkanaal"     "SEMP"                   "PTN-Silver-eel-Mondego" "2015_phd_verhelst_eel"  "Noordzeekanaal"     
#[6] "DAK"                    "EMMN"                   "2004_Gudena"            "2017_Fremur"            "2013_albertkanaal"  
#[11] "2011_Loire"             "2011_Warnow"            "2014_Frome"             "2014_Nene"              "2019_Grotenete"    
#[16] "ESGL"                   "life4fish"              "2013_Stour"             "nedap_meuse"       

# Filter project detection data
subset <- filter(data, animal_project_code == "SEMP")
head(subset)


# Filter DAK project and eels for Suderpolder
#subset <- filter(data, animal_project_code == "DAK" ,
#                          acoustic_tag_id == "A69-1602-10817" | 
#                            acoustic_tag_id == "A69-1602-10818" | 
#                            acoustic_tag_id == "A69-1602-10819" | 
#                            acoustic_tag_id == "A69-1602-10820" | 
#                            acoustic_tag_id == "A69-1602-10821" | 
#                            acoustic_tag_id == "A69-1602-10822" | 
#                            acoustic_tag_id == "A69-1602-10823" | 
#                            acoustic_tag_id == "A69-1602-10824" | 
#                            acoustic_tag_id == "A69-1602-10825" | 
#                            acoustic_tag_id == "A69-1602-10826" | 
#                            acoustic_tag_id == "A69-1602-10827" | 
#                            acoustic_tag_id == "A69-1602-10828" | 
#                            acoustic_tag_id == "A69-1602-10829" | 
#                            acoustic_tag_id == "A69-1602-10830" | 
#                            acoustic_tag_id == "A69-1602-10831" | 
#                            acoustic_tag_id == "A69-1602-10857" | 
#                            acoustic_tag_id == "A69-1602-10858" | 
#                            acoustic_tag_id == "A69-1602-10859" | 
#                            acoustic_tag_id == "A69-1602-10860" | 
#                            acoustic_tag_id == "A69-1602-10861" | 
#                            acoustic_tag_id == "A69-1602-10862" | 
#                            acoustic_tag_id == "A69-1602-10863" | 
#                            acoustic_tag_id == "A69-1602-10864" | 
#                            acoustic_tag_id == "A69-1602-10865" | 
#                            acoustic_tag_id == "A69-1602-10866" 
#)

# Filter DAK project and eels for Markiezaatsmeer
#subset <- filter(data, animal_project_code == "DAK" ,
#                         acoustic_tag_id == "A69-1602-10832" | 
#                           acoustic_tag_id == "A69-1602-10833" | 
#                           acoustic_tag_id == "A69-1602-10834" | 
#                           acoustic_tag_id == "A69-1602-10835" | 
#                           acoustic_tag_id == "A69-1602-10836" | 
#                           acoustic_tag_id == "A69-1602-10837" | 
#                           acoustic_tag_id == "A69-1602-10838" | 
#                           acoustic_tag_id == "A69-1602-10839" | 
#                           acoustic_tag_id == "A69-1602-10840" | 
#                           acoustic_tag_id == "A69-1602-10841" | 
#                           acoustic_tag_id == "A69-1602-10842" | 
#                           acoustic_tag_id == "A69-1602-10843" | 
#                           acoustic_tag_id == "A69-1602-10844" | 
#                           acoustic_tag_id == "A69-1602-10845" | 
#                           acoustic_tag_id == "A69-1602-10846" | 
#                           acoustic_tag_id == "A69-1602-10847" | 
#                           acoustic_tag_id == "A69-1602-10848" | 
#                           acoustic_tag_id == "A69-1602-10849" | 
#                           acoustic_tag_id == "A69-1602-10850" | 
#                           acoustic_tag_id == "A69-1602-10851" | 
#                           acoustic_tag_id == "A69-1602-10852" | 
#                           acoustic_tag_id == "A69-1602-10853" | 
#                           acoustic_tag_id == "A69-1602-10854" | 
#                           acoustic_tag_id == "A69-1602-10855" | 
#                           acoustic_tag_id == "A69-1602-10856" 
#)

# Add 'count' column
subset$counts <- 1

# Good practice to sort the dataset
subset %<>% arrange(acoustic_tag_id, date_time)

# Import distance matrix
distance_matrix <- read_csv("./data/external/distance_matrices/distancematrix_semp.csv")


# Extract eel codes
eels_all <- subset %>% 
  select(acoustic_tag_id) %>% 
  unique()
eels_all <- eels_all[[1]]
eels_all
n_eels <- length(eels_all)


# Extract stations
stations_all <- subset %>% 
  select(station_name) %>%
  unique()
stations_all <- stations_all[[1]]
stations_all


# Rename column `...1` to `station` in distance dataframe
distance_matrix %<>% rename(station_name = ...1)
head(distance_matrix)


# Select from `distance` only the distance among stations we need and overwrite it:
distance_all <- distance_matrix %<>% 
  select(station_name, which(colnames(distance_matrix) %in% stations_all)) %>%
  filter(station_name %in% stations_all)
distance_all


# Before proceeding smoothing the raw data and removing duplicates, we should be
# sure that stations present in the data are present in the distance matrix as
# well!
assert_that(
  all(stations_all %in% colnames(distance_all)),
  msg = cat("These stations are not present in distance matrix:",
            stations_all[which(!stations_all %in% colnames(distance_all))]))


# Set temporal and distance treshold
max_limit <- 3600 # seconds
max_dist <- 1005 #  meters; based on detection range


# For each eel, the nearest stations are found by `get_nearest_stations()` and
# saved in a list, `near_stations`
near_stations_all <- purrr::map(stations_all, 
                                function(x) 
                                  distance_all %>%
                                  filter(station_name == x) %>%
                                  select(-station_name) %>%
                                  get_nearest_stations(limit = max_dist))
names(near_stations_all) <- stations_all


# For each eel, smoothing is applied by calling function `get_timeline`
tracks <- purrr::imap(eels_all, 
                     function(eel, index) {
                       message <- paste0(
                         "Analyse timeseries of ", eel,
                         " (",index,"/", n_eels,")"
                       )
                       message(message)
                       get_timeline(subset, 
                                    proxy_stations = near_stations_all,
                                    eel = eel, verbose = FALSE)
                     })

# You get a list of data.frames. You can view them separately
View(tracks[[5]])

# Give eels "names" to tracks list
names(tracks) <- eels_all


## In case you want to run the smoothing only for one eel from `eels` (e.g; the first one with `code = "A69-1601-52622"`) and modify default parameters `max_limit` (e.g. 1 hour) and `verbose` (TRUE):
#track_A69_1601_28264_1h <- get_timeline(dfk, 
#                                        proxy_stations = near_stations_all,
#                                        eel = eels_all[1], 
#                                        limit = 3600, 
#                                        verbose = TRUE) 
#head(track_A69_1601_28264_1h, n = 20)





## Arrange dataset ##

# Merge list of dataframes into 1 dataframe
residency <- do.call(rbind.data.frame, tracks)

# Rename columns 
residency <- rename(residency, arrival = start)
residency <- rename(residency, departure = end)
residency <- rename(residency, detections = counts)


# Change order of columns
residency <- residency[, c("animal_project_code", "acoustic_tag_id","station_name","receiver_id", "deploy_latitude", "deploy_longitude", "arrival", "departure","detections")]



# Write csv
write.csv(residency, "./data/interim/residencies/residency_semp.csv")

