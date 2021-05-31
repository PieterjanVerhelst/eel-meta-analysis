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


# Filter Gudena detection data
gudena <- filter(data, animal_project_code == "2004_Gudena")
head(gudena)

# Add 'count' column
gudena$counts <- 1

# Good practice to sort the dataset
gudena %<>% arrange(tag_id, date_time)

# Import distance matrix
gudena_distances <- read_csv("./data/external/distance_matrices/distancematrix_2004_gudena.csv")



# Extract eel codes
eels_all <- gudena %>% 
  select(tag_id) %>% 
  unique()
eels_all <- eels_all[[1]]
eels_all


# Extract stations
stations_all <- gudena %>% 
  select(station_name) %>%
  unique()
stations_all <- stations_all[[1]]
stations_all


# Rename column `X1` to `station` in distance dataframe
gudena_distances %<>% rename(station_name = X1)
head(gudena_distances)


# Select from `distance` only the distance among stations we need and overwrite it:
distance_all <- gudena_distances %<>% select(station_name, 
                                     which(colnames(gudena_distances) %in% stations_all)) %>%
  filter(station_name %in% stations_all)
distance_all


# Before proceeding smoothing the raw data and removing duplicates, we should be sure that stations present in the data are present in the distance matrix as well!
assert_that(
  all(stations_all %in% colnames(distance_all)),
  msg = cat("These stations are not present in distance matrix:",
            stations_all[which(!stations_all %in% colnames(distance_all))]))


# Set temporal and distance treshold
max_limit <- 3600 # seconds
max_dist <- 1005 #  meters; based on detection range


# For each eel, the nearest stations are found by `get_nearest_stations()` and saved in a list, `near_stations`
near_stations_all <- purrr::map(stations_all, 
                                function(x) 
                                  distance_all %>%
                                  filter(station_name == x) %>%
                                  select(-station_name) %>%
                                  get_nearest_stations(limit = max_dist))
names(near_stations_all) <- stations_all


# For each eel, smoothing is applied by calling function `get_timeline`
tracks <- purrr::map(eels_all, 
                     function(eel) 
                       get_timeline(gudena, 
                                    proxy_stations = near_stations_all,
                                    eel = eel, verbose = FALSE))


# You get a list of data.frames. You can view them separately
View(tracks[[5]])


## In case you want to run the smoothing only for one eel from `eels` (e.g; the first one with `code = "A69-1601-52622"`) and modify default parameters `max_limit` (e.g. 1 hour) and `verbose` (TRUE):
track_A69_1601_28264_1h <- get_timeline(dfk, 
                                        proxy_stations = near_stations_all,
                                        eel = eels_all[1], 
                                        limit = 3600, 
                                        verbose = TRUE) 
head(track_A69_1601_28264_1h, n = 20)







## Arrange dataset ##

# Merge list of dataframes into 1 dataframe
df <- do.call(rbind.data.frame, tracks)


# Rename columns (necessary for following scripts)
df <- rename(df, Transmitter = code)
df <- rename(df, Detections = counts)
df <- rename(df, Receiver = receiver)
df <- rename(df, Arrival = start)
df <- rename(df, Departure = end)


# Change order of columns
df <- df[, c("Transmitter","Receiver","latitude","longitude","network","station","Arrival","Departure","Detections")]




write.csv(df, "./data/interim/residency_eel.csv")






