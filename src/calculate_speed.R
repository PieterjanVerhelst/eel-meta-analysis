# Calculate migration speeds between consecutive detection stations
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)
library(magrittr)
library(lubridate)
library(actel)
#browseVignettes("actel")

# Source functions
source("./src/calculate_speed_function.R")

# Filter Gudena detection data
residency <- read_csv("./data/interim/residencies/residency_2004_gudena.csv")
residency$X1 <- NULL

# Load distance matrix
# Make sure the first column is not containing the station names
distance_matrix <- read.csv("./data/external/distance_matrices/distancematrix_2004_gudena.csv",  row.names = 1)

# Calculate speed without taking into account different tag_id
#speed <- movementSpeeds(residency, "last to first", distance_matrix)

# Turn dataset into list per tag_id
residency_list <- split(residency , f = residency$tag_id )
#sapply(residency_list, function(x) max(x$detections))

# Calculate speed per tag_id
speed_list <- lapply(residency_list, function(x) movementSpeeds(x, "last to first", distance_matrix))
#speed_list[[1]]

# Turn lists back into dataframe
#speed <- do.call(rbind.data.frame, speed)
speed <- plyr::ldply (speed_list, data.frame)
speed$.id <- NULL



# Write csv
write.csv(speed, "./data/interim/speed/speed_2004_gudena.csv")


