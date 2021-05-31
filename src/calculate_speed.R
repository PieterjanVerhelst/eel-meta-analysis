# Calculate migration speeds between consecutive detection stations
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)
library(lubridate)
library(actel)

browseVignettes("actel")


# Filter Gudena detection data
gudena <- read_csv("./data/interim/residencies/residency_2004_gudena.csv")
gudena$X1 <- NULL

# Load distance matrix
gudena_distances <- read_csv("./data/external/distance_matrices/distancematrix_2004_gudena.csv")




