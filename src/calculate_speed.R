# Calculate migration speeds between consecutive detection stations
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)
library(lubridate)
library(actel)

browseVignettes("actel")



# Load distance matrix
gudena_distances <- read_csv("./data/external/distance_matrices/distancematrix_2004_gudena.csv")

# Filter Gudena detection data
gudena <- filter(data, animal_project_code == "2004_Gudena")


