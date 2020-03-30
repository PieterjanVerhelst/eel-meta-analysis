# Attach detection and meta data of project on the River Stour
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be

# Packages
library(tidyverse)



# 1. Detection data ####

# Upload ETN data
data <- read_csv("./data/raw/raw_detection_data.csv")
data$X1 <- NULL

# Upload Stour Data
stour <- read_csv("./data/raw/stour/stour_data.csv")




# 2. Eel meta-data ####

# Upload ETN meta-data
eels <- read_csv("./data/raw/eel_meta_data.csv")
