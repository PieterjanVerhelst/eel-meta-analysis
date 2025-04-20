# Download data from silver eel meta-analysis from ETN database via RStudio LifeWatch server
# by Pieterjan Verhelst
# Pieterjan.Verhelst@inbo.be


# Install and load packages
install.packages("devtools") 
install.packages("digest")
#install.packages("processx")
devtools::install_github("inbo/etn")

library(dplyr)
library(etn)


# Database connection
my_con <- connect_to_etn("login",      # Add login to connect to ETN database
                         "password")   # Add password to connect to ETN database
my_con


# Create vector with relevant animal_project_codes
animal_projects <- c("2015_phd_verhelst_eel", 
                     "2012_leopoldkanaal",
                     "PTN-Silver-eel-Mondego",
                     "SEMP",
                     "DAK",
                     "EMMN",
                     "Noordzeekanaal",
                     "2011_Warnow",
                     "2017_Fremur",
                     "2011_Loire",
                     "2019_Grotenete",
                     "ESGL",
                     "2013_albertkanaal",
                     "2004_Gudena",
                     "2014_Frome",
                     "2014_Nene"
)


# 1. Download detection data ####
data <- get_acoustic_detections(my_con, scientific_name = "Anguilla anguilla",
                       animal_project_code = animal_projects,
                       limit = FALSE)

# Select relevant columns
data <- select(data, animal_project_code, date_time, acoustic_tag_id, station_name, receiver_id, deploy_latitude, deploy_longitude)



# 2. Download eel meta-data ####
eels <- get_animals(my_con, scientific_name = "Anguilla anguilla", 
                    animal_project_code = animal_projects)

# Select relevant columns
eels <- select(eels, animal_project_code, scientific_name, acoustic_tag_id,
               capture_date_time, capture_location, capture_latitude, capture_longitude, capture_method,
               release_date_time, release_location, release_latitude, release_longitude,
               length1_type, length1, length1_unit,
               length2_type, length2, length2_unit,
               length3_type, length3, length3_unit,
               length4_type, length4, length4_unit,
               weight, weight_unit,
               age, age_unit, sex, life_stage, 
               treatment_type)


# 3. Download deployment positions ####
network_projects <- c("lifewatch",
                      "2013_Maas",
                      "dijle",
                      "albert",
                      "leopold",
                      "bpns",
                      "demer",
                      "ws1",
                      "ws2",
                      "ws3",
                      "zeeschelde",
                      "PTN-Silver-eel-Mondego",
                      "Albertkanaal_VPS_Ham",
                      "SEMP",
                      "DAK",
                      "EMMN",
                      "Noordzeekanaal",
                      "2017_Fremur",
                      "2011_Loire",
                      "2019_Grotenete",
                      "2011_Warnow",
                      "ESGL",
                      "2014_Frome",
                      "2004_Gudena",
                      "2014_Nene")

deployments <- get_acoustic_deployments(my_con, acoustic_project_code = network_projects, open_only = FALSE)
deployments$station_name <- factor(deployments$station_name)

# Get unique deployments
unique_deployments <- deployments %>% 
  group_by(acoustic_project_code) %>%
  distinct(station_name, .keep_all = TRUE) 

# Select relevant columns
unique_deployments <- select(unique_deployments, acoustic_project_code, station_name, deploy_latitude, deploy_longitude)


# Write csv files
write.csv(data, "raw_detection_data.csv")
write.csv(eels, "eel_meta_data.csv")
write.csv(unique_deployments, "deployments.csv")



