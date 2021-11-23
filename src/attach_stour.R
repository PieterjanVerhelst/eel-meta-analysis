# Attach detection and meta data of project on the River Stour
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be

# Packages
library(tidyverse)
library(lubridate)


# 1. Detection data ####

# Upload ETN data
data <- read_csv("./data/raw/raw_detection_data.csv")
data$...1 <- NULL

# Upload Stour data
stour_detections <- read_csv("./data/raw/stour/stour_data.csv")
stour_detections$date_time <- dmy_hm(stour_detections$date_time)
stour_detections <- rename(stour_detections, acoustic_tag_id = tag_id)
stour_detections$scientific_name <- NULL

# Bind datasets
data <- rbind(data, stour_detections)

rm(stour_detections)



# 2. Eel meta-data ####

# Upload ETN meta-data
eels <- read_csv("./data/raw/eel_meta_data.csv")
eels$...1 <- NULL
eels$scientific_name <- NULL

# Upload Stour data
stour_eels <- read_csv("./data/raw/stour/stour_eel_meta.csv")
stour_eels <- select(stour_eels, projectName,
                     tag,
                     catchedDateTime, captureLocation, captureLatitude, captureLongitude, captureMethod,
                     utcReleaseDateTime, releaseLocation, releaseLatitude, releaseLongitude,
                     lengthType, length, lengthUnits,
                     length2Type, length2, length2Units,
                     length3Type, length3, length3Units,
                     length4Type, length4, length4Units,
                     weight, weightUnits,
                     age, ageUnits, sex, lifeStage, implantType)

stour_eels <- stour_eels %>%
  rename(animal_project_code = projectName,
         acoustic_tag_id = tag,
         capture_date_time = catchedDateTime,
         capture_location = captureLocation,
         capture_latitude = captureLatitude,
         capture_longitude = captureLongitude,
         capture_method = captureMethod,
         release_date_time = utcReleaseDateTime,
         release_location = releaseLocation,
         release_latitude = releaseLatitude,
         release_longitude = releaseLongitude,
         length1_type = lengthType,
         length1 = length,
         length1_unit = lengthUnits,
         length2_type = length2Type,
         length2_unit = length2Units,
         length3_type = length3Type,
         length3_unit = length3Units,
         length4_type = length4Type,
         length4_unit = length4Units,
         weight_unit = weightUnits,
         age_unit = ageUnits,
         life_stage = lifeStage,
         treatment_type = implantType)


# Bind datasets
eels <- rbind(eels, stour_eels)

rm(stour_eels)



# 3. Deployment meta-data ####

# Upload ETN meta-data
deployments <- read_csv("./data/raw/deployments.csv")
deployments$...1 <- NULL

# Upload Stour data
stour_deployments <- read_csv("./data/raw/stour/stour_deployments.csv")
stour_deployments <- select(stour_deployments, 
                            projectName,
                            stationName,
                            deployLat,
                            deployLong)

stour_deployments <- stour_deployments %>%
  rename(acoustic_project_code = projectName,
         station_name = stationName,
         deploy_latitude = deployLat,
         deploy_longitude = deployLong)


# Bind datasets
deployments <- rbind(deployments, stour_deployments)

rm(stour_deployments)


# write csv of deployments
write.csv(deployments, "./data/interim/deployments.csv")

