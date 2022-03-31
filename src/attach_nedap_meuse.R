# Attach detection and meta data of NEDAP project on the River Meuse
# by Pieterjan Verhelst
# Pieterjan.Verhelst@inbo.be

# Packages
library(tidyverse)
library(lubridate)


source("./src/attach_stour.R")


# 1. Eel meta-data ####

# Upload nedap_meuse data
nedap_eels <- read_csv("./data/raw/nedap_meuse/nedap_meuse_eel_meta.csv")
nedap_eels <- select(nedap_eels, projectName,
                     tag,
                     catchedDateTime, captureLocation, captureLatitude, captureLongitude, captureMethod,
                     utcReleaseDateTime, releaseLocation, releaseLatitude, releaseLongitude,
                     lengthType, length, lengthUnits,
                     length2Type, length2, length2Units,
                     length3Type, length3, length3Units,
                     length4Type, length4, length4Units,
                     weight, weightUnits,
                     age, ageUnits, sex, lifeStage, implantType)

nedap_eels <- nedap_eels %>%
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
eels <- rbind(eels, nedap_eels)



# 2. Deployment meta-data ####

# Load nedap_meuse deployments
nedap_deployments <- read_csv("./data/raw/nedap_meuse/nedap_meuse_deployments.csv")
nedap_deployments <- select(nedap_deployments, 
                            projectName,
                            stationName,
                            deployLat,
                            deployLong)

nedap_deployments <- nedap_deployments %>%
  rename(acoustic_project_code = projectName,
         station_name = stationName,
         deploy_latitude = deployLat,
         deploy_longitude = deployLong)

# Remove stations with no detections of Meuse NEDAP eels
nedap_deployments <- filter(nedap_deployments, station_name != "HV_Stellendam_scheep",
                             station_name != "Nederrijn_Hagestein",
                             station_name != "Ijssel_Kampen",
                             station_name != "Ijsselmeer_Den_Oever",
                             station_name != "Ijsselmeer_Kornwerderzand",
                             station_name != "Noordzeekanaal_IJmuiden_gemaal",
                             station_name != "Noordzeekanaal_Velsen",
                             station_name != "Roer_Roermond",
                             station_name != "Waal_Brakel",
                            station_name != "NZK_IJmuiden_spui",
                            station_name != "Roer_Sint_Odili?ber",
                            station_name != "Rijn_Xanten",
                            station_name != "Nederrijn_Arnhem",
                            station_name != "Nederrijn_Maurik_WKC_ben",
                            station_name != "Lek_Nieuwegein")

# Bind datasets
deployments <- rbind(deployments, nedap_deployments)

write.csv(deployments, "./data/interim/deployments.csv")



# 3. Detection data ####

# Load data with English headers
data_en <-  list.files(path = "./data/raw/nedap_meuse/raw_nedap_data/EN_headers/",
                       pattern = "*.csv", 
                       full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

data_en$animal_project_code <- "nedap_meuse"
data_en$Date <- mdy(data_en$Date)
data_en$date_time <- str_c(data_en$Date, " ", data_en$Time)
data_en$date_time <- ymd_hm(data_en$date_time)
data_en <- rename(data_en, 
                  acoustic_tag_id = ID,
                  station_name = Stationname)

data_en <- select(data_en, animal_project_code, date_time, acoustic_tag_id, station_name)


# Load data with Dutch headers
data_nl <-  list.files(path = "./data/raw/nedap_meuse/raw_nedap_data/NL_headers/",
                       pattern = "*.csv", 
                       full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

data_nl$animal_project_code <- "nedap_meuse"
data_nl$Datum <- sub('/','-', data_nl$Datum)  # Run it 2 times to replace 2 '/'
data_nl$Datum <- sub('/','-', data_nl$Datum)
data_nl$Datum <- dmy(data_nl$Datum)
data_nl$date_time <- str_c(data_nl$Datum, " ", data_nl$Tijd)
data_nl$date_time <- ymd_hm(data_nl$date_time)
data_nl <- rename(data_nl, 
                  acoustic_tag_id = Visnr,
                  station_name = Stationnaam)

data_nl <- select(data_nl, animal_project_code, date_time, acoustic_tag_id, station_name)


data_nedap <- rbind(data_en, data_nl)


# Add coordinates to detection data based on deployment file
# Load nedap_meuse deployments
nedap_deployments <- read_csv("./data/raw/nedap_meuse/nedap_meuse_deployments.csv")
nedap_deployments <- select(nedap_deployments,
                            stationName,
                            receiver,
                            deployLat,
                            deployLong)

nedap_deployments <- nedap_deployments %>%
  rename(station_name = stationName,
         receiver_id = receiver,
         deploy_latitude = deployLat,
         deploy_longitude = deployLong)

# Remove stations with no detections of Meuse NEDAP eels
nedap_deployments <- filter(nedap_deployments, station_name != "HV_Stellendam_scheep",
                            station_name != "Nederrijn_Hagestein",
                            station_name != "Ijssel_Kampen",
                            station_name != "Ijsselmeer_Den_Oever",
                            station_name != "Ijsselmeer_Kornwerderzand",
                            station_name != "Noordzeekanaal_IJmuiden_gemaal",
                            station_name != "Noordzeekanaal_Velsen",
                            station_name != "Roer_Roermond",
                            station_name != "Waal_Brakel",
                            station_name != "NZK_IJmuiden_spui",
                            station_name != "Roer_Sint_Odili?ber",
                            station_name != "Rijn_Xanten",
                            station_name != "Nederrijn_Arnhem",
                            station_name != "Nederrijn_Maurik_WKC_ben",
                            station_name != "Lek_Nieuwegein")

data_nedap <- merge(data_nedap, nedap_deployments, by = "station_name")  # Since not all deployments are in detection file, records are removed

# Only take into account tag IDs of eels in the eel meta data file
data_nedap$acoustic_tag_id <- factor(data_nedap$acoustic_tag_id)
nedap_eels$acoustic_tag_id <- factor(nedap_eels$acoustic_tag_id)
list_nedap_eels <- unique(nedap_eels$acoustic_tag_id)

data_nedap <- data_nedap[data_nedap$acoustic_tag_id %in% list_nedap_eels, ] 


# Bind datasets
data <- rbind(data, data_nedap)









