# Extract receiver networks based on detections per project
# by Pieterjan Verhelst
# Pieterjan.Verhelst@inbo.be

# Packages
library(tidyverse)
library(lubridate)

# Load data
data <- read_csv("./data/interim/detection_data.csv")
data$...1 <- NULL


# 2015_phd_verhelst_eel ####
scheldt <- filter(data, animal_project_code == "2015_phd_verhelst_eel")
distinct_stations <- scheldt %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_2015_phd_verhelst_eel.csv", row.names=FALSE)



# 2012_leopoldkanaal ####
leopoldkanaal <- filter(data, animal_project_code == "2012_leopoldkanaal")
distinct_stations <- leopoldkanaal %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_2012_leopoldkanaal.csv", row.names=FALSE)



# 2013_albertkanaal ####
albertkanaal <- filter(data, animal_project_code == "2013_albertkanaal")
distinct_stations <- albertkanaal %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_2013_albertkanaal.csv", row.names=FALSE)



# 2019_Grotenete ####
grotenete <- filter(data, animal_project_code == "2019_Grotenete")
distinct_stations <- grotenete %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_2019_Grotenete.csv", row.names=FALSE)



# 2004_Gudena ####
gudena <- filter(data, animal_project_code == "2004_Gudena")
distinct_stations <- gudena %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_2004_Gudena.csv", row.names=FALSE)



# SEMP ####
semp <- filter(data, animal_project_code == "SEMP")
distinct_stations <- semp %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_SEMP.csv", row.names=FALSE)



# PTN-Silver-eel-Mondego ####
mondego <- filter(data, animal_project_code == "PTN-Silver-eel-Mondego")
distinct_stations <- mondego %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_PTN-Silver-eel-Mondego.csv", row.names=FALSE)



# 2014_Frome ####
frome <- filter(data, animal_project_code == "2014_Frome")
distinct_stations <- frome %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_2014_Frome.csv", row.names=FALSE)



# ESGL ####
esgl <- filter(data, animal_project_code == "ESGL")
distinct_stations <- esgl %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_ESGL.csv", row.names=FALSE)



# 2011_Warnow ####
warnow <- filter(data, animal_project_code == "2011_Warnow")
distinct_stations <- warnow %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_2011_Warnow.csv", row.names=FALSE)



# DAK_SUPERPOLDER ####
dak_superpolder <- filter(data, animal_project_code == "DAK" ,
                            acoustic_tag_id == "A69-1602-10817" | 
                            acoustic_tag_id == "A69-1602-10818" | 
                            acoustic_tag_id == "A69-1602-10819" | 
                            acoustic_tag_id == "A69-1602-10820" | 
                            acoustic_tag_id == "A69-1602-10821" | 
                            acoustic_tag_id == "A69-1602-10822" | 
                            acoustic_tag_id == "A69-1602-10823" | 
                            acoustic_tag_id == "A69-1602-10824" | 
                            acoustic_tag_id == "A69-1602-10825" | 
                            acoustic_tag_id == "A69-1602-10826" | 
                            acoustic_tag_id == "A69-1602-10827" | 
                            acoustic_tag_id == "A69-1602-10828" | 
                            acoustic_tag_id == "A69-1602-10829" | 
                            acoustic_tag_id == "A69-1602-10830" | 
                            acoustic_tag_id == "A69-1602-10831" | 
                            acoustic_tag_id == "A69-1602-10857" | 
                            acoustic_tag_id == "A69-1602-10858" | 
                            acoustic_tag_id == "A69-1602-10859" | 
                            acoustic_tag_id == "A69-1602-10860" | 
                            acoustic_tag_id == "A69-1602-10861" | 
                            acoustic_tag_id == "A69-1602-10862" | 
                            acoustic_tag_id == "A69-1602-10863" | 
                            acoustic_tag_id == "A69-1602-10864" | 
                            acoustic_tag_id == "A69-1602-10865" | 
                            acoustic_tag_id == "A69-1602-10866" 
                            )
distinct_stations <- dak_superpolder %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_DAK_SUPERPOLDER.csv", row.names=FALSE)



# DAK_MARKIEZAAT ####
dak_markiezaat <- filter(data, animal_project_code == "DAK" ,
                         acoustic_tag_id == "A69-1602-10832" | 
                           acoustic_tag_id == "A69-1602-10833" | 
                           acoustic_tag_id == "A69-1602-10834" | 
                           acoustic_tag_id == "A69-1602-10835" | 
                           acoustic_tag_id == "A69-1602-10836" | 
                           acoustic_tag_id == "A69-1602-10837" | 
                           acoustic_tag_id == "A69-1602-10838" | 
                           acoustic_tag_id == "A69-1602-10839" | 
                           acoustic_tag_id == "A69-1602-10840" | 
                           acoustic_tag_id == "A69-1602-10841" | 
                           acoustic_tag_id == "A69-1602-10842" | 
                           acoustic_tag_id == "A69-1602-10843" | 
                           acoustic_tag_id == "A69-1602-10844" | 
                           acoustic_tag_id == "A69-1602-10845" | 
                           acoustic_tag_id == "A69-1602-10846" | 
                           acoustic_tag_id == "A69-1602-10847" | 
                           acoustic_tag_id == "A69-1602-10848" | 
                           acoustic_tag_id == "A69-1602-10849" | 
                           acoustic_tag_id == "A69-1602-10850" | 
                           acoustic_tag_id == "A69-1602-10851" | 
                           acoustic_tag_id == "A69-1602-10852" | 
                           acoustic_tag_id == "A69-1602-10853" | 
                           acoustic_tag_id == "A69-1602-10854" | 
                           acoustic_tag_id == "A69-1602-10855" | 
                           acoustic_tag_id == "A69-1602-10856" 
)
distinct_stations <- dak_markiezaat %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_DAK_MARKIEZAAT.csv", row.names=FALSE)



# Noordzeekanaal ####
noordzeekanaal <- filter(data, animal_project_code == "Noordzeekanaal")
distinct_stations <- noordzeekanaal %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_Noordzeekanaal.csv", row.names=FALSE)



# 2017_Fremur ####
fremur <- filter(data, animal_project_code == "2017_Fremur")
distinct_stations <- fremur %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_2017_Fremur.csv", row.names=FALSE)



# EMMN ####
emmn <- filter(data, animal_project_code == "EMMN")
distinct_stations <- emmn %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_EMMN.csv", row.names=FALSE)



# 2014_Nene ####
nene <- filter(data, animal_project_code == "2014_Nene")
distinct_stations <- nene %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_2014_Nene.csv", row.names=FALSE)



# 2011_Loire ####
loire <- filter(data, animal_project_code == "2011_Loire")
distinct_stations <- loire %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_2011_Loire.csv", row.names=FALSE)



# 2013_Stour ####
stour <- filter(data, animal_project_code == "2013_Stour")
distinct_stations <- stour %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_2013_Stour.csv", row.names=FALSE)


# life4fish ####
life4fish <- filter(data, animal_project_code == "life4fish")
distinct_stations <- life4fish %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_life4fish.csv", row.names=FALSE)


