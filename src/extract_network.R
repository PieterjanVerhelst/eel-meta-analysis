# Extract receiver networks based on detections per project
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


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
                            tag_id == "A69-1602-10817" | 
                            tag_id == "A69-1602-10818" | 
                            tag_id == "A69-1602-10819" | 
                            tag_id == "A69-1602-10820" | 
                            tag_id == "A69-1602-10821" | 
                            tag_id == "A69-1602-10822" | 
                            tag_id == "A69-1602-10823" | 
                            tag_id == "A69-1602-10824" | 
                            tag_id == "A69-1602-10825" | 
                            tag_id == "A69-1602-10826" | 
                            tag_id == "A69-1602-10827" | 
                            tag_id == "A69-1602-10828" | 
                            tag_id == "A69-1602-10829" | 
                            tag_id == "A69-1602-10830" | 
                            tag_id == "A69-1602-10831" | 
                            tag_id == "A69-1602-10857" | 
                            tag_id == "A69-1602-10858" | 
                            tag_id == "A69-1602-10859" | 
                            tag_id == "A69-1602-10860" | 
                            tag_id == "A69-1602-10861" | 
                            tag_id == "A69-1602-10862" | 
                            tag_id == "A69-1602-10863" | 
                            tag_id == "A69-1602-10864" | 
                            tag_id == "A69-1602-10865" | 
                            tag_id == "A69-1602-10866" 
                            )
distinct_stations <- dak_superpolder %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)

#write.csv(distinct_stations, "./data/interim/receivernetworks/receivernetwork_DAK_SUPERPOLDER.csv", row.names=FALSE)



# DAK_MARKIEZAAT ####
dak_markiezaat <- filter(data, animal_project_code == "DAK" ,
                          tag_id == "A69-1602-10832" | 
                            tag_id == "A69-1602-10833" | 
                            tag_id == "A69-1602-10834" | 
                            tag_id == "A69-1602-10835" | 
                            tag_id == "A69-1602-10836" | 
                            tag_id == "A69-1602-10837" | 
                            tag_id == "A69-1602-10838" | 
                            tag_id == "A69-1602-10839" | 
                            tag_id == "A69-1602-10840" | 
                            tag_id == "A69-1602-10841" | 
                            tag_id == "A69-1602-10842" | 
                            tag_id == "A69-1602-10843" | 
                            tag_id == "A69-1602-10844" | 
                            tag_id == "A69-1602-10845" | 
                            tag_id == "A69-1602-10846" | 
                            tag_id == "A69-1602-10847" | 
                            tag_id == "A69-1602-10848" | 
                            tag_id == "A69-1602-10849" | 
                            tag_id == "A69-1602-10850" | 
                            tag_id == "A69-1602-10851" | 
                            tag_id == "A69-1602-10852" | 
                            tag_id == "A69-1602-10853" | 
                            tag_id == "A69-1602-10854" | 
                            tag_id == "A69-1602-10855" | 
                            tag_id == "A69-1602-10856" 
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


