# Create interactive html widget maps
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be

# Packages
library(tidyverse)
library(sf)
library(tmap)


# Load data
data <- read_csv("./data/interim/detection_data.csv")
data$...1 <- NULL

# Animal project codes
unique(data$animal_project_code)
#[1] "2012_leopoldkanaal"     "SEMP"                   "PTN-Silver-eel-Mondego" "2015_phd_verhelst_eel" 
#[5] "Noordzeekanaal"         "DAK"                    "EMMN"                   "2004_Gudena"           
#[9] "2017_Fremur"            "2013_albertkanaal"      "2011_Loire"             "2011_Warnow"           
#[13] "2014_Frome"             "2014_Nene"              "2019_Grotenete"         "ESGL"                  
#[17] "life4fish"              "2013_Stour"             "nedap_meuse"


# Return number of detections, eels and detections per eel for each project
number <- data %>%
  group_by(animal_project_code) %>%
  summarise(tot_detections = n(),
            tot_eels = n_distinct(acoustic_tag_id))
number$detpereel <- number$tot_detections  /  number$tot_eels



# Set interactive view
ttm() # switches between static plot and interactive viewing


# Create subset per project


# 2004_gudena ####
gudena <- filter(data, animal_project_code == "2004_Gudena")
gudena$day <- as.Date(gudena$date_time)
gudena <- select(gudena, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
gudena$date_time <- NULL
gudena <- distinct(gudena)   # Select unique rows to reduce size of dataset
unique(gudena$acoustic_tag_id) # 68 detected eels, 

# Create sf
spatial_gudena <- st_as_sf(gudena,
                           coords = c(6:7),
                           crs = 4326)  # WGS84

# Create and save interactive map
gudena_map <- tm_shape(spatial_gudena) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 34, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 68), max.categories = 19) 
gudena_map




# 2011_Loire ####
loire <- filter(data, animal_project_code == "2011_Loire")
loire$day <- as.Date(loire$date_time)
loire <- select(loire, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
loire$date_time <- NULL
loire <- distinct(loire)   # Select unique rows to reduce size of dataset
unique(loire$acoustic_tag_id) # 89 detected eels

# Create sf
spatial_loire <- st_as_sf(loire,
                          coords = c(6:7),
                          crs = 4326)  # WGS84

# Create and save interactive map
loire_map <- tm_shape(spatial_loire) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 45, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 90), max.categories = 19) 
loire_map





# 2011_Warnow ####
warnow <- filter(data, animal_project_code == "2011_Warnow")
warnow$day <- as.Date(warnow$date_time)
warnow <- select(warnow, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
warnow$date_time <- NULL
warnow <- distinct(warnow)   # Select unique rows to reduce size of dataset
unique(warnow$acoustic_tag_id) # 146 detected eels

# Create sf
spatial_warnow <- st_as_sf(warnow,
                           coords = c(6:7),
                           crs = 4326)  # WGS84

# Create and save interactive map
warnow_map <- tm_shape(spatial_warnow) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 73, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 146), max.categories = 19) 
warnow_map





# 2012_leopoldkanaal ####
leopoldkanaal <- filter(data, animal_project_code == "2012_leopoldkanaal")
leopoldkanaal$day <- as.Date(leopoldkanaal$date_time)
leopoldkanaal <- select(leopoldkanaal, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
leopoldkanaal$date_time <- NULL
leopoldkanaal <- distinct(leopoldkanaal)   # Select unique rows to reduce size of dataset
unique(leopoldkanaal$acoustic_tag_id) # 96 detected eels, 

# Create sf
spatial_leopoldkanaal <- st_as_sf(leopoldkanaal,
                                  coords = c(6:7),
                                  crs = 4326)  # WGS84

# Create and save interactive map
leopoldkanaal_map <- tm_shape(spatial_leopoldkanaal) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 48, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 96), max.categories = 19)
leopoldkanaal_map





# 2013_albertkanaal ####
albertkanaal <- filter(data, animal_project_code == "2013_albertkanaal")
albertkanaal$day <- as.Date(albertkanaal$date_time)
albertkanaal <- select(albertkanaal, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
albertkanaal$date_time <- NULL
albertkanaal <- distinct(albertkanaal)   # Select unique rows to reduce size of dataset
unique(albertkanaal$acoustic_tag_id) # 160 detected eels, 

# Create sf
spatial_albertkanaal <- st_as_sf(albertkanaal,
                           coords = c(6:7),
                           crs = 4326)  # WGS84

# Create and save interactive map
albertkanaal_map <- tm_shape(spatial_albertkanaal) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 80, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 160), max.categories = 19)
albertkanaal_map





# 2013_stour ####
stour <- filter(data, animal_project_code == "2013_Stour")
stour$day <- as.Date(stour$date_time)
stour <- select(stour, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
stour$date_time <- NULL
stour <- distinct(stour)   # Select unique rows to reduce size of dataset
unique(stour$acoustic_tag_id) # 101 detected eels

# Create sf
spatial_stour <- st_as_sf(stour,
                          coords = c(6:7),
                          crs = 4326)  # WGS84

# Create and save interactive map
stour_map <- tm_shape(spatial_stour) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 51, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 101), max.categories = 19) 
stour_map





# 2014_Frome ####
frome <- filter(data, animal_project_code == "2014_Frome")
frome$day <- as.Date(frome$date_time)
frome <- select(frome, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
frome$date_time <- NULL
frome <- distinct(frome)   # Select unique rows to reduce size of dataset
unique(frome$acoustic_tag_id) # 50 detected eels, 

# Create sf
spatial_frome <- st_as_sf(frome,
                          coords = c(6:7),
                          crs = 4326)  # WGS84

# Create and save interactive map
frome_map <- tm_shape(spatial_frome) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 25, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 50), max.categories = 19)
frome_map





# 2014_Nene ####
nene <- filter(data, animal_project_code == "2014_Nene")
nene$day <- as.Date(nene$date_time)
nene <- select(nene, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
nene$date_time <- NULL
nene <- distinct(nene)   # Select unique rows to reduce size of dataset
unique(nene$acoustic_tag_id) # 19 detected eels 

# Create sf
spatial_nene <- st_as_sf(nene,
                          coords = c(6:7),
                          crs = 4326)  # WGS84

# Create and save interactive map
nene_map <- tm_shape(spatial_nene) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 10, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 20), max.categories = 19)
nene_map




# 2015_phd_verhelst ####
verhelst <- filter(data, animal_project_code == "2015_phd_verhelst_eel")
verhelst$day <- as.Date(verhelst$date_time)
verhelst <- select(verhelst, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
verhelst$date_time <- NULL
verhelst <- distinct(verhelst)   # Select unique rows to reduce size of dataset
unique(verhelst$acoustic_tag_id) # 135 detected eels

# Create sf
spatial_verhelst <- st_as_sf(verhelst,
                             coords = c(6:7),
                             crs = 4326)  # WGS84

# Create and save interactive map
verhelst_map <- tm_shape(spatial_verhelst) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 68, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 136), max.categories = 19) 
verhelst_map




# 2017_Fremur ####
fremur <- filter(data, animal_project_code == "2017_Fremur")
fremur$day <- as.Date(fremur$date_time)
fremur <- select(fremur, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
fremur$date_time <- NULL
fremur <- distinct(fremur)   # Select unique rows to reduce size of dataset
unique(fremur$acoustic_tag_id) # 22 detected eels

# Create sf
spatial_fremur <- st_as_sf(fremur,
                           coords = c(6:7),
                           crs = 4326)  # WGS84

# Create and save interactive map
fremur_map <- tm_shape(spatial_fremur) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 11, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 22), max.categories = 19) 
fremur_map




# 2019_Grotenete ####
grotenete <- filter(data, animal_project_code == "2019_Grotenete")
grotenete$day <- as.Date(grotenete$date_time)
grotenete <- select(grotenete, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
grotenete$date_time <- NULL
grotenete <- distinct(grotenete)   # Select unique rows to reduce size of dataset
unique(grotenete$acoustic_tag_id) # 39 detected eels 

# Create sf
spatial_grotenete <- st_as_sf(grotenete,
                                 coords = c(6:7),
                                 crs = 4326)  # WGS84

# Create and save interactive map
grotenete_map <- tm_shape(spatial_grotenete) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 20, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 40), max.categories = 19)
grotenete_map




# DAK ####
dak <- filter(data, animal_project_code == "DAK")
dak$day <- as.Date(dak$date_time)
dak <- select(dak, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
dak$date_time <- NULL
dak <- distinct(dak)   # Select unique rows to reduce size of dataset
unique(dak$acoustic_tag_id) # 50 detected eels

# Create sf
spatial_dak <- st_as_sf(dak,
                        coords = c(6:7),
                        crs = 4326)  # WGS84

# Create and save interactive map
dak_map <- tm_shape(spatial_dak) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 25, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 50), max.categories = 19) 
dak_map





# ESGL ####
esgl <- filter(data, animal_project_code == "ESGL")
esgl$day <- as.Date(esgl$date_time)
esgl <- select(esgl, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
esgl$date_time <- NULL
esgl <- distinct(esgl)   # Select unique rows to reduce size of dataset
unique(esgl$acoustic_tag_id) # 49 detected eels

# Create sf
spatial_esgl <- st_as_sf(esgl,
                         coords = c(6:7),
                         crs = 4326)  # WGS84

# Create and save interactive map
esgl_map <- tm_shape(spatial_esgl) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 25, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 50), max.categories = 19) 
esgl_map






# Noordzeekanaal ####
noordzeekanaal <- filter(data, animal_project_code == "Noordzeekanaal")
noordzeekanaal$day <- as.Date(noordzeekanaal$date_time)
noordzeekanaal <- select(noordzeekanaal, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
noordzeekanaal$date_time <- NULL
noordzeekanaal <- distinct(noordzeekanaal)   # Select unique rows to reduce size of dataset
unique(noordzeekanaal$acoustic_tag_id) # 329 detected eels

# Create sf
spatial_noordzeekanaal <- st_as_sf(noordzeekanaal,
                                   coords = c(6:7),
                                   crs = 4326)  # WGS84

# Create and save interactive map
noordzeekanaal_map <- tm_shape(spatial_noordzeekanaal) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 2.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 165, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 330), max.categories = 19) 
noordzeekanaal_map





# SEMP ####
semp <- filter(data, animal_project_code == "SEMP")
semp$day <- as.Date(semp$date_time)
semp <- select(semp, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
semp$date_time <- NULL
semp <- distinct(semp)   # Select unique rows to reduce size of dataset
unique(semp$acoustic_tag_id) # 70 detected eels

# Create sf
spatial_semp <- st_as_sf(semp,
                         coords = c(6:7),
                         crs = 4326)  # WGS84

# Create and save interactive map
semp_map <- tm_shape(spatial_semp) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 35, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 70), max.categories = 19) 
semp_map






# PTN-Silver-eel-Mondego ####
mondego <- filter(data, animal_project_code == "PTN-Silver-eel-Mondego")
mondego$day <- as.Date(mondego$date_time)
mondego <- select(mondego, animal_project_code,date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
mondego$date_time <- NULL
mondego <- distinct(mondego)   # Select unique rows to reduce size of dataset
unique(mondego$acoustic_tag_id) # 40 detected eels

# Create sf
spatial_mondego <- st_as_sf(mondego,
                          coords = c(6:7),
                          crs = 4326)  # WGS84

# Create and save interactive map
mondego_map <- tm_shape(spatial_mondego) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 20, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 40), max.categories = 19) 
mondego_map






# EMMN ####
emmn <- filter(data, animal_project_code == "EMMN")
emmn$day <- as.Date(emmn$date_time)
emmn <- select(emmn, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
emmn$date_time <- NULL
emmn <- distinct(emmn)   # Select unique rows to reduce size of dataset
unique(emmn$acoustic_tag_id) # 26 detected eels

# Create sf
spatial_emmn <- st_as_sf(emmn,
                         coords = c(6:7),
                         crs = 4326)  # WGS84

# Create and save interactive map
emmn_map <- tm_shape(spatial_emmn) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 13, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 26), max.categories = 19) 
emmn_map




# life4fish ####
life4fish <- filter(data, animal_project_code == "life4fish")
life4fish$day <- as.Date(life4fish$date_time)
life4fish <- select(life4fish, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
life4fish$date_time <- NULL
life4fish <- distinct(life4fish)   # Select unique rows to reduce size of dataset
unique(life4fish$acoustic_tag_id) # 216 detected eels

# Create sf
spatial_life4fish <- st_as_sf(life4fish,
                         coords = c(6:7),
                         crs = 4326)  # WGS84

# Create and save interactive map
life4fish_map <- tm_shape(spatial_life4fish) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 1.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 13, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 432), max.categories = 19) 
life4fish_map




# nedap_meuse ####
nedap <- filter(data, animal_project_code == "nedap_meuse")
nedap$day <- as.Date(nedap$date_time)
nedap <- select(nedap, animal_project_code, date_time, day, acoustic_tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
nedap$date_time <- NULL
nedap <- distinct(nedap)   # Select unique rows to reduce size of dataset
unique(nedap$acoustic_tag_id) # 1048 detected eels, 

# Create sf
spatial_nedap <- st_as_sf(nedap,
                           coords = c(6:7),
                           crs = 4326)  # WGS84

# Create and save interactive map
nedap_map <- tm_shape(spatial_nedap) + 
  tm_dots(col = "day", id = "station_name", palette = "Spectral", size = 5.0) +
  tm_facets(by = "acoustic_tag_id",  ncol = 2, nrow = 524, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 1048), max.categories = 19) 
nedap_map

