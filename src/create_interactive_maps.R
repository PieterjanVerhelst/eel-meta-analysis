# Create interactive html widget maps
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be

# Packages
library(sf)
library(tmap)



# Source code to upload and process data
source('./src/attach_stour.R')
source('./src/process_data.R')

# Animal project codes
unique(data$animal_project_code)
#[1] "2004_Gudena"            "2013_albertkanaal"      "2012_leopoldkanaal"     "SEMP"                  
#[5] "PTN-Silver-eel-Mondego" "2014_Frome"             "2015_phd_verhelst"      "ESGL"                  
#[9] "2011_Warnow"            "DAK"                    "2019_Grotenete"         "Noordzeekanaal"        
#[13] "2017_Fremur"            "2011_Loire"             "2013_stour"   

# Return number of detections, eels and detections per eel for each project
number <- data %>%
  group_by(animal_project_code) %>%
  summarise(tot_detections = n(),
            tot_eels = n_distinct(tag_id))
number$detpereel <- number$tot_detections  /  number$tot_eels



# Set interactive view
ttm() # switches between static plot and interactive viewing


# Create subset per project


# 2004_gudena ####
gudena <- filter(data, animal_project_code == "2004_Gudena")
gudena$day <- as.Date(gudena$date_time)
gudena <- select(gudena, animal_project_code, scientific_name, date_time, day, tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
unique(gudena$tag_id) # 63 detected eels, 

# Create sf
spatial_gudena <- st_as_sf(gudena,
                           coords = c(8:9),
                           crs = 4326)  # WGS84

# Create and save interactive map
gudena_map <- tm_shape(spatial_gudena) + tm_dots(col = "day", palette = "Spectral", size = 0.5) +
  tm_facets(by = "tag_id",  ncol = 2, nrow = 32, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 63), max.categories = 33) 
gudena_map





# 2013_albertkanaal ####
albertkanaal <- filter(data, animal_project_code == "2013_albertkanaal")
albertkanaal$day <- as.Date(albertkanaal$date_time)
albertkanaal <- select(albertkanaal, animal_project_code, scientific_name, date_time, day, tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
unique(albertkanaal$tag_id) # 160 detected eels, 

# Create sf
spatial_albertkanaal <- st_as_sf(albertkanaal,
                           coords = c(8:9),
                           crs = 4326)  # WGS84

# Create and save interactive map
albertkanaal_map <- tm_shape(spatial_albertkanaal) + tm_dots(col = "day", palette = "Spectral", size = 0.5) +
  tm_facets(by = "tag_id",  ncol = 2, nrow = 80, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 160), max.categories = 50)
albertkanaal_map





# 2019_Grotenete ####
grotenete <- filter(data, animal_project_code == "2019_Grotenete")
grotenete$day <- as.Date(grotenete$date_time)
grotenete <- select(grotenete, animal_project_code, scientific_name, date_time, day, tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
unique(grotenete$tag_id) # 7 detected eels, 

# Create sf
spatial_grotenete <- st_as_sf(grotenete,
                                 coords = c(8:9),
                                 crs = 4326)  # WGS84

# Create and save interactive map
grotenete_map <- tm_shape(spatial_grotenete) + tm_dots(col = "day", palette = "Spectral", size = 0.5) +
  tm_facets(by = "tag_id",  ncol = 2, nrow = 4, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 7), max.categories = 50)
grotenete_map





# 2014_Frome ####
frome <- filter(data, animal_project_code == "2014_Frome")
frome$day <- as.Date(frome$date_time)
frome <- select(frome, animal_project_code, scientific_name, date_time, day, tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
unique(frome$tag_id) # 47 detected eels, 

# Create sf
spatial_frome <- st_as_sf(frome,
                              coords = c(8:9),
                              crs = 4326)  # WGS84

# Create and save interactive map
frome_map <- tm_shape(spatial_frome) + tm_dots(col = "day", palette = "Spectral", size = 0.5) +
  tm_facets(by = "tag_id",  ncol = 2, nrow = 24, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 48), max.categories = 50)
frome_map





# 2012_leopoldkanaal ####
leopoldkanaal <- filter(data, animal_project_code == "2012_leopoldkanaal")
leopoldkanaal$day <- as.Date(leopoldkanaal$date_time)
leopoldkanaal <- select(leopoldkanaal, animal_project_code, scientific_name, date_time, day, tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
unique(leopoldkanaal$tag_id) # 92 detected eels, 

# Create sf
spatial_leopoldkanaal <- st_as_sf(leopoldkanaal,
                          coords = c(8:9),
                          crs = 4326)  # WGS84

# Create and save interactive map
leopoldkanaal_map <- tm_shape(spatial_leopoldkanaal) + tm_dots(col = "day", palette = "Spectral", size = 0.5) +
  tm_facets(by = "tag_id",  ncol = 2, nrow = 46, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 92), max.categories = 50)
leopoldkanaal_map




# 2011_Loire ####
loire <- filter(data, animal_project_code == "2011_Loire")
loire$day <- as.Date(loire$date_time)
loire <- select(loire, animal_project_code, scientific_name, date_time, day, tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
unique(loire$tag_id) # 50 detected eels

# Create sf
spatial_loire <- st_as_sf(loire,
                                  coords = c(8:9),
                                  crs = 4326)  # WGS84

# Create and save interactive map
loire_map <- tm_shape(spatial_loire) + tm_dots(col = "day", palette = "Spectral", size = 0.5) +
  tm_facets(by = "tag_id",  ncol = 2, nrow = 25, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 50), max.categories = 33) 
loire_map




# PTN-Silver-eel-Mondego ####
mondego <- filter(data, animal_project_code == "PTN-Silver-eel-Mondego")
mondego$day <- as.Date(mondego$date_time)
mondego <- select(mondego, animal_project_code, scientific_name, date_time, day, tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
unique(mondego$tag_id) # 37 detected eels

# Create sf
spatial_mondego <- st_as_sf(mondego,
                          coords = c(8:9),
                          crs = 4326)  # WGS84

# Create and save interactive map
mondego_map <- tm_shape(spatial_mondego) + tm_dots(col = "day", palette = "Spectral", size = 0.5) +
  tm_facets(by = "tag_id",  ncol = 2, nrow = 19, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 37), max.categories = 50) 
mondego_map





# ESGL ####
esgl <- filter(data, animal_project_code == "ESGL")
esgl$day <- as.Date(esgl$date_time)
esgl <- select(esgl, animal_project_code, scientific_name, date_time, day, tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
unique(esgl$tag_id) # 44 detected eels

# Create sf
spatial_esgl <- st_as_sf(esgl,
                            coords = c(8:9),
                            crs = 4326)  # WGS84

# Create and save interactive map
esgl_map <- tm_shape(spatial_esgl) + tm_dots(col = "day", palette = "Spectral", size = 0.5) +
  tm_facets(by = "tag_id",  ncol = 2, nrow = 22, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 44), max.categories = 50) 
esgl_map




# 2017_Fremur ####
fremur <- filter(data, animal_project_code == "2017_Fremur")
fremur$day <- as.Date(fremur$date_time)
fremur <- select(fremur, animal_project_code, scientific_name, date_time, day, tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
unique(fremur$tag_id) # 22 detected eels

# Create sf
spatial_fremur <- st_as_sf(fremur,
                         coords = c(8:9),
                         crs = 4326)  # WGS84

# Create and save interactive map
fremur_map <- tm_shape(spatial_fremur) + tm_dots(col = "day", palette = "Spectral", size = 0.5) +
  tm_facets(by = "tag_id",  ncol = 2, nrow = 11, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 22), max.categories = 50) 
fremur_map





# 2011_Warnow ####
warnow <- filter(data, animal_project_code == "2011_Warnow")
warnow$day <- as.Date(warnow$date_time)
warnow <- select(warnow, animal_project_code, scientific_name, date_time, day, tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
unique(warnow$tag_id) # 145 detected eels

# Create sf
spatial_warnow <- st_as_sf(warnow,
                           coords = c(8:9),
                           crs = 4326)  # WGS84

# Create and save interactive map
warnow_map <- tm_shape(spatial_warnow) + tm_dots(col = "day", palette = "Spectral", size = 0.5) +
  tm_facets(by = "tag_id",  ncol = 2, nrow = 73, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 145), max.categories = 50) 
warnow_map




# 2013_stour ####
stour <- filter(data, animal_project_code == "2013_stour")
stour$day <- as.Date(stour$date_time)
stour <- select(stour, animal_project_code, scientific_name, date_time, day, tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
unique(stour$tag_id) # 101 detected eels

# Create sf
spatial_stour <- st_as_sf(stour,
                           coords = c(8:9),
                           crs = 4326)  # WGS84

# Create and save interactive map
stour_map <- tm_shape(spatial_stour) + tm_dots(col = "day", palette = "Spectral", size = 0.5) +
  tm_facets(by = "tag_id",  ncol = 2, nrow = 51, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 101), max.categories = 50) 
stour_map






# SEMP ####
semp <- filter(data, animal_project_code == "SEMP")
semp$day <- as.Date(semp$date_time)
semp <- select(semp, animal_project_code, scientific_name, date_time, day, tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
unique(semp$tag_id) # 145 detected eels

# Create sf
spatial_semp <- st_as_sf(semp,
                          coords = c(8:9),
                          crs = 4326)  # WGS84

# Create and save interactive map
semp_map <- tm_shape(spatial_semp) + tm_dots(col = "day", palette = "Spectral", size = 0.5) +
  tm_facets(by = "tag_id",  ncol = 2, nrow = 73, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 145), max.categories = 50) 
semp_map




# DAK ####
dak <- filter(data, animal_project_code == "DAK")
dak$day <- as.Date(dak$date_time)
dak <- select(dak, animal_project_code, scientific_name, date_time, day, tag_id, station_name, receiver_id, deploy_longitude, deploy_latitude)
unique(dak$tag_id) # 45 detected eels

# Create sf
spatial_dak <- st_as_sf(dak,
                         coords = c(8:9),
                         crs = 4326)  # WGS84

# Create and save interactive map
dak_map <- tm_shape(spatial_dak) + tm_dots(col = "day", palette = "Spectral", size = 0.5) +
  tm_facets(by = "tag_id",  ncol = 2, nrow = 23, free.scales = TRUE) +
  tmap_options(limits = c(facets.view = 45), max.categories = 50) 
dak_map








