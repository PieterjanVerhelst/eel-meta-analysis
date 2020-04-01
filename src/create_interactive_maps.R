# Create interactive html widget maps
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be

# Packages
library(sf)
library(tmap)



# Source code to upload and process data
source('./src/attach_stour.R')

# Animal project codes
unique(data$animal_project_code)
#[1] "2004_Gudena"            "2013_albertkanaal"      "2012_leopoldkanaal"     "SEMP"                  
#[5] "PTN-Silver-eel-Mondego" "2014_Frome"             "2015_phd_verhelst"      "ESGL"                  
#[9] "2011_Warnow"            "DAK"                    "2019_Grotenete"         "Noordzeekanaal"        
#[13] "2017_Fremur"            "2011_Loire"             "2013_stour"   



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
            tm_facets(by = "tag_id",  ncol = 2, nrow = 32) +
            tmap_options(limits = c(facets.view = 63))
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
  tm_facets(by = "tag_id",  ncol = 2, nrow = 80) +
  tmap_options(limits = c(facets.view = 160))
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
  tm_facets(by = "tag_id",  ncol = 2, nrow = 4) +
  tmap_options(limits = c(facets.view = 7))
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
  tm_facets(by = "tag_id",  ncol = 2, nrow = 24) +
  tmap_options(limits = c(facets.view = 48))
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
  tm_facets(by = "tag_id",  ncol = 2, nrow = 46) +
  tmap_options(limits = c(facets.view = 92))
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




