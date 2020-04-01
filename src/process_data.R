# Data processing by filling in missing values and removing ghost detections
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# 2012_leopoldkanaal  ####

# receiver VR2W-115438 doesn't have station name and coordinates
# bh-9, 51.2591163, 3.5789817
leopoldkanaal <- filter(data, animal_project_code == "2012_leopoldkanaal")
no_lon <- leopoldkanaal[is.na(leopoldkanaal$deploy_longitude),]

sub_data <- filter(data, animal_project_code == "2012_leopoldkanaal" |
                     animal_project_code == "2014_Frome")


for (i in 1:dim(sub_data)[1]){
  if (sub_data$animal_project_code[1] == "2012_leopoldkanaal" & sub_data$station_name[1] == 'NA'){
    sub_data$station_name[i] = "bh_9"
  }   }


no_lon2 <- sub_data[is.na(sub_data$deploy_longitude),]
