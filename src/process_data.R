# Data processing by filling in missing values and removing ghost detections
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# 2012_leopoldkanaal  ####

# receiver VR2W-115438 doesn't have station name and coordinates
# bh-9, 51.2591163, 3.5789817
data$station_name <- ifelse(data$animal_project_code == '2012_leopoldkanaal' & data$receiver_id == "VR2W-115438" & is.na(data$station_name), "bh-9", data$station_name)   

data$deploy_latitude <- ifelse(data$animal_project_code == '2012_leopoldkanaal' & data$receiver_id == "VR2W-115438" & is.na(data$deploy_latitude), 51.2591163, data$deploy_latitude)   

data$deploy_longitude <- ifelse(data$animal_project_code == '2012_leopoldkanaal' & data$receiver_id == "VR2W-115438" & is.na(data$deploy_longitude), 3.5789817, data$deploy_longitude)   



# 2014_Frome ####

# False detections in the Albert Canal (S-stations)
frome <- filter(data, animal_project_code == "2014_Frome")
unique(frome$station_name)
# [1] "F1"  "F2"  "F6"  "F7"  "F8"  "F5"  "F3"  "F4"  "S18" "S15" "S10" "S12"

data <- data[!(data$animal_project_code == "2014_Frome" & data$station_name=="S10"),]
data <- data[!(data$animal_project_code == "2014_Frome" & data$station_name=="S12"),]
data <- data[!(data$animal_project_code == "2014_Frome" & data$station_name=="S15"),]
data <- data[!(data$animal_project_code == "2014_Frome" & data$station_name=="S18"),]

rm(frome)



# PTN-Silver-eel-Mondego ####

# False detection in the Albert Canal (S09) and North Sea (PC4C-B6-4)
data <- data[!(data$animal_project_code == "PTN-Silver-eel-Mondego" & data$station_name=="S09"),]
data <- data[!(data$animal_project_code == "PTN-Silver-eel-Mondego" & data$station_name=="PC4C-B6-4"),]






data$deploy_latitude



bh9 <- filter(data, station_name == "bh-9")






leopoldkanaal <- filter(data, animal_project_code == "2012_leopoldkanaal")
no_lon <- leopoldkanaal[is.na(leopoldkanaal$deploy_longitude),]

sub_data <- filter(data, animal_project_code == "2012_leopoldkanaal" |
                     animal_project_code == "2014_Frome")



for (i in 1:dim(sub_data)[1]){
  if (sub_data$animal_project_code[i] == "2012_leopoldkanaal" & sub_data$receiver_id[i] == "VR2W-115438" &
      is.na(sub_data$station_name)){
    sub_data$station_name[i] = "bh_test" 
  }else{
    sub_data$station_name[i] <- sub_data$station_name[i]
  }
}




if (sub_data$animal_project_code == "2012_leopoldkanaal" & sub_data$receiver_id == "VR2W-115438" &
      is.na(sub_data$station_name)){
    sub_data$station_name = "bh-9" 
  }else{
    sub_data$station_name <- sub_data$station_name
  }













sub_data2 <- sub_data %>% 
  filter(animal_project_code == "2012_leopoldkanaal" , receiver_id == "VR2W-115438" , station_name == 'NA') %>%
  mutate(height = replace(height, height == 20, NA))


df[df$height == 20, "height"] <- NA 

sub_data[sub_data$station_name == 'NA'] <- "bh_test"



sub_data$station_name <- if(sub_data$animal_project_code == "2012_leopoldkanaal" & 
                                   sub_data$receiver_id[i] == "VR2W-115438" &
                                   is.na(sub_data$station_name), "bh-test", sub_data$station_name)  


  
  
sub_data$station_name <- if(sub_data$animal_project_code == "2012_leopoldkanaal" & sub_data$receiver_id == "VR2W-115438" & is.na(sub_data$station_name)){
    sub_data$station_name <- "bh_test"
  }else{
    sub_data$station_name <- sub_data$station_name
  }
                                
                                

no_lon2 <- sub_data[is.na(sub_data$deploy_longitude),]
