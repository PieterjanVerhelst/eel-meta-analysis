# Data processing by filling in missing values and removing ghost detections
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# 2012_leopoldkanaal  ####

# receiver VR2W-115438 doesn't have station name and coordinates
# bh-9, 51.2591163, 3.5789817
data$station_name <- ifelse(data$animal_project_code == '2012_leopoldkanaal' & data$receiver_id == "VR2W-115438" & is.na(data$station_name), "bh-9", data$station_name)   

data$deploy_latitude <- ifelse(data$animal_project_code == '2012_leopoldkanaal' & data$receiver_id == "VR2W-115438" & is.na(data$deploy_latitude), 51.2591163, data$deploy_latitude)   

data$deploy_longitude <- ifelse(data$animal_project_code == '2012_leopoldkanaal' & data$receiver_id == "VR2W-115438" & is.na(data$deploy_longitude), 3.5789817, data$deploy_longitude)   

# False detections in the Albert Canal (S07 and S12), Zeeschelde (s-wetteren) and bpns (PC4C-B10-2)
data <- data[!(data$animal_project_code == "2012_leopoldkanaal" & data$station_name=="S07"),]
data <- data[!(data$animal_project_code == "2012_leopoldkanaal" & data$station_name=="S12"),]
data <- data[!(data$animal_project_code == "2012_leopoldkanaal" & data$station_name=="s-Wetteren"),]
data <- data[!(data$animal_project_code == "2012_leopoldkanaal" & data$station_name=="PC4C-B10-2"),]



# 2014_Frome ####

# False detections in the Albert Canal (S-stations)
#frome <- filter(data, animal_project_code == "2014_Frome")
#unique(frome$station_name)
# [1] "F1"  "F2"  "F6"  "F7"  "F8"  "F5"  "F3"  "F4"  "S18" "S15" "S10" "S12"

data <- data[!(data$animal_project_code == "2014_Frome" & data$station_name=="S10"),]
data <- data[!(data$animal_project_code == "2014_Frome" & data$station_name=="S12"),]
data <- data[!(data$animal_project_code == "2014_Frome" & data$station_name=="S15"),]
data <- data[!(data$animal_project_code == "2014_Frome" & data$station_name=="S18"),]




# PTN-Silver-eel-Mondego ####

# False detection in the Albert Canal (S09) and North Sea (PC4C-B6-4)
data <- data[!(data$animal_project_code == "PTN-Silver-eel-Mondego" & data$station_name=="S09"),]
data <- data[!(data$animal_project_code == "PTN-Silver-eel-Mondego" & data$station_name=="PC4C-B6-4"),]



# ESGL ####

# 2 false detection in the Albert Canal (HH5)
data <- data[!(data$animal_project_code == "ESGL" & data$station_name=="HH5"),]




# 2011_Warnow ####

# False detections in Albert Canal (ak-29, ak-33, S14, S16, S18) and bpns (bpns-CNB03, PC4C-A7-3 and PC4C-B10-6)
# warnow <- filter(data, animal_project_code == "2011_Warnow")
# unique(warnow$station_name)
#[1] "W5"         "W6"         "W2"         "W7"         "W8"         "W3"         "W4"         "W1"        
#[9] "PC4C-B10-6" "S14"        "PC4C-A7-3"  "ak-33"      "S18"        "S16"        "ak-29"      "bpns-CNB03"

data <- data[!(data$animal_project_code == "2011_Warnow" & data$station_name=="ak-29"),]
data <- data[!(data$animal_project_code == "2011_Warnow" & data$station_name=="ak-33"),]
data <- data[!(data$animal_project_code == "2011_Warnow" & data$station_name=="S14"),]
data <- data[!(data$animal_project_code == "2011_Warnow" & data$station_name=="S16"),]
data <- data[!(data$animal_project_code == "2011_Warnow" & data$station_name=="S18"),]
data <- data[!(data$animal_project_code == "2011_Warnow" & data$station_name=="bpns-CNB03"),]
data <- data[!(data$animal_project_code == "2011_Warnow" & data$station_name=="PC4C-A7-3"),]
data <- data[!(data$animal_project_code == "2011_Warnow" & data$station_name=="PC4C-B10-6"),]




