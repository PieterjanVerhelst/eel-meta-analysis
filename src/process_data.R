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

# Remove complete eel (false detections)
data <- data[!(data$animal_project_code == "2012_leopoldkanaal" & data$date_time >= '2016-01-01 00:00:00'),]




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
# 1 false detection in the North Sea
data <- data[!(data$animal_project_code == "ESGL" & data$station_name=="JJ_B9_4"),]




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




# SEMP ####

# False detections in Albert Canal (ak-30, S14, S15), bpns (PC4C-A7-2, bpns-CPowerReefballs, PC4C-C09-2, PC4C-F05-3) and Norway (4, 9, 13)
#semp <- filter(data, animal_project_code == "SEMP")
#unique(semp$station_name)
#[1] "above-kaunas-2"       "above-kaunas-3"       "above-kaunas-4"       "above-kaunas-1"      
#[5] "PC4C-A7-2"            "Rusne-1"              "Rusne-2"              "Rusne-3"             
#[9] "Rusne-4"              "klaipeda-1"           "klaipeda-4"           "klaipeda-2"          
#[13] "klaipeda-3"           "below-kaunas-2"       "below-kaunas-3"       "below-kaunas-4"      
#[17] "below-kaunas-1"       "bpns-CPowerReefballs" "S14"                  "S15"                 
#[21] "ak-30"                "PC4C-C09-2"           "PC4C-F05-3"           "13"                  
#[25] "4"                    "9"   

data <- data[!(data$animal_project_code == "SEMP" & data$station_name=="ak-30"),]
data <- data[!(data$animal_project_code == "SEMP" & data$station_name=="S14"),]
data <- data[!(data$animal_project_code == "SEMP" & data$station_name=="S15"),]
data <- data[!(data$animal_project_code == "SEMP" & data$station_name=="PC4C-A7-2"),]
data <- data[!(data$animal_project_code == "SEMP" & data$station_name=="bpns-CPowerReefballs"),]
data <- data[!(data$animal_project_code == "SEMP" & data$station_name=="PC4C-C09-2"),]
data <- data[!(data$animal_project_code == "SEMP" & data$station_name=="PC4C-F05-3"),]
data <- data[!(data$animal_project_code == "SEMP" & data$station_name=="4"),]
data <- data[!(data$animal_project_code == "SEMP" & data$station_name=="9"),]
data <- data[!(data$animal_project_code == "SEMP" & data$station_name=="13"),]

# Remove false detections after study period
data <- data[!(data$animal_project_code == "SEMP" & data$date_time >= '2016-01-01 00:00:00'),]



# DAK ####

# False detections of eel A69-1602-10860 in Markiezaatsmeer (Markiezaat-5), while tagged and released in Superpolder
data <- data[!(data$tag_id == "A69-1602-10860" & data$station_name=="Markiezaat-5"),]



# 2013_albertkanaal ####

# False detections in France and Norway
data <- data[!(data$tag_id == "A69-1105-100" & data$station_name=="F25"),]
data <- data[!(data$tag_id == "A69-1105-104" & data$station_name=="F17"),]
data <- data[!(data$tag_id == "A69-1601-26446" & data$station_name=="9"),]
data <- data[!(data$tag_id == "A69-1601-26446" & data$station_name=="15"),]
data <- data[!(data$tag_id == "A69-1601-26446" & data$station_name=="27"),]
data <- data[!(data$tag_id == "A69-1601-38745" & data$station_name=="4"),]
data <- data[!(data$tag_id == "A69-1601-38745" & data$station_name=="13"),]

# False detections in Spain after study period
data <- data[!(data$animal_project_code == "2013_albertkanaal" & data$date_time >= '2019-01-01 00:00:00'),]




# EMMN ####
# False detections in various networks of Europe, all after study period (2007)
data <- data[!(data$animal_project_code == "EMMN" & data$date_time >= '2008-01-01 00:00:00'),]

# Remove detections from receivers with incorrect position (actual position is unknown)
data <- data[!(data$animal_project_code == "EMMN" & data$receiver_id == "VR2-5349"),]
data <- data[!(data$animal_project_code == "EMMN" & data$receiver_id == "VR2-5357"),]
data <- data[!(data$animal_project_code == "EMMN" & data$receiver_id == "VR2W-100569"),]
data <- data[!(data$animal_project_code == "EMMN" & data$receiver_id == "VR2W-100574"),]




# 2015_phd_verhelst_eel ####
# Remove eel from Saeftinghe
data <- data[!(data$tag_id == "A69-1601-58620"),]

# Remove false detections from Albert Canal (ak-43 and ak-x)
data <- data[!(data$tag_id == "A69-1601-52649" & data$station_name=="ak-43"),]
data <- data[!(data$tag_id == "A69-1601-57477" & data$station_name=="ak-x"),]

# Remove false detections in Spain
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$station_name=="1"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$station_name=="9"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$station_name=="14"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$station_name=="16"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$station_name=="18"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$station_name=="20"),]




# 2004_Gudena ####
# Remove false detections from Azores
data <- data[!(data$animal_project_code == "2004_Gudena" & data$station_name=="06 MG ponta radares"),]



# 2019_Grotenete ####
# Remove false detections in Spain
data <- data[!(data$animal_project_code == "2019_Grotenete" & data$station_name=="14"),]




# Noordzeekanaal ####
# Remove false detections in North Sea
data <- data[!(data$animal_project_code == "Noordzeekanaal" & data$station_name=="bpns-Belwindreefballs-CPOD"),]



