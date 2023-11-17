# Process migration data by combining the datasets of the different studies and calculate the number of migratory eels per study
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be

# Packages
library(tidyverse)
library(lubridate)


# 1. Load and merge migration datasets ####
data_path <- "./data/interim/migration/"   # path to the data
files <- dir(data_path, pattern = "*.csv") # get file names
files

data <- files %>%
  map(~ read_csv(file.path(data_path, .))) %>% 
  reduce(rbind)

data <- data %>%
  mutate_at(c('acoustic_tag_id', 'animal_project_code', 'station_name', 'migration'), as.factor)

# Remove redundant columns
data <- select(data, -...1, -row_id, -first_dist_to_use, -first_dist_to_use_idx, -time_first_dist_to_use, -delta_totdist, -delta_t)

# Rename animal_project_code
data <- data %>% mutate(animal_project_code=recode(animal_project_code, 
                                                   "2004_Gudena" = "2004_gudena",
                                                   "2011_Loire" = "2011_loire",
                                                   "2011_Warnow" = "2011_warnow",
                                                   "2013_Stour" = "2013_stour",
                                                   "2014_Frome" = "2014_frome",
                                                   "2014_Nene" = "2014_nene",
                                                   "2017_Fremur" = "2017_fremur",
                                                   "2019_Grotenete" = "2019_grotenete",
                                                   "DAK" = "dak_markiezaatsmeer",
                                                   "EMMN" = "emmn",
                                                   "ESGL" = "esgl",
                                                   "Noordzeekanaal" = "noordzeekanaal",
                                                   "PTN-Silver-eel-Mondego" = "mondego",
                                                   "SEMP" = "semp"))

# Change project code name for relevant eels
data <- data %>%
  mutate(animal_project_code = if_else(condition = acoustic_tag_id %in% c("A69-1602-10817",
                                                                          "A69-1602-10818",
                                                                          "A69-1602-10819",
                                                                          "A69-1602-10820",
                                                                          "A69-1602-10821",
                                                                          "A69-1602-10822",
                                                                          "A69-1602-10823",
                                                                          "A69-1602-10824",
                                                                          "A69-1602-10825",
                                                                          "A69-1602-10826",
                                                                          "A69-1602-10827",
                                                                          "A69-1602-10828",
                                                                          "A69-1602-10829",
                                                                          "A69-1602-10830",
                                                                          "A69-1602-10831",
                                                                          "A69-1602-10857",
                                                                          "A69-1602-10858",
                                                                          "A69-1602-10859",
                                                                          "A69-1602-10860",
                                                                          "A69-1602-10861",
                                                                          "A69-1602-10862",
                                                                          "A69-1602-10863",
                                                                          "A69-1602-10864",
                                                                          "A69-1602-10865",
                                                                          "A69-1602-10866"),
                           true = "dak_superpolder",
                           false = animal_project_code))



# 2. Filter out migration data ####
# Select eels considered migratory
# 'migration' == TRUE
data <- filter(data, migration == "TRUE")



# 3. Remove data from the Fremur ####
data <- data[!(data$animal_project_code == "2017_fremur"),]


# 4. Remove bpns data ####
data <- data[!(data$station_name %in% c("bpns-Whitley","bpns-D1","bpns-WK12","bpns-A1BIS","bpns-S4","bpns-WENDUINEBANKW","bpns-W1","bpns-Trapegeer","bpns-S7","bpns-O6","bpns-KB2","bpns-middelkerkebank","bpns-nieuwpoortbank","PC4C-C05-2","bpns-Cpowerreefballs-CPOD","bpns-zbe1","bpns-ZA2","bpns-F53","bpns-WK14","bpns-WZ","bpns-zbw2","bpns-Nauticaena","bpns-Faulbaums","bpns-Grafton","CP_100m_base","bpns-G-88")),]

# 5. Remove 9 eels from '2015_phd_verhelst_eel' project that are flagged as migratory, but based on expert judgement they weren't and hence left out of the analysis
#data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$acoustic_tag_id == "A69-1601-28264"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$acoustic_tag_id == "A69-1601-52624"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$acoustic_tag_id == "A69-1601-57478"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$acoustic_tag_id == "A69-1601-52630"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$acoustic_tag_id == "A69-1601-52658"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$acoustic_tag_id == "A69-1601-52650"),]
#data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$acoustic_tag_id == "A69-1601-52628"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$acoustic_tag_id == "A69-1601-52652"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$acoustic_tag_id == "A69-1601-57465"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$acoustic_tag_id == "A69-1601-52665"),]
#data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$acoustic_tag_id == "A69-1601-64883"),]
data <- data[!(data$animal_project_code == "2015_phd_verhelst_eel" & data$acoustic_tag_id == "A69-1602-30335"),]



# 6. Count number of eels per project ####
eels_per_project <- data %>%
  group_by(animal_project_code, acoustic_tag_id) %>%
  select(animal_project_code, acoustic_tag_id) %>%
  summarise(ind_eels=n_distinct(acoustic_tag_id)) %>%
  summarise(eels = n())
eels_per_project$eels <- as.numeric(eels_per_project$eels)

par(mar=c(10,4,2,1))
barplot(eels_per_project$eels, names.arg=eels_per_project$animal_project_code, ylim = c(0,600), cex.names=0.8, las=2)
title(ylab="Number of eels", line = 3, cex.lab=1)
title(xlab="Animal project code", line = 8, cex.lab=1)




