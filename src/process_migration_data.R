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
  mutate_at(c('acoustic_tag_id', 'animal_project_code', 'station_name'), as.factor)

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

# Change factor level from eels that are now under project `dak_markiezaatsmeer`, but should be under project `dak_superpolder`
# Create levels with all project code names
levels(data$animal_project_code) <- c("2004_gudena","2011_loire","2011_warnow","2012_leopoldkanaal","2013_albertkanaal","2013_stour","2014_frome","2014_nene","2015_phd_verhelst_eel","2017_fremur","2019_grotenete","dak_markiezaatsmeer","emmn","esgl","mondego","nedap_meuse","noordzeekanaal","semp")

# Change project code name for relevant eels
data %>%
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
# 'has_migration_started' == TRUE
# total distance > 4000 m
data <- filter(data, has_migration_started == "TRUE")

migrants <- data %>%
  select(acoustic_tag_id, distance_to_source_m) %>%
  group_by(acoustic_tag_id) %>%
  mutate(total_distance = max(distance_to_source_m)-min(distance_to_source_m)) %>%
  select(-distance_to_source_m) %>%
  distinct()

migrants <- filter(migrants, total_distance > 4000)

data <- subset(data, acoustic_tag_id %in% migrants$acoustic_tag_id)
data$acoustic_tag_id <- factor(data$acoustic_tag_id)


# 3. Count number of eels per project ####
eels_per_project <- data %>%
  group_by(animal_project_code, acoustic_tag_id) %>%
  select(animal_project_code, acoustic_tag_id) %>%
  summarise(ind_eels=n_distinct(acoustic_tag_id)) %>%
  summarise(eels = n())
eels_per_project$eels <- as.numeric(eels_per_project$eels)

par(mar=c(10,4,2,1))
barplot(eels_per_project$eels, names.arg=eels_per_project$animal_project_code, ylim = c(0,700), cex.names=0.8, las=2)
title(ylab="Number of eels", line = 3, cex.lab=1)
title(xlab="Animal project code", line = 8, cex.lab=1)




