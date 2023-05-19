# Clean residency data: errors and inconsistencies that came to light through the distance plots
# by Pieterjan Verhelst
# pieterjan.verhelst@inbo.be



# ESGL ####
# False detection in project ESGL after 2016-02-15 and the detection of eel A69-1601-38319 at station A on 2016-01-02 06:59:09
residency <- residency[!(residency$animal_project_code == "ESGL" & residency$arrival >= '2016-02-15 00:00:00'),]
residency <- residency[!(residency$animal_project_code == "ESGL" & residency$acoustic_tag_id == "A69-1601-38319" &
                           residency$arrival >= '2016-01-02 06:59:09' &
                           residency$arrival <= '2016-01-02 07:59:09'),]



# 2011_warnow ####
# False detections of station W3 in 2011 warnow project: detections at W3 same day as release, while W3 is ca. 25 km downstream of release
# see github issue https://github.com/PieterjanVerhelst/eel-meta-analysis/issues/20
residency$date <- as.Date(residency$arrival)
residency <- residency %>%
  group_by(acoustic_tag_id) %>%
  mutate(start_date = dplyr::first(date)) %>%
  filter((station_name != "W3") | (station_name == "W3" & date != start_date)) %>%
  filter((station_name != "W4") | (station_name == "W4" & date != start_date))

residency$date <- NULL
residency$start_date <- NULL


#Seven eels within the 2011_warnow project had dubious detections at station W3. They seem to 'jump' from upper stations to W3 and back, spanning over 20 km over very short time periods which is highly unlikely for eels. Hence, these detections at station W3 are considered false detections and need to be removed. The eels having such false detections are 539, 542, 555, 570, 620, 633 and 649.
#https://github.com/PieterjanVerhelst/eel-meta-analysis/issues/25

residency$arrival_numeric <- as.numeric(residency$arrival)  # Set numeric: works easier to remove line

residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-539" &
                           residency$arrival_numeric == 1309160880),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-542" &
                           residency$station_name == "W3"),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-555" &
                           residency$arrival_numeric > 1309812238 &
                           residency$arrival_numeric < 1310376350),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-570" &
                           residency$arrival_numeric == 1311038460),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-570" &
                           residency$arrival_numeric == 1311053160),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-570" &
                           residency$arrival_numeric == 1311064800),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-570" &
                           residency$arrival_numeric > 1311064899 &
                           residency$arrival_numeric < 1311282481),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-620" &
                           residency$arrival_numeric == 1319453082),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-633" &
                           residency$arrival_numeric == 1319043271),]
residency <- residency[!(residency$animal_project_code == "2011_Warnow" & residency$acoustic_tag_id == "A69-1601-649" &
                           residency$arrival_numeric > 1319670143 &
                           residency$arrival_numeric < 1319732079),]

residency$arrival_numeric <- NULL # Remove column


# Remove three eels that identified as American eel (Anguilla rostrata)
residency <- residency %>%
  filter(animal_project_code == "2011_Warnow" & acoustic_tag_id != "A69-1601-582",
         animal_project_code == "2011_Warnow" & acoustic_tag_id != "A69-1601-632",
         animal_project_code == "2011_Warnow" & acoustic_tag_id != "A69-1601-634")




# 2013_albertkanaal ####
# Remove wrong release location in 2013_albertkanaal
# No need to adjust exact release location, since eels were released next to receiver, leading to detection right after release
# --> exact release positions was HH5 instead of rel_albertkanaal2
residency <- subset(residency, station_name != "rel_albertkanaal2")



# nedap_meuse ####
# For 58 eels from the NEDAP Meuse project tagged in 2013 the release date is wrong: this should be 2013-09-11 instead of 2013-11-09.

residency$arrival_numeric <- as.numeric(residency$arrival)  # Set arrival to numeric as this makes filtering/subsetting easier

wrong_release_dates <- residency[(residency$station_name == "rel_nedap_meuse" & residency$arrival_numeric == 1383951600),] # Isolate  records with wrong release dates
unique(wrong_release_dates$acoustic_tag_id)   # Gives number of unique tag IDs
wrong_release_dates$arrival <- '2013-09-11 00:00:00'  # Correct release date
wrong_release_dates$departure <- '2013-09-11 00:00:00'  # Correct release date


residency <- residency[!(residency$station_name == "rel_nedap_meuse" & residency$arrival_numeric == 1383951600),]  # Remove records with wrong release dates
residency <- rbind(residency, wrong_release_dates)  # Bind records with correct release dates

residency$arrival_numeric <- NULL   # Remove column with arrival numeric which became now redundant



