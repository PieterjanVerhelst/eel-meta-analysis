# Clean residency data: errors and inconsistencies that came to light through the distance plots
# by Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


animal_project_id <- "2011_warnow"


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




# Remove three eels that identified as American eel (Anguilla rostrata)
residency2 <- residency %>%
  filter(acoustic_tag_id != "A69-1601-582",
         acoustic_tag_id != "A69-1601-632",
         acoustic_tag_id != "A69-1601-634")


# 2013_albertkanaal ####
# Remove wrong release location in 2013_albertkanaal
# No need to adjust exact release location, since eels were released next to receiver, leading to detection right after release
# --> exact release positions was HH5 instead of rel_albertkanaal2
residency <- subset(residency, station_name != "rel_albertkanaal2")
