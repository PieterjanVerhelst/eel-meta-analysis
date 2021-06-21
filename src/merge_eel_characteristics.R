# Merge shad meta data to tracking datase
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# 1. Load eel metadata ####
eel <- read.csv("./data/interim/eel_meta_data.csv")
eel$X <- NULL
eel$scientific_name <- NULL
eel$age <- NULL
eel$age_unit <- NULL
eel$tag_id <- factor(eel$tag_id)
eel$capture_date_time  <- as_datetime(eel$capture_date_time)
eel$release_date_time  <- as_datetime(eel$release_date_time)

# 2. Return number of tagged eels per project ####
eel %>%
  group_by(animal_project_code) %>%
  summarise(tot_eels = n_distinct(tag_id))


# 3. Merge eel characteristics with dataset ####
data <- merge(data, eel, by=c("tag_id", "animal_project_code"))


# 4. Return number of detected eels per year ####
data %>%
  group_by(animal_project_code) %>%
  summarise(tot_eels = n_distinct(tag_id))


