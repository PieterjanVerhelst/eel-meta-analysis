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


# 2. Remove eel from saeftinghe ####
eel <- eel[!(eel$animal_project_code == "2015_phd_verhelst_eel" & eel$tag_id == "A69-1601-58620"),]

# 3. Replace "Silver" by "silver" in life stage ####
unique(eel$life_stage)
eel <- eel %>%                               
  mutate(life_stage = replace(life_stage, life_stage == "Silver", "silver"))


# 4. Return number of tagged eels per project ####
eel %>%
  group_by(animal_project_code) %>%
  summarise(tot_eels = n_distinct(tag_id))


# 5. Substitute code space into 'Vemco' format ####
# 2011_Loire & part EMNN
eel$tag_id <- gsub("R04K", "A69-1206", eel$tag_id)

# 2017_Fremur & part EMMN
eel$tag_id <- gsub("S256", "A69-1105", eel$tag_id)



# 6. Merge eel characteristics with dataset ####
data <- merge(data, eel, by=c("tag_id", "animal_project_code"))


# 7. Return number of detected eels per year ####
data %>%
  group_by(animal_project_code) %>%
  summarise(tot_eels = n_distinct(tag_id))


