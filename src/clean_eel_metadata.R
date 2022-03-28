# Eel meta-data cleaning by removing irrelevant and redundant eels, columns and make certain column values consistent
# by Pieterjan Verhelst
# Pieterjan.Verhelst@inbo.be


# 1. Remove redundant columns ####
eels$age <- NULL
eels$age_unit <- NULL
eels$treatment_type <- NULL

# 2. Set as factors ####
eels$animal_project_code <- factor(eels$animal_project_code)
eels$acoustic_tag_id <- factor(eels$acoustic_tag_id)
eels$sex <- factor(eels$sex)
eels$life_stage <- factor(eels$life_stage)

# 3. Remove redundant and irrelevant eels ####

# Eel from saeftinghe 
eels <- eels[!(eels$animal_project_code == "2015_phd_verhelst_eel" & eels$acoustic_tag_id == "A69-1601-58620"),]

# 4 transmitter IDs that were reused 
eels <- eels[!(eels$acoustic_tag_id == "A69-1601-29925"),]
eels <- eels[!(eels$acoustic_tag_id == "A69-1601-29920"),]
eels <- eels[!(eels$acoustic_tag_id == "A69-1601-38334"),]
eels <- eels[!(eels$acoustic_tag_id == "A69-1602-4036"),]

# 5 eels from '2012_leopoldkanaal' that were tagged in 2011, but not detected
eels <- eels[!(eels$animal_project_code == "2012_leopoldkanaal" & eels$acoustic_tag_id == "A69-1601-31894"),]
eels <- eels[!(eels$animal_project_code == "2012_leopoldkanaal" & eels$acoustic_tag_id == "A69-1601-31899"),]
eels <- eels[!(eels$animal_project_code == "2012_leopoldkanaal" & eels$acoustic_tag_id == "A69-1601-31901"),]
eels <- eels[!(eels$animal_project_code == "2012_leopoldkanaal" & eels$acoustic_tag_id == "A69-1601-31902"),]
eels <- eels[!(eels$animal_project_code == "2012_leopoldkanaal" & eels$acoustic_tag_id == "A69-1601-31903"),]


# 140 eels from life4fish tagged after 2017 (139 in 2019 and 1 in 220)
eels <- eels[!(eels$animal_project_code == "life4fish" & eels$capture_date_time > "2018-01-01"),]


# 6. Replace "Silver" by "silver" in life stage ####
unique(eels$life_stage)
eels <- eels %>%                               
  mutate(life_stage = replace(life_stage, life_stage == "Silver", "silver"))

# 7. Consistent use of sex notation ####
unique(eels$sex)
eels <- eels %>%                               
  mutate(sex = replace(sex, sex == "F", "female"))

eels <- eels %>%                               
  mutate(sex = replace(sex, sex == "M", "male"))

eels <- eels %>%                               
  mutate(sex = replace(sex, sex == "unknown", "female"))  # 'unknown' had TL > 45 cm, so considered female

eels$sex <- ifelse(eels$animal_project_code == 'SEMP' & is.na(eels$sex), "female", eels$sex) # SEMP eels > 45 cm, so considered female

# For 2014_Frome consider males < 46 cm and females > 46 cm
for (i in 1:dim(eels)[1]){
  if (eels$animal_project_code[i] == "2014_Frome" & eels$length1[i] < 46.0){
    eels$sex[i] = "male"
  } else if (eels$animal_project_code[i] == "2014_Frome" & eels$length1[i] > 46.0){
    eels$sex[i] = "female"
  } else{
    eels$sex[i] = eels$sex[i]
  }}


# 8. Consistent use of weight units ####
unique(eels$weight_unit)
eels <- eels %>%                               
  mutate(weight_unit = replace(weight_unit, weight_unit == "grams", "g"))


# 9. Consistent use of length type, units and values (mm) ####
# Length type
unique(eels$length1_type)
eels <- eels %>%                               
  mutate(length1_type = replace(length1_type, length1_type == "Total length", "total length"))

# Length unit
unique(eels$length1_unit)
eels <- eels %>%                               
  mutate(length1_unit = replace(length1_unit, length1_unit == "milimeters", "mm"))

eels$length1_unit <- ifelse(eels$animal_project_code == '2017_Fremur' & is.na(eels$length1_unit), "mm", eels$length1_unit) # missing length unit for this eel


# Length value
for (i in 1:dim(eels)[1]){
  if (eels$length1_unit[i] == "cm"){
    eels$length1[i] = eels$length1[i] * 10
  } else{
    eels$length1[i] = eels$length1[i]
  }}

# cm to mm
unique(eels$length1_unit)
eels <- eels %>%                               
  mutate(length1_unit = replace(length1_unit, length1_unit == "cm", "mm"))


summary(eels$length1)

#check <- 
#  eels %>% 
#  filter(is.na(length1))


# 10. Return number of tagged eels per project ####
eels %>%
  group_by(animal_project_code) %>%
  summarise(tot_eels = n_distinct(acoustic_tag_id))


# 11. Substitute code space into 'Vemco' format ####
# 2011_Loire & part EMNN
eels$acoustic_tag_id <- gsub("R04K", "A69-1206", eels$acoustic_tag_id)

# 2017_Fremur & part EMMN
eels$acoustic_tag_id <- gsub("S256", "A69-1105", eels$acoustic_tag_id)


# 12. Remove '416kHz-' prefix for tags of life4fish ####
for (i in 1:dim(eels)[1]){
  if (eels$animal_project_code[i] == "life4fish"){
    eels$acoustic_tag_id[i] = gsub('416kHz-', '', eels$acoustic_tag_id[i])
  } else{
    eels$acoustic_tag_id[i] = eels$acoustic_tag_id[i]
  }}


# 13. Write csv file  ####
write.csv(eels, "./data/interim/eel_meta_data.csv")




