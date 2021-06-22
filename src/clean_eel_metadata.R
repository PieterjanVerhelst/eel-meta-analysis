# Eel meta-data cleaning by removing irrelevant and redundant eels, columns and make certain column values consistent
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# 1. Remove redundant columns ####
eels$scientific_name <- NULL
eels$age <- NULL
eels$age_unit <- NULL
eels$treatment_type <- NULL
eels$tag_id <- factor(eels$tag_id)


# 2. Remove redundant and irrelevant eels ####

# Eel from saeftinghe 
eels <- eels[!(eels$animal_project_code == "2015_phd_verhelst_eel" & eels$tag_id == "A69-1601-58620"),]

# 4 transmitter IDs that were reused 
eels <- eels[!(eels$tag_id == "A69-1601-29925"),]
eels <- eels[!(eels$tag_id == "A69-1601-29920"),]
eels <- eels[!(eels$tag_id == "A69-1601-38334"),]
eels <- eels[!(eels$tag_id == "A69-1602-4036"),]

# 4 eels from '2012_leopoldkanaal' that were tagged in 2011, but not detected
eels <- eels[!(eels$animal_project_code == "2012_leopoldkanaal" & eels$tag_id == "A69-1601-31894"),]
eels <- eels[!(eels$animal_project_code == "2012_leopoldkanaal" & eels$tag_id == "A69-1601-31899"),]
eels <- eels[!(eels$animal_project_code == "2012_leopoldkanaal" & eels$tag_id == "A69-1601-31901"),]
eels <- eels[!(eels$animal_project_code == "2012_leopoldkanaal" & eels$tag_id == "A69-1601-31902"),]
eels <- eels[!(eels$animal_project_code == "2012_leopoldkanaal" & eels$tag_id == "A69-1601-31903"),]


# 3. Replace "Silver" by "silver" in life stage ####
unique(eels$life_stage)
eels <- eels %>%                               
  mutate(life_stage = replace(life_stage, life_stage == "Silver", "silver"))

# 4. Consistent use of F and M ####
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
  } else{
    eels$sex[i] = "female"
  }}








# 5. Return number of tagged eels per project ####
eel %>%
  group_by(animal_project_code) %>%
  summarise(tot_eels = n_distinct(tag_id))


# 6. Substitute code space into 'Vemco' format ####
# 2011_Loire & part EMNN
eel$tag_id <- gsub("R04K", "A69-1206", eel$tag_id)

# 2017_Fremur & part EMMN
eel$tag_id <- gsub("S256", "A69-1105", eel$tag_id)

