# Identify months when migration starts and ends
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be

source("src/process_migration_data.R")

# Remove data from the Fremur
data <- data[!(data$animal_project_code == "2017_fremur"),]

# Remove bpns data
residency <- residency[!(residency$station_name %in% c("bpns-Whitley","bpns-D1","bpns-WK12","bpns-A1BIS","bpns-S4","bpns-WENDUINEBANKW","bpns-W1","bpns-Trapegeer","bpns-S7","bpns-O6","bpns-KB2","bpns-middelkerkebank","bpns-nieuwpoortbank","PC4C-C05-2","bpns-Cpowerreefballs-CPOD","bpns-zbe1","bpns-ZA2","bpns-F53","bpns-WK14","bpns-WZ","bpns-zbw2","bpns-Nauticaena","bpns-Faulbaums","bpns-Grafton","CP_100m_base","bpns-G-88")),]

# Get first migration record and identify month
first_record <- data %>%
  group_by(acoustic_tag_id) %>%
  arrange(arrival) %>%
  filter(row_number()==1) %>%
  as.data.frame() %>%
  select(acoustic_tag_id, arrival, animal_project_code, deploy_longitude)

first_record$month <- factor(month(first_record$arrival))     
first_record <- select(first_record, -arrival)

number_per_month <- first_record %>%
  count(animal_project_code, month)

# Get last migration record and identify month
last_record <- data %>%
  group_by(acoustic_tag_id) %>% 
  arrange(arrival) %>%  
  slice(n()) %>%
  as.data.frame() %>%
  select(acoustic_tag_id, arrival, animal_project_code, deploy_longitude)

last_record$month <- factor(month(last_record$arrival))     
last_record <- select(last_record, -arrival)

number_per_month <- last_record %>%
  count(animal_project_code, month)


# Create boxplot with speeds per project in geographical order from west to east
number_per_month$animal_project_code <- factor(number_per_month$animal_project_code, 
                                              levels = c("mondego",
                                                         "esgl",
                                                         "2011_loire",
                                                         #"2017_fremur",
                                                         "2014_frome",
                                                         "2012_leopoldkanaal",
                                                         "2015_phd_verhelst_eel",
                                                         "dak_markiezaatsmeer",
                                                         "2019_grotenete",
                                                         "2013_albertkanaal",
                                                         "nedap_meuse",
                                                         "2013_stour",
                                                         "noordzeekanaal",
                                                         "2014_nene",
                                                         "dak_superpolder",
                                                         "2004_gudena",
                                                         "2011_warnow",
                                                         "semp",
                                                         "emmn")) 



# Create stacked barplot
onset_month_plot <- ggplot(number_per_month, aes(x=animal_project_code, y=n, fill = month)) + 
  geom_bar(position="fill", stat="identity") +  # position = "stack" or position = "fill"
  ylab("Percentage of eels") + #"Number of eels" or "Percentage of eels"
  xlab("Animal project code") +
  scale_fill_viridis(discrete = T) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22))
onset_month_plot




# Onset in relation to sex
subset <- filter(data, animal_project_code == "2014_frome")
subset <- filter(data, animal_project_code == "mondego")

eel <- read_csv("./data/interim/eel_meta_data.csv")
eel <- eel %>%
  mutate_at(c('acoustic_tag_id', 'animal_project_code', 'sex', 'life_stage'), as.factor)
eel$length1 <- as.numeric(eel$length1)
eel$weight <- as.numeric(eel$weight)
eel <- eel[!(eel$animal_project_code == "life4fish" & eel$acoustic_tag_id == "7422"),]# Remove eel with ID 7422 from project (life4fish)
eel <- select(eel, 
              animal_project_code, 
              release_date_time, 
              acoustic_tag_id,
              length1,
              weight,
              sex,
              life_stage,
              release_latitude, 
              release_longitude)

eel <- subset(eel, acoustic_tag_id %in% subset$acoustic_tag_id)

subset <- left_join(subset, eel, by = "acoustic_tag_id")

# Get first migration record and identify month
first_record <- subset %>%
  group_by(acoustic_tag_id) %>%
  arrange(arrival) %>%
  filter(row_number()==1) %>%
  as.data.frame() %>%
  select(acoustic_tag_id, arrival, sex, deploy_longitude)

first_record$month <- factor(month(first_record$arrival))     
first_record <- select(first_record, -arrival)

number_per_month <- first_record %>%
  count(sex, month)

# Create stacked barplot
onset_month_plot <- ggplot(number_per_month, aes(x=sex, y=n, fill = month)) + 
  geom_bar(position="fill", stat="identity") +  # position = "stack" or position = "fill"
  ylab("Percentage of eels") + #"Number of eels" or "Percentage of eels"
  xlab("Animal project code") +
  scale_fill_viridis(discrete = T) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22))
onset_month_plot


