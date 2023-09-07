# Calculate the migration speed and relate it to geographical position, size and sex
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be


source("src/process_migration_data.R")

# Remove data from the Fremur
data <- data[!(data$animal_project_code == "2017_fremur"),]

# Remove bpns data
data <- data[!(data$station_name %in% c("bpns-Whitley","bpns-D1","bpns-WK12","bpns-A1BIS","bpns-S4","bpns-WENDUINEBANKW","bpns-W1","bpns-Trapegeer","bpns-S7","bpns-O6","bpns-KB2","bpns-middelkerkebank","bpns-nieuwpoortbank","PC4C-C05-2","bpns-Cpowerreefballs-CPOD","bpns-zbe1","bpns-ZA2","bpns-F53","bpns-WK14","bpns-WZ","bpns-zbw2","bpns-Nauticaena","bpns-Faulbaums","bpns-Grafton","CP_100m_base","bpns-G-88")),]

# 1. Calculate overall migration speed: speed between first and last detection as 'has_migration_started == TRUE' ####
migration_speed <- data %>%
  group_by(animal_project_code, acoustic_tag_id) %>%
  mutate(time = max(departure)-min(arrival),
         distance = max(distance_to_source_m) - min(distance_to_source_m)) %>%
  select(animal_project_code, acoustic_tag_id, time, distance) %>%
  distinct()

migration_speed$time <- as.numeric(migration_speed$time)
migration_speed$speed_ms <- migration_speed$distance / migration_speed$time
summary(migration_speed$speed_ms)
migration_speed$time_days <- migration_speed$time / (60*60*24)
summary(migration_speed$time_days)
boxplot(migration_speed$speed_ms, ylab = "Migration speed (m/s)")

# 2. Create boxplot with speeds per project in geographical order from west to east ####
migration_speed$animal_project_code <- factor(migration_speed$animal_project_code, 
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

aggregate(migration_speed$speed_ms, list(migration_speed$animal_project_code), mean)


par(mar=c(10,4,2,1))
migration_speed_plot <- ggplot(migration_speed, aes(x=animal_project_code, y=speed_ms)) + 
  geom_boxplot() +
  ylab("Migration speed (m/s)") + 
  xlab("Animal project code") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 4, color = "blue") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22))
migration_speed_plot



# 3. Link sex and size to the dataset  ####
eel <- read_csv("./data/interim/eel_meta_data.csv")
eel <- eel %>%
  mutate_at(c('acoustic_tag_id', 'animal_project_code', 'life_stage'), as.factor)
eel$length1 <- as.numeric(eel$length1)
eel$weight <- as.numeric(eel$weight)

# Create length frequency distribution
ggplot(data=eel,aes(x=length1)) +
  geom_histogram(binwidth=25,boundary=0,closed="left",
                 fill="gray80",color="black")

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

eel <- subset(eel, acoustic_tag_id %in% migration_speed$acoustic_tag_id)

migration_speed <- left_join(migration_speed, eel, by = "acoustic_tag_id")
migration_speed <- rename(migration_speed, animal_project_code = animal_project_code.x)

aggregate(migration_speed$speed_ms, list(migration_speed$sex), mean)



# 4. Create boxplot with speeds in relation to sex ####
par(mar=c(10,4,2,1))
migration_speed_plot <- ggplot(migration_speed, aes(x=sex, y=speed_ms)) + 
  geom_boxplot() +
  ylab("Migration speed (m/s)") + 
  xlab("Sex") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 4, color = "blue") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=360),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22))
migration_speed_plot


# 5. Create dotplot with speeds in relation to total length and colour according to sex ####
par(mar=c(10,4,2,1))
migration_speed_plot <- ggplot(migration_speed, aes(x=length1, y=speed_ms)) + 
  geom_point(alpha = 1.0, aes(color = sex)) +
  ylab("Migration speed (m/s)") + 
  xlab("Total length (mm)") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=360),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22))
migration_speed_plot

# 6. Create dotplot with speeds in relation to release longitude and colour according to sex ####
par(mar=c(10,4,2,1))
migration_speed_plot <- ggplot(migration_speed, aes(x=release_longitude, y=speed_ms)) + 
  geom_point(size = 3, alpha = 1.0, aes(color = sex)) +
  ylab("Migration speed (m/s)") + 
  xlab("Release longitude") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 4, color = "blue") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=360),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22))
migration_speed_plot


# 7. Merge migration barrier qualification to the dataset and plot speed in relation to those barrier qualification ####
migrationbarriers <- read_csv("./data/external/migrationbarriers.csv")
migrationbarriers <- migrationbarriers %>%
  mutate_at(c('animal_project_code', 'barrier_impact', 'weir', 'sluice_gate', 'shipping_lock', 'hydropower', 'pump', 'barrier_type'), as.factor) %>%
  select(-'barrier_type')
migration_speed <- left_join(migration_speed, migrationbarriers, by = "animal_project_code")

# Part of semp-eels had free migration route and another part went through hydropower
semp_data <- read.csv('./data/interim/migration/migration_semp.csv')
eel_semp_free <- semp_data %>%
  mutate_at(c('acoustic_tag_id', 'animal_project_code', 'station_name'), as.factor) %>%
  filter(station_name == "rel_semp1" |
           station_name == "rel_semp2"|
           station_name == "rel_semp3"|
           station_name == "rel_semp5"|
           station_name == "rel_semp6") %>%
  select(animal_project_code, acoustic_tag_id, station_name)
eel_semp_free <- eel_semp_free$acoustic_tag_id

# Classify barrier type
migration_speed$barrier_type <- NA
migration_speed <- migration_speed %>%
  mutate(barrier_type = case_when(animal_project_code == "semp" & acoustic_tag_id %in% eel_semp_free ~ "none",
                                  animal_project_code == "semp" ~ "hydropower",
                                  animal_project_code == "mondego" ~ "weir",
                                  animal_project_code == "esgl" ~ "weir",
                                  animal_project_code == "2011_loire" ~ "none",
                                  animal_project_code == "2014_frome" ~ "weir",
                                  animal_project_code == "2012_leopoldkanaal" ~ "pump",
                                  animal_project_code == "2015_phd_verhelst_eel" ~ "none",
                                  animal_project_code == "dak_markiezaatsmeer" ~ "weir",
                                  animal_project_code == "2019_grotenete" ~ "none",
                                  animal_project_code == "2013_albertkanaal" ~ "shipping_lock",
                                  animal_project_code == "nedap_meuse" ~ "hydropower",
                                  animal_project_code == "2013_stour" ~ "weir",
                                  animal_project_code == "noordzeekanaal" ~ "pump",
                                  animal_project_code == "2014_nene" ~ "weir",
                                  animal_project_code == "dak_superpolder" ~ "shipping_lock",
                                  animal_project_code == "2004_gudena" ~ "none",
                                  animal_project_code == "2011_warnow" ~ "weir",
                                  animal_project_code == "emmn" ~ "none"))

aggregate(migration_speed$speed_ms, list(migration_speed$barrier_impact), mean)
aggregate(migration_speed$speed_ms, list(migration_speed$barrier_type), mean)

migration_speed_plot <- ggplot(migration_speed, aes(x=barrier_impact, y=speed_ms)) + 
  geom_boxplot() +
  ylab("Migration speed (m/s)") + 
  xlab("Qualitative migration barrier impact") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 4, color = "blue") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=360),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22))
migration_speed_plot

# Colour boxplots in relation to barrier impact
migration_speed_plot <- ggplot(migration_speed, aes(x=animal_project_code, y=speed_ms, fill = barrier_impact)) + 
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2") +
  ylab("Migration speed (m/s)") + 
  xlab("Animal project code") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22))
migration_speed_plot

# Colour boxplots in relation to barrier type
migration_speed_plot <- ggplot(migration_speed, aes(x=animal_project_code, y=speed_ms, fill = barrier_type)) + 
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2") +
  ylab("Migration speed (m/s)") + 
  xlab("Animal project code") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22))
migration_speed_plot




