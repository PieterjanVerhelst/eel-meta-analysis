# Calculate the migration speed and relate it to geographical position, size and sex
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be


source("src/process_migration_data.R")

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
                                                         "2017_fremur",
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

eel <- subset(eel, acoustic_tag_id %in% migration_speed$acoustic_tag_id)

migration_speed <- left_join(migration_speed, eel, by = "acoustic_tag_id")
migration_speed <- rename(migration_speed, animal_project_code = animal_project_code.x)

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


# 7. Merge migration barrier qualification to the dataset and plot speed in relation to those barrier qualification ####
migrationbarriers <- read_csv("./data/external/migrationbarriers.csv")
migrationbarriers <- migrationbarriers %>%
  mutate_at(c('animal_project_code', 'barrier_impact', 'weir', 'sluice_gate', 'shipping_lock', 'hydropower', 'pump'), as.factor)
migration_speed <- left_join(migration_speed, migrationbarriers, by = "animal_project_code")

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

