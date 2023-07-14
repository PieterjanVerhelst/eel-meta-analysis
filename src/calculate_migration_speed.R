# Calculate the migration speed and relate it to geographical position, size and sex
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be


source("src/process_migration_data.R")

# Calculate overall migration speed: speed between first and last detection as 'has_migration_started == TRUE'
migration_speed <- data %>%
  group_by(animal_project_code, acoustic_tag_id) %>%
  mutate(time = max(departure)-min(arrival),
         distance = max(distance_to_source_m) - min(distance_to_source_m)) %>%
  select(animal_project_code, acoustic_tag_id, time, distance) %>%
  distinct()

migration_speed$time <- as.numeric(migration_speed$time)
migration_speed$speed_ms <- migration_speed$distance / migration_speed$time
summary(migration_speed$speed_ms)

# Create boxplot with speeds per project in geographical order from west to east
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



# Link sex and size to the dataset
eel <- read_csv("./data/interim/eel_meta_data.csv")
eel <- eel %>%
  mutate_at(c('acoustic_tag_id', 'animal_project_code', 'sex', 'life_stage'), as.factor)
eel$length1 <- as.numeric(eel$length1)
eel$weight <- as.numeric(eel$weight)

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

eel <- subset(eel, acoustic_tag_id %in% migrants$acoustic_tag_id)

