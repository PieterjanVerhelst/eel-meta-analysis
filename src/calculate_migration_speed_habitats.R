# Calculate the migration speed according to different habitats
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be


source("src/process_migration_data.R")

# Remove data from the Fremur
data <- data[!(data$animal_project_code == "2017_fremur"),]

# Remove bpns data
residency <- residency[!(residency$station_name %in% c("bpns-Whitley","bpns-D1","bpns-WK12","bpns-A1BIS","bpns-S4","bpns-WENDUINEBANKW","bpns-W1","bpns-Trapegeer","bpns-S7","bpns-O6","bpns-KB2","bpns-middelkerkebank","bpns-nieuwpoortbank","PC4C-C05-2","bpns-Cpowerreefballs-CPOD","bpns-zbe1","bpns-ZA2","bpns-F53","bpns-WK14","bpns-WZ","bpns-zbw2","bpns-Nauticaena","bpns-Faulbaums","bpns-Grafton","CP_100m_base","bpns-G-88")),]

# Load data with habitat info and join to dataset
habitats <- read_csv("./data/external/habitats.csv")
habitats <- habitats %>%
  mutate_at(c('animal_project_code', 'station_name', 'habitat_type', 'habitat_type2', 'habitat_type3'), as.factor) %>%
  select(animal_project_code, station_name, habitat_type, habitat_type2, habitat_type3)

data <- left_join(data, habitats, by = c("animal_project_code", "station_name"))

# Calculate overall migration speed: speed between first and last detection as 'has_migration_started == TRUE' 
migration_speed <- data %>%
  group_by(animal_project_code, habitat_type2, acoustic_tag_id) %>%
  mutate(time = max(departure)-min(arrival),
         distance = max(distance_to_source_m) - min(distance_to_source_m)) %>%
  select(animal_project_code, habitat_type2, acoustic_tag_id, time, distance) %>%
  distinct()

migration_speed$time <- as.numeric(migration_speed$time)
migration_speed$speed_ms <- migration_speed$distance / migration_speed$time
summary(migration_speed$speed_ms)
migration_speed$time_days <- migration_speed$time / (60*60*24)
summary(migration_speed$time_days)
boxplot(migration_speed$speed_ms, ylab = "Migration speed (m/s)")

# Create boxplot with speeds per project in geographical order from west to east 
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

aggregate(migration_speed$speed_ms, list(migration_speed$animal_project_code, migration_speed$habitat_type2), mean)


par(mar=c(10,4,2,1))
migration_speed_plot <- ggplot(migration_speed, aes(x=habitat_type2, y=speed_ms)) + 
  geom_boxplot() +
  facet_wrap(~animal_project_code) +
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
    axis.title.y = element_text(size = 22),
    strip.text = element_text(size=22)) +
  coord_cartesian(ylim = c(0, 2))
migration_speed_plot

