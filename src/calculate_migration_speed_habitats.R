# Calculate the migration speed according to different habitats and analyse difference in migration speed between tidal and non-tidal habitats
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be


source("src/process_migration_data.R")

# 1. Calculate migration speed according to habitat type ####
# Load data with habitat info and join to dataset
habitats <- read_csv("./data/external/habitats.csv")
habitats <- habitats %>%
  mutate_at(c('animal_project_code', 'station_name', 'habitat_type', 'habitat_type2', 'habitat_type3'), as.factor) %>%
  select(animal_project_code, station_name, habitat_type, habitat_type2, habitat_type3)

data <- left_join(data, habitats, by = c("animal_project_code", "station_name"))

# Calculate overall migration speed according to habitat type: speed between first and last detection as 'migration == TRUE' per habitat
migration_speed <- data %>%
  group_by(animal_project_code, habitat_type3, acoustic_tag_id) %>%
  mutate(time = max(departure)-min(arrival),
         distance = max(distance_to_source_m) - min(distance_to_source_m)) %>%
  select(animal_project_code, habitat_type3, acoustic_tag_id, time, distance) %>%
  distinct()

migration_speed <- filter(migration_speed, distance > 0) # Remove records with zero distance
migration_speed$time <- as.numeric(migration_speed$time)
migration_speed$speed_ms <- migration_speed$distance / migration_speed$time
summary(migration_speed$speed_ms)
migration_speed$time_days <- migration_speed$time / (60*60*24)
summary(migration_speed$time_days)
boxplot(migration_speed$speed_ms, ylab = "Migration speed (m/s)")


# Rename animal_project_code to river or estuary names
migration_speed$animal_project_code <- recode_factor(migration_speed$animal_project_code, 
                                                     'mondego' = "Mondego",
                                                     'esgl' = "Grand Lieu Lake",
                                                     '2011_loire' = "Loire",
                                                     '2014_frome' = "Frome",
                                                     '2012_leopoldkanaal' = "Leopold Canal",
                                                     '2015_phd_verhelst_eel' = "Scheldt",
                                                     'dak_markiezaatsmeer' = "Markiezaatsmeer",
                                                     '2019_grotenete' = "Grote Nete",
                                                     '2013_albertkanaal' = "Albert Canal",
                                                     'nedap_meuse' = "Meuse",
                                                     '2013_stour' = "Stour",
                                                     'noordzeekanaal' = "Noordzeekanaal",
                                                     '2014_nene' = "Nene",
                                                     'dak_superpolder' = "Suderpolder",
                                                     '2004_gudena' = "Gudena",
                                                     '2011_warnow' = "Warnow",
                                                     'semp' = "Nemunas",
                                                     'emmn' = "Alta")


# Create boxplot with speeds according to habitat type per project in geographical order from west to east 
migration_speed$animal_project_code <- factor(migration_speed$animal_project_code, 
                                              levels = c("Mondego",
                                                         "Grand Lieu Lake",
                                                         "Loire",
                                                         #"2017_fremur",
                                                         "Frome",
                                                         "Leopold Canal",
                                                         "Scheldt",
                                                         "Markiezaatsmeer",
                                                         "Grote Nete",
                                                         "Albert Canal",
                                                         "Meuse",
                                                         "Stour",
                                                         "Noordzeekanaal",
                                                         "Nene",
                                                         "Suderpolder",
                                                         "Gudena",
                                                         "Warnow",
                                                         "Nemunas",
                                                         "Alta"))

aggregate(migration_speed$speed_ms, list(migration_speed$habitat_type3), mean)
aggregate(migration_speed$speed_ms, list(migration_speed$habitat_type3, migration_speed$animal_project_code), mean)


par(mar=c(10,4,2,1))
# Plot with facets
ggplot(migration_speed, aes(x=habitat_type3, y=speed_ms)) + 
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

# Plot without facets
ggplot(migration_speed, aes(x=animal_project_code, y=speed_ms, fill = habitat_type3)) + 
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


# 2. Analyse difference in migration speed between tidal and non-tidal habitats ####

# Filter for projects with both tidal and non-tidal data
migration_speed_tidal_nontidal <- filter(migration_speed, animal_project_code == "Mondego" |
                                           animal_project_code == "Grand Lieu Lake" |
                                           animal_project_code == "Frome" |
                                           animal_project_code == "Leopold Canal" |
                                           animal_project_code == "Markiezaatsmeer" |
                                           animal_project_code == "Grote Nete" |
                                           animal_project_code == "Albert Canal" |
                                           animal_project_code == "Meuse" |
                                           animal_project_code == "Stour" |
                                           animal_project_code == "Nene" |
                                           animal_project_code == "Gudena" |
                                           animal_project_code == "Warnow" |
                                           animal_project_code == "Nemunas")

aggregate(migration_speed_tidal_nontidal$speed_ms, list(migration_speed_tidal_nontidal$habitat_type3, migration_speed_tidal_nontidal$animal_project_code), mean)

group_by(migration_speed_tidal_nontidal, habitat_type3) %>%
  summarise(
    count = n(),
    mean = mean(speed_ms, na.rm = TRUE),
    sd = sd(speed_ms, na.rm = TRUE),
    min = min(speed_ms, na.rm = TRUE),
    max = max(speed_ms, na.rm = TRUE)
  )

# Plot
ggplot(migration_speed_tidal_nontidal, aes(x=animal_project_code, y=speed_ms, fill = habitat_type3)) + 
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2") +
  ylab("Migration speed (m/s)") + 
  xlab("Water body") +
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


# Paired samples t-test

# Compute the difference
d <- with(migration_speed_tidal_nontidal, 
          speed_ms[habitat_type3 == "tidal"] - speed_ms[habitat_type3 == "freshwater"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value = 0.6141

# Create datasets

waterbody <- "Frome"

tidal <- filter(migration_speed_tidal_nontidal, habitat_type3 == "tidal",
                animal_project_code == waterbody)

nontidal <- filter(migration_speed_tidal_nontidal, habitat_type3 == "freshwater",
                animal_project_code == waterbody)

nontidal <- subset(nontidal, acoustic_tag_id %in% tidal$acoustic_tag_id)  # Make sure same eels are in both datasets
tidal <- subset(tidal, acoustic_tag_id %in% nontidal$acoustic_tag_id)

nontidal <- nontidal$speed_ms
tidal <- tidal$speed_ms

# Conduct paired samples t-test
res <- t.test(tidal, nontidal, paired = TRUE)
res

res$p.value
res$estimate
res$conf.int




