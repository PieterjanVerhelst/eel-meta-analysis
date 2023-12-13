# Calculate and analyse the migration speed according to different habitats and barriers
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
aggregate(migration_speed$speed_ms, list(migration_speed$animal_project_code, migration_speed$habitat_type3), mean)


par(mar=c(10,4,2,1))
# Plot with facets
migration_speed_plot <- ggplot(migration_speed, aes(x=habitat_type3, y=speed_ms)) + 
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

# Plot without facets
migration_speed_plot <- ggplot(migration_speed, aes(x=animal_project_code, y=speed_ms, fill = habitat_type3)) + 
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


# 2. Analyse migration speed in relation to migration barriers ####
# Merge migration barrier qualification to the dataset
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


# Impact of migration barriers is only relevant in non-tidal freshwater systems
migration_speed_freshwater <- filter(migration_speed, habitat_type3 == "freshwater")
  
  
#  Plot speed in relation to those barrier qualification
migration_speed_plot <- ggplot(migration_speed_freshwater, aes(x=barrier_impact, y=speed_ms)) + 
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
migration_speed_plot <- ggplot(migration_speed_freshwater, aes(x=animal_project_code, y=speed_ms, fill = barrier_impact)) + 
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
migration_speed_plot <- ggplot(migration_speed_freshwater, aes(x=animal_project_code, y=speed_ms, fill = barrier_type)) + 
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


# 3. Analyse migration speed in non-tidal, freshwater area in relation to barrier type and geographical location ####
# Join geographical location (i.e. latitude) to dataset
lat <- read_csv("./data/external/project_geographical_location.csv")
lat$animal_project_code <- factor(lat$animal_project_code)

migration_speed_freshwater <- left_join(migration_speed_freshwater, lat, by = "animal_project_code")
plot(migration_speed_freshwater$speed_ms ~ migration_speed_freshwater$latitude)







# 4. Analyse migration speed in tidal area in relation to geographical location ####
migration_speed_tidal <- filter(migration_speed, habitat_type3 == "tidal")
lat <- read_csv("./data/external/project_geographical_location.csv")
lat$animal_project_code <- factor(lat$animal_project_code)

migration_speed_tidal <- left_join(migration_speed_tidal, lat, by = "animal_project_code")
plot(migration_speed_tidal$speed_ms ~ migration_speed_tidal$latitude)

migration_speed_tidal_plot <- ggplot(migration_speed_tidal, aes(x=animal_project_code, y=speed_ms)) + 
  geom_boxplot() +
  #scale_fill_brewer(palette="Dark2") +
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
migration_speed_tidal_plot

# Conduct ANOVA or non-parametric alternative

# Check normality
par(mfrow=c(4,2))
qqnorm(migration_speed_tidal$speed_ms)
qqline(migration_speed_tidal$speed_ms)

shapiro.test(migration_speed_tidal$speed_ms)

# Check homogeneity of variances
# Levene’s test
# Levene’s test is used to assess whether the variances of two or more populations are equal.
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
# When p > 0.05, there is no significant difference between the two variances.
car::leveneTest(speed_ms ~ animal_project_code, data = migration_speed_tidal)

## Conduct one-way ANOVA ####
aov <- aov(migration_speed_tidal$speed_ms ~ migration_speed_tidal$animal_project_code)
summary(aov)

anova <- oneway.test(migration_speed_tidal$speed_ms ~ migration_speed_tidal$animal_project_code, var.equal=FALSE) # var.equal = FALSE when homogeneity of variances is not fulfilled
anova

# Check assumptions
par(mfrow=c(2,2))
plot(aov)
dev.off

# Post-hoc test for equal variances
TukeyHSD(aov)

# Post-hoc test for unequal variances
posthocTGH(migration_speed_tidal$speed_ms, migration_speed_tidal$animal_project_code, method=c("games-howell"), digits=3)  # post-hoc test for unequal variances

## Kruskal-Wallis test when data is not normally distributed ####
kruskal.test(migration_speed_tidal$speed_ms ~ migration_speed_tidal$animal_project_code)
#posthoc.kruskal.dunn.test(x=migration_speed_tidal$animal_project_code, g=migration_speed_tidal$speed_ms, p.adjust.method="bonferroni")
FSA::dunnTest(migration_speed_tidal$speed_ms ~ migration_speed_tidal$animal_project_code, data=migration_speed_tidal, method="bonferroni")


# 5. Analyse migration speed in tidal area in relation to sex ####
# Link sex and size to the dataset  
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

eel <- subset(eel, acoustic_tag_id %in% migration_speed_tidal$acoustic_tag_id)

migration_speed_tidal <- left_join(migration_speed_tidal, eel, by = "acoustic_tag_id")
migration_speed_tidal <- rename(migration_speed_tidal, animal_project_code = animal_project_code.x)

# Filter on specific project
sex_project <- filter(migration_speed_tidal, animal_project_code == "Frome")
#sex_project <- filter(migration_speed, animal_project_code == "2019_grotenete")
#sex_project <- filter(migration_speed, animal_project_code == "mondego")
sex_project$sex <-factor(sex_project$sex)

aggregate(sex_project$speed_ms, list(sex_project$sex), mean)
aggregate(sex_project$speed_ms, list(sex_project$sex), median)
aggregate(sex_project$speed_ms, list(sex_project$sex), sd)
aggregate(sex_project$speed_ms, list(sex_project$sex), min)
aggregate(sex_project$speed_ms, list(sex_project$sex), max)


ggplot(sex_project, aes(x=sex, y=speed_ms)) + 
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


# Apply parametric t-test or non-parametric Mann-Whitney-Wilcoxon Test 
# (independent) two-sample t-test
# For more info on different types of t-test, see https://www.jmp.com/en_be/statistics-knowledge-portal/t-test.html#:~:text=Types%20of%20t%2Dtests,and%20a%20paired%20t%2Dtest.

# Check normality - Shapiro Wilk test
sex_project %>%
  group_by(sex) %>%
  rstatix::shapiro_test(speed_ms)

shapiro.test(sex_project$speed_ms)

# Draw a qq-plot by group
qqnorm(sex_project$speed_ms)
qqline(sex_project$speed_ms)


# Check equality of variances - Levene test
car::leveneTest(speed_ms ~ sex, sex_project)

# Perform (independent) two-sample t-test
t.test(speed_ms ~ sex, data = sex_project)

# Perform Mann-Whitney-Wilcoxon Test
wilcox.test(speed_ms ~ sex, data = sex_project)

