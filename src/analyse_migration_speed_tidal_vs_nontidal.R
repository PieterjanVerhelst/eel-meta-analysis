# Analyse difference in migration speed between tidal and non-tidal habitats
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be



source("src/calculate_migration_speed_habitats.R")

# 1. Analyse difference in migration speed between tidal and non-tidal habitats ####

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
                                           animal_project_code == "Nene" )

# Recode habitat type 'freshwater' to 'nontidal'
migration_speed_tidal_nontidal$habitat_type3 <- recode_factor(migration_speed_tidal_nontidal$habitat_type3, 
                                          'freshwater' = "nontidal")

# 2. Add water regulating structure info to the dataset ####
wrs <- read_csv("./data/external/eels_wrs.csv")
wrs <- select(wrs, animal_project_code, acoustic_tag_id, wrs_impact_score, wrs_types, water_body_class)
wrs$animal_project_code <- recode_factor(wrs$animal_project_code, # Rename animal_project_code to river or estuary names 
                                         'PTN-Silver-eel-Mondego' = "Mondego",
                                         'ESGL' = "Grand Lieu Lake",
                                         '2011_Loire' = "Loire",
                                         '2014_Frome' = "Frome",
                                         '2012_leopoldkanaal' = "Leopold Canal",
                                         '2015_phd_verhelst_eel' = "Scheldt",
                                         'DAK_markiezaatsmeer' = "Markiezaatsmeer",
                                         '2019_Grotenete' = "Grote Nete",
                                         '2013_albertkanaal' = "Albert Canal",
                                         'nedap_meuse' = "Meuse",
                                         '2013_Stour' = "Stour",
                                         'Noordzeekanaal' = "Noordzeekanaal",
                                         '2014_Nene' = "Nene",
                                         'DAK_suderpolder' = "Suderpolder",
                                         '2004_Gudena' = "Gudena",
                                         '2011_Warnow' = "Warnow",
                                         'SEMP' = "Nemunas",
                                         'EMMN' = "Alta")
migration_speed_tidal_nontidal <- left_join(migration_speed_tidal_nontidal, wrs, by = c("animal_project_code","acoustic_tag_id"))
migration_speed_tidal_nontidal$animal_project_code <- factor(migration_speed_tidal_nontidal$animal_project_code, ordered = TRUE, 
                                                       levels = c("Mondego", 
                                                                  "Grand Lieu Lake",
                                                                  "Frome", 
                                                                  "Stour",
                                                                  "Nene",
                                                                  "Leopold Canal",
                                                                  "Grote Nete",
                                                                  "Albert Canal",
                                                                  "Markiezaatsmeer",
                                                                  "Meuse"))
migration_speed_tidal_nontidal$water_body_class <- factor(migration_speed_tidal_nontidal$water_body_class, ordered = TRUE, 
                                                    levels = c("A",
                                                               "B",
                                                               "C",
                                                               "D",
                                                               "E"))


# Calculate summaries
aggregate(migration_speed_tidal_nontidal$speed_ms, list(migration_speed_tidal_nontidal$habitat_type3, migration_speed_tidal_nontidal$animal_project_code), mean)

group_by(migration_speed_tidal_nontidal, habitat_type3) %>%
  summarise(
    count = n(),
    mean = mean(speed_ms, na.rm = TRUE),
    sd = sd(speed_ms, na.rm = TRUE),
    min = min(speed_ms, na.rm = TRUE),
    max = max(speed_ms, na.rm = TRUE)
  )

# Plot according to water body (i.e. animal project code)
ggplot(migration_speed_tidal_nontidal, aes(x=animal_project_code, y=speed_ms, fill = habitat_type3)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("nontidal" = "white",
                               "tidal" = "lightgrey")) +
  ylab("Migration speed (m/s)") + 
  xlab("Water body") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")


# Plot according to water body class
ggplot(migration_speed_tidal_nontidal, aes(x=water_body_class, y=speed_ms, fill = habitat_type3)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("nontidal" = "lightgrey",
                               "tidal" = "gray35")) +
  ylab("Migration speed (m/s)") + 
  xlab("Water body class") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=360),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16))



# Paired samples t-test

# Compute the difference
d <- with(migration_speed_tidal_nontidal, 
          speed_ms[habitat_type3 == "tidal"] - speed_ms[habitat_type3 == "nontidal"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value = 0.6141

# Create datasets

waterbody <- "Mondego"

tidal <- filter(migration_speed_tidal_nontidal, habitat_type3 == "tidal",
                animal_project_code == waterbody)

nontidal <- filter(migration_speed_tidal_nontidal, habitat_type3 == "nontidal",
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


# Conduct non-parametric paired samples Wilcoxon test
# http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r
wilcox.test(tidal, nontidal, paired = TRUE, alternative = "two.sided")



