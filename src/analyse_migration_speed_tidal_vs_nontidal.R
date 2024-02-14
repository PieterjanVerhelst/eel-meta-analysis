# Analyse difference in migration speed between tidal and non-tidal habitats
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be



source("src/calculate_migration_speed_habitats.R")

# Analyse difference in migration speed between tidal and non-tidal habitats ####

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

# Plot
ggplot(migration_speed_tidal_nontidal, aes(x=animal_project_code, y=speed_ms, fill = habitat_type3)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("nontidal" = "lightgrey",
                               "tidal" = "gray35")) +
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
