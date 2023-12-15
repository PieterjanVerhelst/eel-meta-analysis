# Analyse the migration speed in tidal area according to size, sex and geographical location
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be


source("src/calculate_migration_speed_habitats.R")












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

