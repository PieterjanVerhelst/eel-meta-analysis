# Analyse the migration speed in tidal area according to size, sex and geographical location
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be


source("src/calculate_migration_speed_habitats.R")


# Load packages ####
library(nlme)
library(coefplot2)


# 1. Select data in tidal areas ####
migration_speed_tidal <- filter(migration_speed, habitat_type3 == "tidal")

summary(migration_speed_tidal$speed_ms)
sd(migration_speed_tidal$speed_ms)

# 2. Link size and sex to the dataset  ####
eel <- read_csv("./data/interim/eel_meta_data.csv")
eel <- eel %>%
  mutate_at(c('acoustic_tag_id', 'animal_project_code', 'life_stage'), as.factor)
eel$length1 <- as.numeric(eel$length1)
eel$weight <- as.numeric(eel$weight)

# Create length frequency distribution
ggplot(data=eel,aes(x=length1)) +
  geom_histogram(binwidth=25,boundary=0,closed="left",
                 fill="gray80",color="black")

#eel <- eel[!(eel$animal_project_code == "life4fish" & eel$acoustic_tag_id == "7422"),]# Remove eel with ID 7422 from project (life4fish)
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

# Only keep eels in the tidal dataset
eel <- subset(eel, acoustic_tag_id %in% migration_speed_tidal$acoustic_tag_id)

# Join eel metadata to tidal dataset
migration_speed_tidal <- left_join(migration_speed_tidal, eel, by = "acoustic_tag_id")
migration_speed_tidal <- rename(migration_speed_tidal, animal_project_code = animal_project_code.x)
migration_speed_tidal$animal_project_code.y <- NULL


# 3. Size analysis  ####

# Plot
ggplot(migration_speed_tidal, aes(x=length1, y=speed_ms)) + 
  geom_point() +
  facet_wrap(~animal_project_code, scales = "free") +
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
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16)) +
  geom_smooth(method='lm')

# Linear regression
lm_size <- lm(speed_ms ~ length1, data = migration_speed_tidal) # Create the linear regression
summary(lm_size)

par(mfrow = c(2, 2))
plot(lm_size)
dev.off()
plot(lm_size$residuals, pch = 16, col = "red")  # residuals should look random
shapiro.test(residuals(lm_size))


# 4. Sex analysis  ####
# Filter on specific project
sex_project <- filter(migration_speed_tidal, animal_project_code == "Frome")
#sex_project <- filter(migration_speed_tidal, animal_project_code == "Grote Nete")
#sex_project <- filter(migration_speed_tidal, animal_project_code == "Mondego")
sex_project$sex <-factor(sex_project$sex)

aggregate(sex_project$speed_ms, list(sex_project$sex), mean)
aggregate(sex_project$speed_ms, list(sex_project$sex), median)
aggregate(sex_project$speed_ms, list(sex_project$sex), sd)
aggregate(sex_project$speed_ms, list(sex_project$sex), min)
aggregate(sex_project$speed_ms, list(sex_project$sex), max)

# Plot
ggplot(sex_project, aes(x=sex, y=speed_ms)) + 
  geom_boxplot() +
  ylab("Migration speed (m/s)") + 
  xlab("Sex") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=360),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16))


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




# 5. Geographical location analysis ####
# Add latitude to dataset
#lat <- read_csv("./data/external/project_geographical_location.csv")
#lat$animal_project_code <- factor(lat$animal_project_code)

#migration_speed_tidal <- left_join(migration_speed_tidal, lat, by = "animal_project_code")

# Plot
ggplot(migration_speed_tidal, aes(x= release_latitude, y=speed_ms)) + 
  geom_point() +
  ylab("Migration speed (m/s)") + 
  xlab("Latitude") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=360),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16)) +
  geom_smooth(method='lm')

# Linear regression
lm_geo <- lm(speed_ms ~ release_latitude, data = migration_speed_tidal) # Create the linear regression
summary(lm_geo)

par(mfrow = c(2, 2))
plot(lm_geo)
dev.off()
plot(lm_geo$residuals, pch = 16, col = "red")  # residuals should look random
shapiro.test(residuals(lm_geo))



# 6. Statistical analysis on whole dataset ####
# Calculate average migration speed
summary(migration_speed_tidal$speed_ms)
sd(migration_speed_tidal$speed_ms)


# Apply linear mixed effects model
# Full model
lmm1 <- lme(log(speed_ms) ~ release_latitude + length1,
            random = ~length1 | animal_project_code,
            data = migration_speed_tidal)

summary(lmm1)


# Check model
plot(lmm1)
par(mfrow=c(2,2))
qqnorm(resid(lmm1, type = "n"))  # type = "n"   means that the normalised residues are used; these take into account autocorrelation
hist(resid(lmm1, type = "n"))
plot(fitted(lmm1),resid(lmm1, type = "n"))
dev.off()

coefplot2(lmm1)


# Apply Tukey multiple comparisons on the model
posthoc <- glht(lmm1, linfct = mcp(water_body_class = "Tukey"))
summary(posthoc)
#par(mar = c(4, 7, 2, 2))  #par(mar = c(bottom, left, top, right))
plot(posthoc)

