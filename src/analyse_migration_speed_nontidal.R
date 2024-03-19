# Analyse the migration speed in non-tidal areas according to size, sex, geographical location, migration barrier numbers and types
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be


source("src/calculate_migration_speed_habitats.R")

# Load packages ####
#library(rstatix)  # To apply Games-Howell posthoc test with unequal variances


# 1. Select data in non-tidal areas ####

# Select freshwater = non-tidal areas
migration_speed_nontidal <- filter(migration_speed, habitat_type3 == "freshwater")

# Calculate summaries
summary(migration_speed_nontidal$speed_ms)
sd(migration_speed_nontidal$speed_ms)
aggregate(migration_speed_nontidal$speed_ms, list(migration_speed_nontidal$animal_project_code), mean)


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

eel <- eel[!(eel$animal_project_code == "life4fish" & eel$acoustic_tag_id == "7422"),]# Remove eel with ID 7422 from project (life4fish) since there is another eel with same ID in project nedap_meuse (Meuse)
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

# Only keep eels in the non-tidal dataset
eel <- subset(eel, acoustic_tag_id %in% migration_speed_nontidal$acoustic_tag_id)

# Join eel metadata to non-tidal dataset
migration_speed_nontidal <- left_join(migration_speed_nontidal, eel, by = "acoustic_tag_id")
migration_speed_nontidal <- rename(migration_speed_nontidal, animal_project_code = animal_project_code.x)
migration_speed_nontidal$animal_project_code.y <- NULL


# 3. Size analysis  ####

# Plot
ggplot(migration_speed_nontidal, aes(x=length1, y=speed_ms)) + 
  geom_point() +
  facet_wrap(~animal_project_code, scales = "free") +
  ylab("Migration speed (m/s)") + 
  xlab("Total length (mm)") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 2, color = "blue") +
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
lm_size <- lm(speed_ms ~ length1, data = migration_speed_nontidal) # Create the linear regression
summary(lm_size)

par(mfrow = c(2, 2))
plot(lm_size)
dev.off()
plot(lm_size$residuals, pch = 16, col = "red")  # residuals should look random
shapiro.test(residuals(lm_size))


# 4. Sex analysis  ####
# Filter on specific project
sex_project <- filter(migration_speed_nontidal, animal_project_code == "Frome")
#sex_project <- filter(migration_speed_nontidal, animal_project_code == "Grote Nete")
#sex_project <- filter(migration_speed_nontidal, animal_project_code == "Mondego")
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





# 5. WRS impact analysis by means of water body class ####

# Add water regulating structure info to the dataset
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
migration_speed_nontidal <- left_join(migration_speed_nontidal, wrs, by = c("animal_project_code","acoustic_tag_id"))
migration_speed_nontidal$animal_project_code <- factor(migration_speed_nontidal$animal_project_code, ordered = TRUE, 
                                                    levels = c("Mondego", 
                                                               "Grand Lieu Lake",
                                                               "Frome", 
                                                               "Stour",
                                                               "Nene",
                                                               "Leopold Canal",
                                                               "Grote Nete",
                                                               "Albert Canal",
                                                               "Markiezaatsmeer",
                                                               "Meuse",
                                                               "Noordzeekanaal",
                                                               "Suderpolder",
                                                               "Warnow",
                                                               "Gudena",
                                                               "Nemunas"))
migration_speed_nontidal$water_body_class <- factor(migration_speed_nontidal$water_body_class, ordered = TRUE, 
                                                       levels = c("A",
                                                                  "B",
                                                                  "C",
                                                                  "D",
                                                                  "E"))

# Boxplot per project coloured according to water body class
ggplot(migration_speed_nontidal, aes(x=animal_project_code, y=speed_ms, fill = factor(water_body_class))) +
  geom_boxplot() +
  #geom_violin(width = 2,position=position_dodge(1)) +
   scale_fill_manual(values=c("blue",
                             "#33FFFF",
                             "yellow",
                             "orange",
                             "red")) +
  ylab("Migration speed (m/s)") + 
  xlab("Water body") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 2, color = "black",
  #             position = position_dodge(width = 0.85)) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16)) 


# Boxplot per water body class
ggplot(migration_speed_nontidal, aes(x=water_body_class, y=speed_ms)) +
  geom_boxplot() +
  ylab("Migration speed (m/s)") + 
  xlab("Water body class") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 2, color = "black",
  #             position = position_dodge(width = 0.85)) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=360),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16)) 



# Conduct ANOVA or non-parametric alternative

# Check normality
qqnorm(migration_speed_nontidal$speed_ms)
qqline(migration_speed_nontidal$speed_ms)

shapiro.test(migration_speed_nontidal$speed_ms)

# Check homogeneity of variances
# Levene’s test
# Levene’s test is used to assess whether the variances of two or more populations are equal.
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
# When p > 0.05, there is no significant difference between the two variances.
car::leveneTest(speed_ms ~ water_body_class, data = migration_speed_nontidal)

# Conduct one-way ANOVA 
aov <- aov(log(migration_speed_nontidal$speed_ms) ~ migration_speed_nontidal$water_body_class)
summary(aov)

anova <- oneway.test(log(migration_speed_nontidal$speed_ms) ~ migration_speed_nontidal$water_body_class, var.equal=FALSE) # var.equal = FALSE when homogeneity of variances is not fulfilled

anova

# Check assumptions
par(mfrow=c(2,2))
plot(aov)
dev.off

# Post-hoc test for equal variances
TukeyHSD(aov)

# Post-hoc test for unequal variances
migration_speed_nontidal2 <- ungroup(migration_speed_nontidal)
migration_speed_nontidal2$speed_ms_log <- log(migration_speed_nontidal2$speed_ms)
games_howell_test(migration_speed_nontidal2, 
                  speed_ms_log ~ water_body_class,
                  conf.level = 0.95, detailed = FALSE)
#posthocTGH(log(migration_speed_nontidal$speed_ms), migration_speed_nontidal$water_body_class, method=c("games-howell"), digits=3)  # post-hoc test for unequal variances

# Kruskal-Wallis test when data is not normally distributed
kruskal.test(migration_speed_nontidal$speed_ms ~ migration_speed_nontidal$water_body_class)
#posthoc.kruskal.dunn.test(x=migration_speed_nontidal$animal_project_code, g=migration_speed_nontidal$speed_ms, p.adjust.method="bonferroni")
FSA::dunnTest(migration_speed_nontidal$speed_ms, migration_speed_nontidal$water_body_class, method="bonferroni")



# 6. Geographical location analysis ####
# Add latitude to dataset
#lat <- read_csv("./data/external/project_geographical_location.csv")
#lat$animal_project_code <- factor(lat$animal_project_code)

#migration_speed_nontidal_nobarriers <- left_join(migration_speed_nontidal_nobarriers, lat, by = "animal_project_code")

geo <- filter(migration_speed_nontidal, water_body_class == "A" |
                water_body_class == "B" | 
                water_body_class == "C")


# Plot
ggplot(geo, aes(x= release_latitude, y=speed_ms)) + 
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
lm_geo <- lm(speed_ms ~ release_latitude, data = geo) # Create the linear regression
summary(lm_geo)

par(mfrow = c(2, 2))
plot(lm_geo)
dev.off()
plot(lm_geo$residuals, pch = 16, col = "red")  # residuals should look random
shapiro.test(residuals(lm_geo))


# Approach to simply use projects as these have a specific geographical location
# Plot
ggplot(geo, aes(x=animal_project_code, y=speed_ms)) + 
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


# Conduct ANOVA or non-parametric alternative

# Check normality
qqnorm(geo$speed_ms)
qqline(geo$speed_ms)

shapiro.test(geo$speed_ms)

# Check homogeneity of variances
# Levene’s test
# Levene’s test is used to assess whether the variances of two or more populations are equal.
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
# When p > 0.05, there is no significant difference between the two variances.
car::leveneTest(speed_ms ~ animal_project_code, data = geo)

# Conduct one-way ANOVA
aov <- aov(geo$speed_ms ~ geo$animal_project_code)
summary(aov)

anova <- oneway.test(geo$speed_ms ~ geo$animal_project_code, var.equal=FALSE) # var.equal = FALSE when homogeneity of variances is not fulfilled
anova

# Check assumptions
par(mfrow=c(2,2))
plot(aov)
dev.off

# Post-hoc test for equal variances
TukeyHSD(aov)

# Post-hoc test for unequal variances
posthocTGH(geo$speed_ms, geo$animal_project_code, method=c("games-howell"), digits=3)  # post-hoc test for unequal variances

# Kruskal-Wallis test when data is not normally distributed
kruskal.test(geo$speed_ms ~ geo$animal_project_code)
#posthoc.kruskal.dunn.test(x=migration_speed_nontidal$animal_project_code, g=migration_speed_nontidal$speed_ms, p.adjust.method="bonferroni")
FSA::dunnTest(geo$speed_ms ~ geo$animal_project_code, data=geo, method="bonferroni")





# 7. Statistical analysis on whole dataset ####
# Load packages
library(nlme)
library(coefplot2)
library(multcomp)

# Calculate average day of arrival at sea per animal project code
aggregate(migration_speed_nontidal$speed_ms, list(migration_speed_nontidal$water_body_class), mean)

# Set factor
migration_speed_nontidal$water_body_class <- factor(migration_speed_nontidal$water_body_class, ordered = FALSE )

# Apply linear mixed effects model
# Full model
lmm1 <- lme(log(speed_ms) ~ release_latitude + length1 + water_body_class,
            random = ~length1 | animal_project_code,
            data = migration_speed_nontidal)

# Stepwise backward selection: remove water_body_class
lmm1 <- lme(log(speed_ms) ~ water_body_class,
            random = ~1 | animal_project_code,
            data = migration_speed_nontidal)



summary(lmm1)
anova(lmm1)

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


# Apply one-way ANOVA because only water body classes are significant and because there is no need for a random effect after backward stepwise selection
# Conduct one-way ANOVA 
aov <- aov(log(migration_speed_nontidal$speed_ms) ~ migration_speed_nontidal$water_body_class)
summary(aov)

anova <- oneway.test(log(migration_speed_nontidal$speed_ms) ~ migration_speed_nontidal$water_body_class, var.equal=FALSE) # var.equal = FALSE when homogeneity of variances is not fulfilled
anova

# Check assumptions
par(mfrow=c(2,2))
plot(aov)
dev.off

# Post-hoc test for equal variances
TukeyHSD(aov)

# Post-hoc test for unequal variances
migration_speed_nontidal2 <- ungroup(migration_speed_nontidal)
migration_speed_nontidal2$speed_ms_log <- log(migration_speed_nontidal2$speed_ms)
games_howell_test(migration_speed_nontidal2, 
                  speed_ms_log ~ water_body_class,
                  conf.level = 0.95, detailed = FALSE)

