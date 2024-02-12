# Analyse the migration speed in non-tidal areas according to size, sex, geographical location, migration barrier numbers and types
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be


source("src/calculate_migration_speed_habitats.R")

# 1. Select data in non-tidal areas ####

# Select freshwater = non-tidal areas
migration_speed_nontidal <- filter(migration_speed, habitat_type3 == "freshwater")

# Merge migration barrier qualification to the dataset
migrationbarriers <- read_csv("./data/external/migrationbarriers.csv")
migrationbarriers <- migrationbarriers %>%
  mutate_at(c('animal_project_code', 'barrier_impact', 'weir', 'sluice_gate', 'shipping_lock', 'hydropower', 'pump', 'barrier_type'), as.factor) %>%
  select(-'barrier_type')
migration_speed_nontidal <- left_join(migration_speed_nontidal, migrationbarriers, by = "animal_project_code")

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
migration_speed_nontidal$barrier_type <- NA
migration_speed_nontidal <- migration_speed_nontidal %>%
  mutate(barrier_type = case_when(animal_project_code == "Nemunas" & acoustic_tag_id %in% eel_semp_free ~ "none",
                                  animal_project_code == "Nemunas" ~ "hydropower",
                                  animal_project_code == "Mondego" ~ "weir",
                                  animal_project_code == "Grand Lieu Lake" ~ "weir",
                                  animal_project_code == "Loire" ~ "none",
                                  animal_project_code == "Frome" ~ "weir",
                                  animal_project_code == "Leopold Canal" ~ "pump",
                                  animal_project_code == "Scheldt" ~ "none",
                                  animal_project_code == "Markiezaatsmeer" ~ "weir",
                                  animal_project_code == "Grote Nete" ~ "none",
                                  animal_project_code == "Albert Canal" ~ "shipping_lock",
                                  animal_project_code == "Meuse" ~ "hydropower",
                                  animal_project_code == "Stour" ~ "weir",
                                  animal_project_code == "Noordzeekanaal" ~ "shipping_lock",
                                  animal_project_code == "Nene" ~ "weir",
                                  animal_project_code == "Suderpolder" ~ "shipping_lock",
                                  animal_project_code == "Gudena" ~ "none",
                                  animal_project_code == "Warnow" ~ "weir",
                                  animal_project_code == "Alta" ~ "none"))

# Two eels from Leopold Canal migrated via a weir instead of a pumping station
lc_weir_eels <- c("A69-1601-29919","A69-1601-29935")

migration_speed_nontidal <- migration_speed_nontidal %>% mutate(barrier_type=replace(barrier_type, barrier_type=='pump' & acoustic_tag_id %in% lc_weir_eels, "weir"))

# 42 eels from Noordzeekanaal migrated via a pump, followed by a shipping lock instead of only shipping locks
nz_lock_pump_eels <- c("A69-1602-2973","A69-1602-2974","A69-1602-2975","A69-1602-2976","A69-1602-2980","A69-1602-2981","A69-1602-2983","A69-1602-2988","A69-1602-2989","A69-1602-2991","A69-1602-2996","A69-1602-3004","A69-1602-3023","A69-1602-3048","A69-1602-3067","A69-1602-3077","A69-1602-3079","A69-1602-3080","A69-1602-3083","A69-1602-3084","A69-1602-3086","A69-1602-3087","A69-1602-3091","A69-1602-3093","A69-1602-3095","A69-1602-3096","A69-1602-3097","A69-1602-3098","A69-1602-3099","A69-1602-3100","A69-1602-3101","A69-1602-3103","A69-1602-3104","A69-1602-3106","A69-1602-3108","A69-1602-3109","A69-1602-3110","A69-1602-3111","A69-1602-3115","A69-1602-3116","A69-1602-3117","A69-1602-3118")

migration_speed_nontidal <- migration_speed_nontidal %>% mutate(barrier_type=replace(barrier_type, barrier_type=='shipping_lock' & acoustic_tag_id %in% nz_lock_pump_eels, "shipping_lock_pump"))


migration_speed_nontidal <- migration_speed_nontidal %>%
  mutate_at(c('acoustic_tag_id', 'animal_project_code', 'habitat_type3'), as.factor)

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
  facet_wrap(~animal_project_code) +
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





# 5. Migration barrier types analysis ####

#  Plot speed in relation to those barrier qualification
ggplot(migration_speed_nontidal, aes(x=barrier_impact, y=speed_ms)) + 
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
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16))


# Colour boxplots in relation to barrier impact
ggplot(migration_speed_nontidal, aes(x=animal_project_code, y=speed_ms, fill = barrier_impact)) + 
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
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16))


# Colour boxplots in relation to barrier type
ggplot(migration_speed_nontidal, aes(x=animal_project_code, y=speed_ms, fill = barrier_type)) + 
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
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16))


ggplot(migration_speed_nontidal, aes(x=barrier_type, y=speed_ms)) + 
geom_boxplot() +
  #scale_fill_brewer(palette="Dark2") +
  ylab("Migration speed (m/s)") + 
  xlab("Barrier type") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
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
car::leveneTest(speed_ms ~ animal_project_code, data = migration_speed_nontidal)

# Conduct one-way ANOVA 
aov <- aov(migration_speed_nontidal$speed_ms ~ migration_speed_nontidal$animal_project_code)
summary(aov)

anova <- oneway.test(migration_speed_nontidal$speed_ms ~ migration_speed_nontidal$animal_project_code, var.equal=FALSE) # var.equal = FALSE when homogeneity of variances is not fulfilled
anova

# Check assumptions
par(mfrow=c(2,2))
plot(aov)
dev.off

# Post-hoc test for equal variances
TukeyHSD(aov)

# Post-hoc test for unequal variances
posthocTGH(migration_speed_nontidal$speed_ms, migration_speed_nontidal$animal_project_code, method=c("games-howell"), digits=3)  # post-hoc test for unequal variances

# Kruskal-Wallis test when data is not normally distributed
kruskal.test(migration_speed_nontidal$speed_ms ~ migration_speed_nontidal$barrier_type)
#posthoc.kruskal.dunn.test(x=migration_speed_nontidal$animal_project_code, g=migration_speed_nontidal$speed_ms, p.adjust.method="bonferroni")
FSA::dunnTest(migration_speed_nontidal$speed_ms ~ migration_speed_nontidal$barrier_type, data=migration_speed_nontidal, method="bonferroni")


# Migration barrier analysis by means of the WRS score
# Add water regulating structure impact score to the dataset
wrs_score <- read_csv("./data/external/eels_wrs.csv")
wrs_score <- select(wrs_score, animal_project_code, acoustic_tag_id, wrs_impact_score)
wrs_score$animal_project_code <- recode_factor(wrs_score$animal_project_code, # Rename animal_project_code to river or estuary names 
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
migration_speed_nontidal <- left_join(migration_speed_nontidal, wrs_score, by = c("animal_project_code","acoustic_tag_id"))
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

# Boxplot per project coloured according to wrs impact score
ggplot(migration_speed_nontidal, aes(x=animal_project_code, y=speed_ms, fill = factor(wrs_impact_score))) +
  geom_boxplot() +
  #geom_violin(width = 2,position=position_dodge(1)) +
  scale_fill_manual(values=c("blue",
                             "#33FFFF", 
                             "lightblue",
                             "orange",
                             "yellow",
                             "#70ef2d",
                             "darkgreen",
                             "pink",
                             "deeppink",
                             "#9933FF",
                             "darkred",
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


# Boxplot per WRS impact score
ggplot(migration_speed_nontidal, aes(x=factor(wrs_impact_score), y=speed_ms)) +
  geom_boxplot() +
  ylab("Migration speed (m/s)") + 
  xlab("WRS impact score") +
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




# 6. Geographical location analysis ####
# Add latitude to dataset
#lat <- read_csv("./data/external/project_geographical_location.csv")
#lat$animal_project_code <- factor(lat$animal_project_code)

#migration_speed_nontidal_nobarriers <- left_join(migration_speed_nontidal_nobarriers, lat, by = "animal_project_code")

geo <- filter(migration_speed_nontidal, barrier_type == "none" |
              barrier_type == "hydropower" | 
              barrier_type == "weir")


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
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22)) +
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



