# Analyse the arrival time at sea of successful migrants in relation to eel size and geographical location. This reflects the silver eel migratino period.
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be


source("src/process_migration_data.R")

# Load packages ####
library(nlme)
library(coefplot2)



# 1. Load data with habitat info and join to dataset ####
habitats <- read_csv("./data/external/habitats.csv")
habitats <- habitats %>%
  mutate_at(c('animal_project_code', 'station_name', 'habitat_type', 'habitat_type2', 'habitat_type3'), as.factor) %>%
  select(animal_project_code, station_name, habitat_type, habitat_type2, habitat_type3)

data <- left_join(data, habitats, by = c("animal_project_code", "station_name"))


# 2. Rename animal_project_code to river or estuary names ####
data$animal_project_code <- recode_factor(data$animal_project_code, 
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


# 3. Link size and sex to the dataset  ####
eel <- read_csv("./data/interim/eel_meta_data.csv")
eel <- eel %>%
  mutate_at(c('acoustic_tag_id', 'animal_project_code', 'life_stage'), as.factor)
eel$length1 <- as.numeric(eel$length1)
eel$weight <- as.numeric(eel$weight)

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

# Only keep eels in the dataset from the telemetry dataset
eel <- subset(eel, acoustic_tag_id %in% data$acoustic_tag_id)

# Join eel metadata to the dataset
data <- left_join(data, eel, by = "acoustic_tag_id")
data <- rename(data, animal_project_code = animal_project_code.x)
data$animal_project_code.y <- NULL


# FINAL RECORD OF SUCCESSFUL MIGRATION ####

# 4. Load file with final detections of successful migrants ####
end_period <- read_csv("./data/interim/successful_migrants_final_detection.csv")
end_period <- end_period %>%
  mutate_at(c('acoustic_tag_id', 'animal_project_code', 'station_name'), as.factor)
end_period$arrival <- dmy_hm(end_period$arrival)
end_period$departure <- dmy_hm(end_period$departure)

# Join eel metadata to tidal dataset
end_period <- left_join(end_period, eel, by = "acoustic_tag_id")
end_period <- rename(end_period, animal_project_code = animal_project_code.x)
end_period$animal_project_code.y <- NULL

# Add water body class to the dataset
wrs <- read_csv("./data/external/eels_wrs.csv")
wrs <- select(wrs, acoustic_tag_id, wrs_impact_score, water_body_class)
end_period <- left_join(end_period, wrs, by = "acoustic_tag_id")

# Identify day number of the year based on departure date (= when an eel migrated away from the station)
end_period$daynumber <- yday(end_period$departure)
end_period$daynumber <- factor(end_period$daynumber)

# Calculate summary
end_period_summary <- end_period %>%
  group_by(animal_project_code, #wrs_impact_score, 
           water_body_class, daynumber) %>%
  #group_by(daynumber) %>%
  count()


# 5. Plot data ####
# Create dataset for barplot
plot_data <- data.frame(daynumber  = 1:365)
plot_data$daynumber_adj <- plot_data$daynumber - 151  # Adjust dataframe to plot first day in summer
for (i in 1:dim(plot_data)[1]){
  if (plot_data$daynumber_adj[i] <= (0)){
    plot_data$daynumber_adj[i] = plot_data$daynumber_adj[i] +365
  } else{
    plot_data$daynumber_adj[i] = plot_data$daynumber_adj[i]
  }}

plot_data$daynumber <- factor(plot_data$daynumber)
plot_data <- left_join(plot_data, end_period_summary, by = "daynumber")
plot_data <- replace(plot_data, is.na(plot_data), 0)  # Replace NAs with 0s
plot_data$fdaynumber <- plot_data$daynumber
plot_data$ndaynumber <- as.numeric(plot_data$daynumber)

#plot_data_project <- filter(plot_data, animal_project_code == "Meuse")

# Create barplot with number of eels per day number
waterbodies <- c("Mondego", "Grand Lieu Lake", "Loire", "Frome", "Leopold Canal", "Scheldt", "Markiezaatsmeer", "Grote Nete", "Albert Canal", "Meuse", "Stour", "Noordzeekanaal", "Nene", "Gudena", "Warnow", "Nemunas", "Alta") # This removes 'NA' from legend
# Note that project "Suderpolder" did not have successful migrants and is hence not taken up in this list.

ggplot(plot_data, aes(x=ndaynumber, y=n, fill = animal_project_code)) + 
  geom_bar(stat="identity", width = 1) +
  #scale_fill_brewer(palette="Dark2") +
  scale_fill_manual(values=c("#9933FF", 
                             "#33FFFF", 
                             "darkred", 
                             "darkblue",
                             "darkgreen",
                             "orange",
                             "yellow",
                             "#70ef2d",
                             "#652A0E",
                             "grey57",
                             "pink",
                             "deeppink",
                             "#276DC2",
                             #"honeydew3",   # One colour can be removed since Suderpolder is not in this graph
                             "black",
                             "red", 
                             "magenta",
                             "lightcyan"
  ),
  limits = waterbodies) +
  ylab("Number of eels") + 
  xlab("Day of the year") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=360),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22)) +
  scale_x_continuous(breaks = seq(0, 365, by = 30))


# Create horizontal violin plot or boxplot

plot_data_no_na <- na.omit(plot_data)

plot_data_no_na$animal_project_code <- factor(plot_data_no_na$animal_project_code, ordered = TRUE, 
                                              levels = c("Mondego", 
                                                         "Grand Lieu Lake",
                                                         "Loire", 
                                                         "Frome", 
                                                         "Stour",
                                                         "Nene",
                                                         "Scheldt",
                                                         "Leopold Canal",
                                                         "Grote Nete",
                                                         "Albert Canal",
                                                         "Markiezaatsmeer",
                                                         "Meuse",
                                                         "Noordzeekanaal",
                                                         "Suderpolder",
                                                         "Warnow",
                                                         "Gudena",
                                                         "Nemunas",
                                                         "Alta"))

# Plot for wrs impact score
# ! Activate/add wrs impact score to data
ggplot(plot_data_no_na, aes(x=animal_project_code, y=daynumber_adj, fill = factor(wrs_impact_score))) +
  #geom_boxplot() +
  geom_violin(width = 2,position=position_dodge(1)) +
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
  ylab("Day of the year") + 
  xlab("Water body") +
  stat_summary(fun = "median", geom = "point", #shape = 8,
               size = 2, color = "black",
               position = position_dodge(width = 0.85)) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16)) +
  scale_y_continuous(breaks = c(1,32,63,93,124,154,185,215,246,276,307,337), labels = c("1 June","1 Jul", "1 Aug","1 Sept","1 Oct","1 Nov", "1 Dec", "1 Jan", "1 Feb", "1 Mar", "1 Apr", "1 May")) +
  coord_flip() 


# Plot for water body class
plot_data_no_na$water_body_class <- factor(plot_data_no_na$water_body_class, ordered = TRUE, 
                                              levels = c("A", 
                                                         "B",
                                                         "C", 
                                                         "D", 
                                                         "E"))

ggplot(plot_data_no_na, aes(x=animal_project_code, y=daynumber_adj, fill = water_body_class)) +
  #geom_boxplot() +
  geom_violin(width = 1.4,position=position_dodge(1)) +
  scale_fill_manual(values=c("blue",
                             "#33FFFF",
                             "yellow",
                             "orange",
                             "red")) +
  ylab("Day of the year") + 
  xlab("Water body") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 2, color = "black",
               position = position_dodge(width = 0.85),
               show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16)) +
  scale_y_continuous(breaks = c(1,32,63,93,124,154,185,215,246,276,307,337), labels = c("1 June","1 Jul", "1 Aug","1 Sept","1 Oct","1 Nov", "1 Dec", "1 Jan", "1 Feb", "1 Mar", "1 Apr", "1 May")) +
  guides(fill=guide_legend(title="Water body \nclass")) +
  coord_flip() 



# 6. Sex analysis ####
# Calculate summary
end_period_summary_frome <- end_period %>%
  filter(animal_project_code == "Frome") %>%
  group_by(animal_project_code, sex, daynumber) %>%
  #group_by(daynumber) %>%
  count()

plot_data_frome <- data.frame(daynumber  = 1:365)
plot_data_frome$daynumber_adj <- plot_data_frome$daynumber - 151  # Adjust dataframe to plot first day in summer
for (i in 1:dim(plot_data_frome)[1]){
  if (plot_data_frome$daynumber_adj[i] <= (0)){
    plot_data_frome$daynumber_adj[i] = plot_data_frome$daynumber_adj[i] +365
  } else{
    plot_data_frome$daynumber_adj[i] = plot_data_frome$daynumber_adj[i]
  }}

plot_data_frome$daynumber <- factor(plot_data_frome$daynumber)
plot_data_frome <- left_join(plot_data_frome, end_period_summary_frome, by = "daynumber")
plot_data_frome <- replace(plot_data_frome, is.na(plot_data_frome), 0)  # Replace NAs with 0s
plot_data_frome$daynumber <- as.numeric(plot_data_frome$daynumber)
plot_data_frome$sex <- factor(plot_data_frome$sex)

ggplot(plot_data_frome, aes(x=daynumber, y=n, fill = sex)) + 
  geom_bar(stat="identity", width = 5) +
  #scale_fill_brewer(palette="Dark2") +
  scale_fill_manual(values=c("darkblue", 
                             "darkgreen"),
                    limits = c("female", "male")) +
  ylab("Number of eels") + 
  xlab("Day of the year") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=360),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(0, 365, by = 30))


# Create horizontal violin plot or boxplot

plot_data_frome_no_na <- na.omit(plot_data_frome)

plot_data_frome_no_na$sex <- factor(plot_data_frome_no_na$sex, ordered = TRUE, 
                                              levels = c("male", 
                                                         "female"))

ggplot(plot_data_frome_no_na, aes(x=sex, y=daynumber_adj)) +
  #geom_boxplot() +
  geom_violin(width = 1.5,position=position_dodge(0.5)) +
  ylab("Day of the year") + 
  xlab("Sex") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 2, color = "black",
               position = position_dodge(width = 1)) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16)) +
  scale_y_continuous(breaks = c(1,32,63,93,124,154,185,215,246,276,307,337), labels = c("1 June","1 Jul", "1 Aug","1 Sept","1 Oct","1 Nov", "1 Dec", "1 Jan", "1 Feb", "1 Mar", "1 Apr", "1 May"), limits = c(154, 246)) +
  coord_flip() 




# 7. Size analysis  ####

# Plot
end_period$daynumber <- as.character(end_period$daynumber)
end_period$daynumber <- as.numeric(end_period$daynumber)

end_period$daynumber_adj <- end_period$daynumber - 151  # Adjust dataframe to plot first day in summer
for (i in 1:dim(end_period)[1]){
  if (end_period$daynumber_adj[i] <= (0)){
    end_period$daynumber_adj[i] = end_period$daynumber_adj[i] +365
  } else{
    end_period$daynumber_adj[i] = end_period$daynumber_adj[i]
  }}

# Facet plot
ggplot(end_period, aes(x= length1, y=daynumber_adj)) + 
  geom_point() +
  facet_wrap(~animal_project_code, scales = "free") +
  ylab("Day of the year") + 
  xlab("Total length (mm)") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 12, colour = "black", angle=360),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.title.y = element_text(size = 12)) +
  #scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  scale_y_continuous(breaks = c(1,32,63,93,124,154,185,215,246,276,307,337), labels = c("1 June","1 Jul", "1 Aug","1 Sept","1 Oct","1 Nov", "1 Dec", "1 Jan", "1 Feb", "1 Mar", "1 Apr", "1 May")) +
  geom_smooth(method='lm')


# Coloured plot
ggplot(end_period, aes(x= length1, y=daynumber_adj, 
                       color=animal_project_code
                       )) + 
  geom_point() +
  scale_color_manual(values = c("Albert Canal" = "blue",
                                "Alta" = "lightblue",
                                "Frome" = "yellow2",
                                "Grand Lieu Lake" = "orange",
                                "Grote Nete" = "purple",
                                "Gudena" = "darkgreen",
                                "Leopold Canal" = "darkred",
                                "Loire" = "pink",
                                "Markiezaatsmeer" = "lightgreen",
                                "Meuse" = "red",
                                "Mondego" = "violet",
                                "Nemunas" = "green",
                                "Nene" = "darkgrey",
                                "Noordzeekanaal" = "magenta",
                                "Scheldt" = "beige",
                                "Stour" = "navy",
                                "Warnow" = "cyan")) +
  ylab("Day of the year") + 
  xlab("Total length (mm)") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 12, colour = "black", angle=360),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.title.y = element_text(size = 12)) +
  #scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  scale_y_continuous(breaks = c(1,32,63,93,124,154,185,215,246,276,307,337), labels = c("1 June","1 Jul", "1 Aug","1 Sept","1 Oct","1 Nov", "1 Dec", "1 Jan", "1 Feb", "1 Mar", "1 Apr", "1 May")) +
  geom_smooth(method='lm', se = F) +
  geom_smooth(method='lm', se = F, aes(group = 1), colour = "black", size = 1.5)



# 8. Geographical location analysis ####

# Plot
end_period$daynumber <- as.character(end_period$daynumber)
end_period$daynumber <- as.numeric(end_period$daynumber)

end_period$daynumber_adj <- end_period$daynumber - 151  # Adjust dataframe to plot first day in summer
for (i in 1:dim(end_period)[1]){
  if (end_period$daynumber_adj[i] <= (0)){
    end_period$daynumber_adj[i] = end_period$daynumber_adj[i] +365
  } else{
    end_period$daynumber_adj[i] = end_period$daynumber_adj[i]
  }}


ggplot(end_period, aes(x= release_latitude , y=daynumber_adj)) + 
  geom_point() +
  ylab("Day of the year") + 
  xlab("Release latitude") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 12, colour = "black", angle=360),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.title.y = element_text(size = 12)) +
  scale_y_continuous(breaks = c(1,32,63,93,124,154,185,215,246,276,307,337), labels = c("1 June","1 Jul", "1 Aug","1 Sept","1 Oct","1 Nov", "1 Dec", "1 Jan", "1 Feb", "1 Mar", "1 Apr", "1 May")) +
  geom_smooth(method='lm') #+
  #coord_flip() 



# 9. Statistical analysis ####

# Check if eel size was significantly different between water bodies
# --> normality
qqnorm(eel$length1)
qqline(eel$length1)
shapiro.test(eel$length1)

# --> Check homogeneity of variances
# Levene’s test
# Levene’s test is used to assess whether the variances of two or more populations are equal.
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
# When p > 0.05, there is no significant difference between the two variances.
car::leveneTest(length1 ~ animal_project_code, data = eel)

# --> assumptions not met, so conduct Kruskal-Wallis test
kruskal.test(eel$length1 ~ eel$animal_project_code)


# Calculate average day of arrival at sea per animal project code
aggregate(end_period$daynumber_adj, list(end_period$animal_project_code), mean)


# Set factor
end_period$water_body_class <- factor(end_period$water_body_class)

# Apply linear mixed effects model
# Full model
lmm1 <- lme(daynumber_adj ~ release_latitude + length1 + water_body_class,
            random = ~length1 | animal_project_code,
            data = end_period)

# Stepwise backward selection: remove water_body_class
lmm1 <- lme(daynumber_adj ~ release_latitude + length1,
            random = ~length1 | animal_project_code,
            data = end_period)

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

