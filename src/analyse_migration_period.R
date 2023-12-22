# Analyse period of migration
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be


source("src/process_migration_data.R")


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

# Only keep eels in the dataset
eel <- subset(eel, acoustic_tag_id %in% data$acoustic_tag_id)

# Join eel metadata to tidal dataset
data <- left_join(data, eel, by = "acoustic_tag_id")
data <- rename(data, animal_project_code = animal_project_code.x)
data$animal_project_code.y <- NULL


# FIRST RECORD ####

# 4. Extract first record per migration == TRUE (= period of migration) ####
period <- data%>%
  group_by(acoustic_tag_id) %>%
  arrange(arrival) %>%
  filter(row_number()==1)

# Identify day number of the year based on departure date (= when an eel migrated away from the station)
period$daynumber <- yday(period$departure)
period$daynumber <- factor(period$daynumber)

# Calculate summary
period_summary <- period %>%
  group_by(animal_project_code, daynumber) %>%
  #group_by(daynumber) %>%
   count()

# Create dataset for barplot
plot_data <- data.frame (daynumber  = 1:365)
plot_data$daynumber <- factor(plot_data$daynumber)
plot_data <- left_join(plot_data, period_summary, by = "daynumber")
plot_data <- replace(plot_data, is.na(plot_data), 0)  # Replace NAs with 0s
plot_data$daynumber <- as.numeric(plot_data$daynumber)

plot_data_project <- filter(plot_data, animal_project_code == "Meuse")

# Create barplot with number of eels per day number
waterbodies <- c("Mondego", "Grand Lieu Lake", "Loire", "Frome", "Leopold Canal", "Scheldt", "Markiezaatsmeer", "Grote Nete", "Albert Canal", "Meuse", "Stour", "Noordzeekanaal", "Nene", "Suderpolder", "Gudena", "Warnow", "Nemunas", "Alta") # This removes 'NA' from legend

ggplot(plot_data, aes(x=daynumber, y=n, fill = animal_project_code)) + 
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
                             "red",
                             "black",
                             "honeydew3",
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



# 3. Size analysis  ####

# Plot
period$daynumber <- as.numeric(period$daynumber)

ggplot(period, aes(x= daynumber, y=length1)) + 
  geom_point() +
  facet_wrap(~animal_project_code, scales = "free") +
  ylab("Total length (mm)") + 
  xlab("Day of the year") +
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
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  geom_smooth(method='lm')




# 4. Geographical location analysis ####

# Plot
period$daynumber <- as.numeric(period$daynumber)

ggplot(period, aes(x= release_latitude , y=daynumber)) + 
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
  geom_smooth(method='lm')




# FINAL RECORD OF MIGRATION SUCCESS ####

# Identify for each project the final record of migration success
# The number of lines in each 'waterbody'_success data set equals the number of successful migrants
subset <- filter(data, animal_project_code == "Albert Canal")
period2 <- subset%>%
  group_by(acoustic_tag_id) %>%
  arrange(desc(arrival)) %>%
  filter(row_number()==1)

gudena_success <- filter(period2, distance_to_source_m > 47000)
scheldt_success <- filter(period2, station_name == "ws-DL7"|
                            station_name == "ws-2C"|
                            station_name == "ws-TRAWL"|
                            station_name == "ws-6"|
                            station_name == "ws-25A"|
                            station_name == "ws-11"|
                            station_name == "ws-borssele"|
                            station_name == "ws-WN2"|
                            station_name == "ws-HW6"|
                            station_name == "ws-PVTSS"|
                            station_name == "ws-12A"|
                            station_name == "ws-53A"|
                            station_name == "ws-GVWSP"|
                            station_name == "ws-55"|
                            station_name == "ws-DL9"|
                            station_name == "ws-42A"|
                            station_name == "ws-W6"|
                            station_name == "ws-13"|
                            station_name == "ws-15A"|
                            station_name == "ws-OG10"|
                            station_name == "ws-SP3"|
                            station_name == "ws-W7"|
                            station_name == "ws-16"|
                            station_name == "s-11"|
                            station_name == "s-12"|
                            station_name == "s-STD3")
leopold_success <- filter(period2, station_name == "bh-1"|
                            station_name == "ws-PPC"|
                            station_name == "ws-A5"|
                            station_name == "ws-4"|
                            station_name == "ws-25"|
                            station_name == "ws-11"|
                            station_name == "ws-6"|
                            station_name == "ws-15A"|
                            station_name == "ws-18"|
                            station_name == "ws-15"|
                            station_name == "ws-25B"|
                            station_name == "ws-PVTSS"|
                            station_name == "ws-K"|
                            station_name == "ws-A3"|
                            station_name == "ws-2C"|
                            station_name == "ws-13"|
                            station_name == "ws-12A"|
                            station_name == "ws-A1"|
                            station_name == "ws-23")
albert_success <- filter(period2, station_name == "s-12"|
                            station_name == "ws-WN2"|
                            station_name == "ws-PVTSS"|
                            station_name == "ws-2C"|
                            station_name == "ws-11"|
                            station_name == "ws-HW6"|
                            station_name == "ws-OGDL"|
                            station_name == "ws-53A"|
                            station_name == "ws-53"|
                            station_name == "ws-DL9"|
                            station_name == "ws-DL7"|
                            station_name == "ws-GVWSP"|
                            station_name == "ws-42A"|
                            station_name == "ws-SP3"|
                            station_name == "ws-TRAWL"|
                            station_name == "ws-18"|
                            station_name == "ws-STEEN")
grotenete_success <- filter(period2, distance_to_source_m > 80000)
grandlieulake_success <- filter(period2, distance_to_source_m > 20000)
loire_success <- filter(period2, distance_to_source_m > 80000)
warnow_success <- filter(period2, distance_to_source_m > 40000)
nemunas_success <- filter(period2, station_name == "klaipeda-1"|
                          station_name == "klaipeda-2"|
                          station_name == "klaipeda-3"|
                          station_name == "klaipeda-4")
alta_success <- filter(period2, distance_to_source_m > 6000)
mondego_success <- filter(period2, distance_to_source_m > 65000)
noordzeekanaal_success <- filter(period2, 
                            #station_name == "Grote sluis NZK"|
                            #station_name == "Middensluis NZK"|
                            station_name == "Grote sluis Noordzee"|
                            station_name == "Spuisluis Noordzee"|
                              station_name == "Kleine sluis Noordzee"|
                              #station_name == "Spuisluis NZK"|
                              #station_name == "Kleine sluis NZK"|
                              station_name == "Middensluis Noordzee")
meuse_success <- filter(period2, distance_to_source_m > 172500)
markiezaatsmeer_success <- filter(period2, distance_to_source_m > 15000)
suderpolder_success <- filter(period2, station_name == "Suderpolder-7")
stour_success <- filter(period2, distance_to_source_m > 15000)
frome_success <- filter(period2, distance_to_source_m > 12000)
nene_success <- filter(period2, distance_to_source_m > 80000)




