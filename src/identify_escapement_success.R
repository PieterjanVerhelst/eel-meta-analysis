# Identify migration success as successful escapement to the sea
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be



source("src/process_migration_data.R")



# 1. Rename animal_project_code to river or estuary names ####
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



# 2. Identify for each project the final record of migration ####
# The number of lines in each 'waterbody'_success data set equals the number of successful migrants
# This info is used to generate the file "./data/external/escapement_success.csv"
subset <- filter(data, animal_project_code == "Nene")
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
grandlieulake_success <- filter(period2, distance_to_source_m > 40000)
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


# 3. Create file with final record per successful eel ####
period_end <- do.call("rbind", list(gudena_success,
                                    scheldt_success,
                                    leopold_success,
                                    albert_success,
                                    grotenete_success,
                                    grandlieulake_success,
                                    loire_success,
                                    warnow_success,
                                    nemunas_success,
                                    alta_success,
                                    mondego_success,
                                    noordzeekanaal_success,
                                    meuse_success,
                                    markiezaatsmeer_success,
                                    suderpolder_success,
                                    stour_success,
                                    frome_success,
                                    nene_success))


# 4. Write csv file ####
write.csv(period_end, "./data/interim/successful_migrants_final_detection.csv")







