#' Functions to clean df data for a specific animal project
#' 
#' Damiano Oldoni (damiano.oldoni@inbo.be), Pieterjan Verhelst
#' (pieterjan.verhelst@inbo.be)

#' Clean df data for ESGL animal project
#' 
#' @param df A data.frame with df data
#' 
#' @return A data.frame with clean data, same columns as input `df`
clean_df_esgl <- function(df) {
  # Check inputs
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(
    "animal_project_code" %in% names(df),
    msg = "Column `animal_project_code` is used and must be present in df."
  )
  assertthat::assert_that(
    "arrival" %in% names(df),
    msg = "Column `arrival` is used and must be present in df."
  )
  assertthat::assert_that(
    "acoustic_tag_id" %in% names(df),
    msg = "Column `acoustic_tag_id` is used and must be present in df."
  )
  
  # Clean data
  # False detection in project ESGL after 2016-02-15 and the detection of eel
  # A69-1601-38319 at station A on 2016-01-02 06:59:09
  # A69-1601-38358 is not a migratory eel based on expert judgement. The release location may not be entirely correct and since its track is borderline 4000 m, I decide to remove it from the dataset.
  df <- df[!(df$animal_project_code == "ESGL" & df$arrival >= '2016-02-15 00:00:00'),]
  df <- df[!(df$animal_project_code == "ESGL" & df$acoustic_tag_id == "A69-1601-38319" &
                             df$arrival >= '2016-01-02 06:59:09' &
                             df$arrival <= '2016-01-02 07:59:09'),]
  df <- df[!(df$animal_project_code == "ESGL" & df$acoustic_tag_id == 'A69-1601-38358'),]
  return(df)
}

#' Clean df data for 2011_warnow animal project
#' 
#' @param df A data.frame with df data
#' 
#' @return A data.frame with clean data, same columns as input `df`
clean_df_2011_warnow <- function(df) {
  # Check inputs
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(
    "animal_project_code" %in% names(df),
    msg = "Column `animal_project_code` is used and must be present in df."
  )
  assertthat::assert_that(
    "arrival" %in% names(df),
    msg = "Column `arrival` is used and must be present in df."
  )
  assertthat::assert_that(
    "station_name" %in% names(df),
    msg = "Column `station_name` is used and must be present in df."
  )
  assertthat::assert_that(
    "acoustic_tag_id" %in% names(df),
    msg = "Column `acoustic_tag_id` is used and must be present in df."
  )
  
  # Clean data
  
  # Step 1. False detections of station W3 in 2011 warnow project: detections at
  # W3 same day as release, while W3 is ca. 25 km downstream of release see
  # github issue
  # https://github.com/PieterjanVerhelst/eel-meta-analysis/issues/20
  df$date <- as.Date(df$arrival)
  df <- df %>%
    group_by(acoustic_tag_id) %>%
    mutate(start_date = dplyr::first(date)) %>%
    filter((station_name != "W3") | (station_name == "W3" & date != start_date)) %>%
    filter((station_name != "W4") | (station_name == "W4" & date != start_date))
  
  # remove help columns
  df$date <- NULL
  df$start_date <- NULL
  
  #Step 2. Seven eels within the 2011_warnow project had dubious detections at
  #station W3. They seem to 'jump' from upper stations to W3 and back, spanning
  #over 20 km over very short time periods which is highly unlikely for eels.
  #Hence, these detections at station W3 are considered false detections and
  #need to be removed. The eels having such false detections are 539, 542, 555,
  #570, 620, 633 and 649.
  #https://github.com/PieterjanVerhelst/eel-meta-analysis/issues/25
  df$arrival_numeric <- as.numeric(df$arrival)  # Set numeric: works easier to remove line
  
  df <- df[!(df$animal_project_code == "2011_Warnow" & df$acoustic_tag_id == "A69-1601-539" &
                             df$arrival_numeric == 1309160880),]
  df <- df[!(df$animal_project_code == "2011_Warnow" & df$acoustic_tag_id == "A69-1601-542" &
                             df$station_name == "W3"),]
  df <- df[!(df$animal_project_code == "2011_Warnow" & df$acoustic_tag_id == "A69-1601-555" &
                             df$arrival_numeric > 1309812238 &
                             df$arrival_numeric < 1310376350),]
  df <- df[!(df$animal_project_code == "2011_Warnow" & df$acoustic_tag_id == "A69-1601-570" &
                             df$arrival_numeric == 1311038460),]
  df <- df[!(df$animal_project_code == "2011_Warnow" & df$acoustic_tag_id == "A69-1601-570" &
                             df$arrival_numeric == 1311053160),]
  df <- df[!(df$animal_project_code == "2011_Warnow" & df$acoustic_tag_id == "A69-1601-570" &
                             df$arrival_numeric == 1311064800),]
  df <- df[!(df$animal_project_code == "2011_Warnow" & df$acoustic_tag_id == "A69-1601-570" &
                             df$arrival_numeric > 1311064899 &
                             df$arrival_numeric < 1311282481),]
  df <- df[!(df$animal_project_code == "2011_Warnow" & df$acoustic_tag_id == "A69-1601-620" &
                             df$arrival_numeric == 1319453082),]
  df <- df[!(df$animal_project_code == "2011_Warnow" & df$acoustic_tag_id == "A69-1601-633" &
                             df$arrival_numeric == 1319043271),]
  df <- df[!(df$animal_project_code == "2011_Warnow" & df$acoustic_tag_id == "A69-1601-649" &
                             df$arrival_numeric > 1319670143 &
                             df$arrival_numeric < 1319732079),]
  
  df$arrival_numeric <- NULL # Remove column
  
  # Step 3. Remove three eels that identified as American eel (Anguilla rostrata)
  df <- df %>%
    filter(animal_project_code == "2011_Warnow" & acoustic_tag_id != "A69-1601-582", # American eel
           animal_project_code == "2011_Warnow" & acoustic_tag_id != "A69-1601-632", # American eel
           animal_project_code == "2011_Warnow" & acoustic_tag_id != "A69-1601-634", # American eel
           animal_project_code == "2011_Warnow" & acoustic_tag_id != "A69-1601-542", # Eel was recaptured twice so unrealistic track
           animal_project_code == "2011_Warnow" & acoustic_tag_id != "A69-1601-645") # Very short track of only 5 km at one receiver
  
  return(df)
}

#' Clean df data for 2013_albertkanaal animal project
#' 
#' @param df A data.frame with df data
#' 
#' @return A data.frame with clean data, same columns as input `df`
clean_df_2013_albertkanaal <- function(df) {
  # Check inputs
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(
    "station_name" %in% names(df),
    msg = "Column `station_name` is used and must be present in df."
  )
  
  # Clean data
  # Remove wrong release location in 2013_albertkanaal No need to adjust exact
  # release location, since eels were released next to receiver, leading to
  # detection right after release
  # --> exact release positions was HH5 instead of rel_albertkanaal2
  df <- subset(df, station_name != "rel_albertkanaal2")
  return(df)
}

#' Clean df data for nedap_meuse animal project
#' 
#' @param df A data.frame with df data
#' 
#' @return A data.frame with clean data, same columns as input `df`
clean_df_nedap_meuse <- function(df) {
  # Check inputs
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(
    "arrival" %in% names(df),
    msg = "Column `arrival` is used and must be present in df."
  )
  assertthat::assert_that(
    "station_name" %in% names(df),
    msg = "Column `station_name` is used and must be present in df."
  )
  assertthat::assert_that(
    "acoustic_tag_id" %in% names(df),
    msg = "Column `acoustic_tag_id` is used and must be present in df."
  )
  assertthat::assert_that(
    "departure" %in% names(df),
    msg = "Column `departure` is used and must be present in df."
  )
  # Clean data
  # For 58 eels from the NEDAP Meuse project tagged in 2013 the release date is
  # wrong: this should be 2013-09-11 instead of 2013-11-09.
  df$arrival_numeric <- as.numeric(df$arrival)  # Set arrival to numeric as this makes filtering/subsetting easier
  wrong_release_dates <- df[(df$station_name == "rel_nedap_meuse" & df$arrival_numeric == 1383951600),] # Isolate  records with wrong release dates
  unique(wrong_release_dates$acoustic_tag_id)   # Gives number of unique tag IDs
  wrong_release_dates$arrival <- '2013-09-11 00:00:00'  # Correct release date
  wrong_release_dates$departure <- '2013-09-11 00:00:00'  # Correct release date
  
  df <- df[!(df$station_name == "rel_nedap_meuse" & df$arrival_numeric == 1383951600),]  # Remove records with wrong release dates
  df <- rbind(df, wrong_release_dates)  # Bind records with correct release dates
  
  df$arrival_numeric <- NULL   # Remove column with arrival numeric which became now redundant
  
  return(df)
}


#' Clean df data for 2015_phd_verhelst_eel animal project
#' 
#' @param df A data.frame with df data
#' 
#' @return A data.frame with clean data, same columns as input `df`
clean_df_2015_phd_verhelst_eel <- function(df) {
  # Check inputs
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(
    "animal_project_code" %in% names(df),
    msg = "Column `animal_project_code` is used and must be present in df."
  )
  assertthat::assert_that(
    "arrival" %in% names(df),
    msg = "Column `arrival` is used and must be present in df."
  )
  assertthat::assert_that(
    "acoustic_tag_id" %in% names(df),
    msg = "Column `acoustic_tag_id` is used and must be present in df."
  )
  
  # Clean data
  # Eels tagged in the Dijle did not lead to long, qualitative tracks. Hence, remove these eels from the data
  df <- df[!(df$animal_project_code == "2015_phd_verhelst_eel" & df$acoustic_tag_id == "A69-1601-58623"),]
  df <- df[!(df$animal_project_code == "2015_phd_verhelst_eel" & df$acoustic_tag_id == "A69-1601-58618"),]
  df <- df[!(df$animal_project_code == "2015_phd_verhelst_eel" & df$acoustic_tag_id == "A69-1601-58613"),]
  df <- df[!(df$animal_project_code == "2015_phd_verhelst_eel" & df$acoustic_tag_id == "A69-1601-58612"),]
  df <- df[!(df$animal_project_code == "2015_phd_verhelst_eel" & df$acoustic_tag_id == "A69-1601-58617"),]
  df <- df[!(df$animal_project_code == "2015_phd_verhelst_eel" & df$acoustic_tag_id == "A69-1601-58622"),]
  df <- df[!(df$animal_project_code == "2015_phd_verhelst_eel" & df$acoustic_tag_id == "A69-1601-58621"),]
  df <- df[!(df$animal_project_code == "2015_phd_verhelst_eel" & df$acoustic_tag_id == "A69-1601-58616"),]
  df <- df[!(df$animal_project_code == "2015_phd_verhelst_eel" & df$acoustic_tag_id == "A69-1601-58611"),]
  return(df)
}


#' Clean df data for 2004_gudena animal project
#' 
#' @param df A data.frame with df data
#' 
#' @return A data.frame with clean data, same columns as input `df`
clean_df_2004_gudena <- function(df) {
  # Check inputs
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(
    "animal_project_code" %in% names(df),
    msg = "Column `animal_project_code` is used and must be present in df."
  )
  assertthat::assert_that(
    "arrival" %in% names(df),
    msg = "Column `arrival` is used and must be present in df."
  )
  assertthat::assert_that(
    "acoustic_tag_id" %in% names(df),
    msg = "Column `acoustic_tag_id` is used and must be present in df."
  )
  
  # Clean data
  # False detections in project 2004_gudena for the following eels after a certain date:
  # A69-1008-101: arrival > 2004-05-14  00:00:00
  # A69-1008-185: arrival > 2006-03-10  00:00:00
  # A69-1008-200: arrival > 2005-09-12  00:00:00
  
  df <- df[!(df$animal_project_code == "2004_Gudena" & df$acoustic_tag_id == "A69-1008-101" &
               df$arrival > '2004-05-14 00:00:00'),]
  df <- df[!(df$animal_project_code == "2004_Gudena" & df$acoustic_tag_id == "A69-1008-185" &
               df$arrival > '2006-03-10 00:00:00'),]
  df <- df[!(df$animal_project_code == "2004_Gudena" & df$acoustic_tag_id == "A69-1008-200" &
               df$arrival > '2005-09-12 00:00:00'),]
  return(df)
}


#' Clean df data for 2012_leopoldkanaal animal project
#' 
#' @param df A data.frame with df data
#' 
#' @return A data.frame with clean data, same columns as input `df`
clean_df_2012_leopoldkanaal <- function(df) {
  # Check inputs
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(
    "animal_project_code" %in% names(df),
    msg = "Column `animal_project_code` is used and must be present in df."
  )
  assertthat::assert_that(
    "arrival" %in% names(df),
    msg = "Column `arrival` is used and must be present in df."
  )
  assertthat::assert_that(
    "acoustic_tag_id" %in% names(df),
    msg = "Column `acoustic_tag_id` is used and must be present in df."
  )
  
  # Clean data
  # Doubtfull detections of eel A69-1601-29959: after it reached the Scheldt Estuary (ws2) it kept on being detected for 3 years. The eel might have died or predated. Yet, the uncertainty if this eel was still alive, makes me decide to remove those detections
  # The same is true for eel A69-1601-29956
  df <- df[!(df$animal_project_code == "2012_leopoldkanaal" & df$acoustic_tag_id == "A69-1601-29959" &
               df$arrival > '2013-07-21 00:00:00'),]
  df <- df[!(df$animal_project_code == "2012_leopoldkanaal" & df$acoustic_tag_id == "A69-1601-29956" &
               df$arrival > '2012-10-28 12:00:00'),]
  return(df)
}


#' Generic clean function.
#'
#' This function calls under the hood the specific cleaning function written for
#' the specific animal project level.
#' 
#' @param df A data.frame
#' @param animal_project_code A string with the animal project code. It must be
#'   one of: `"ESGL"`, `"2011_warnow"`, `"2013_albertkanaal"`, `"nedap_meuse"`, `"2015_phd_verhelst_eel"`, `"2004_gudena"`, `"2012_leopoldkanaal"`.
#'   
#' @return A cleaned data.frame
clean_df <- function(df, animal_project_code) {
  # Check inputs
  assertthat::assert_that(is.data.frame(df))
  # Check animal_project_code
  animal_project_codes <- c(
    "ESGL",
    "2011_warnow",
    "2013_albertkanaal",
    "nedap_meuse",
    "2015_phd_verhelst_eel",
    "2004_gudena",
    "2012_leopoldkanaal"
  )
  assertthat::assert_that(
    animal_project_code %in% animal_project_codes,
    msg = paste0("Invalid value for `animal_project_code`. It must be one of: ",
                 paste(animal_project_codes, collapse = ", ",
                       ".")
    )
  )
  
  # Run specific cleaning function
  fun_name <- paste0("clean_df_", tolower(animal_project_code))
  message(sprintf("Cleaning residency data using function `%s()`.", fun_name))
  do.call(fun_name, args = list(df = df))
}