#' Identify migration behavior
#' 
#' Damiano Oldoni (damiano.oldoni@inbo.be)
#' Pieterjan Verhelst (pieterjan.verhelst@inbo.be)
#' 
#' R script for identifying eel migration behavior based on discussion in
#' https://github.com/PieterjanVerhelst/eel-meta-analysis/issues/17
#' 

library(readr)      # to read csv files
library(dplyr)      # to do data wrangling
library(tidyr)      # to organize tabular data
library(tidylog)    # to get useful messages about dplyr and tidyr operations
library(purrr)      # to do functional programming
library(assertthat) # to declare the conditions the code should satisfy
library(lubridate)  # to work with datetime
library(ggplot2)    # to create plots
library(plotly)     # to make ggplot plots interactive


# read input data
eel_df <- read_csv("./data/interim/speed/speed_2019_grotenete.csv")
eel_df <- eel_df %>%
  rename(row_id = "...1")

# define thresholds
dist_for_speed <- 5000 # threshold in meter
migration_speed_threshold <- 0.1 # speed threshold in m/s

# customized `min()` function to avoid warning while calculating min() of empty
# vectors
custom_min <- function(x) {
  if (length(x)>0) min(x) else NA
}

# define core function to find migration periods
get_migrations <- function(df, 
                           dist_threshold = dist_for_speed, 
                           speed_threshold = migration_speed_threshold) {
  ## check inputs
  # df is a data.frame
  assertthat::assert_that(is.data.frame(df))
  # dist_threshold is a number
  assertthat::assert_that(is.numeric(dist_threshold))
  # speed_threshold is a number
  assertthat::assert_that(is.numeric(speed_threshold))
  # we make use of some columns in df. So, they need to be present in df
  assertthat::assert_that("totaldistance_m" %in% names(df),
                          msg = "Column `totaldistance_m` not found in df."
  )
  assertthat::assert_that("arrival" %in% names(df),
                          msg = "Column `arrival` not found in df."
  )
  df %>%
    rowwise() %>%
    mutate(first_dist_to_use = custom_min(
      df$totaldistance_m[df$totaldistance_m >= dist_threshold])) %>%
    mutate(row_first_dist_to_use = if_else(
      !is.na(first_dist_to_use),
      which(df$totaldistance_m == first_dist_to_use)[1],
      NA)) %>%
    ungroup() %>%
    mutate(time_first_dist_to_use = if_else(!is.na(row_first_dist_to_use),
                                        df$arrival[row_first_dist_to_use],
                                        NA)) %>%
    mutate(delta_totdist = first_dist_to_use - totaldistance_m) %>%
    mutate(delta_t = as.numeric(as.duration(time_first_dist_to_use - arrival))) %>%
    mutate(migration_speed = (delta_totdist / delta_t)) %>%
    mutate(downstream_migration = migration_speed >= speed_threshold)
}

# calculate dist + dist_for_speed
eel_df <- eel_df %>%
  mutate(dist_threshold = totaldistance_m + dist_for_speed)

# Apply get_migrations to each eel
eel_df <- eel_df %>%
  group_by(acoustic_tag_id) %>%
  nest() %>%
  mutate(migration_infos = map(data, function(x) {
    get_migrations(x, 
                   dist_threshold = dist_for_speed, 
                   migration_speed_threshold)
    }), .keep = "none") %>%
  unnest(migration_infos)

View(eel_df)

# select one eel
acoustic_tag_id_example <- "A69-9006-4038"
eel_example <- eel_df %>%
  filter(acoustic_tag_id == acoustic_tag_id_example)
# plot
plot_example <- ggplot(eel_example, aes(x = arrival, y = totaldistance_m)) +
  geom_point(aes(color = downstream_migration), size = 3) +
  geom_line() + 
  scale_y_reverse() +
  ggplot2::labs(title = sprintf("%s, speed: %s, min dist for speed calc: %s", 
                                acoustic_tag_id_example,
                                migration_speed_threshold,
                                dist_for_speed)
  )
                                
plot_example

# interactive plot version
ggplotly(plot_example)
