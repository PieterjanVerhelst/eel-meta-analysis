# Calculate migration speeds between consecutive detection stations
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)
library(magrittr)
library(lubridate)
library(actel)

#browseVignettes("actel")


# Filter Gudena detection data
gudena <- read_csv("./data/interim/residencies/residency_2004_gudena.csv")
gudena$X1 <- NULL
#gudena$Array <- gudena$station_name

# Load distance matrix
# Make sure the first column is not containing the station names
gudena_distances <- read.csv("./data/external/distance_matrices/distancematrix_2004_gudena.csv",  row.names = 1)



movementSpeeds <- function(movements, speed.method, dist.mat) {
  #appendTo("debug", "Running movementSpeeds.")
  movements$swimtime_s[1] <- NA
  movements$swimdistance_m[1] <- NA
  movements$speed_m_s[1] <- NA
  if (nrow(movements) > 1) {
    capture <- lapply(2:nrow(movements), function(i) {
      if (movements$station_name[i] != movements$station_name[i - 1] & all(!grep("^Unknown$", movements$station_name[(i - 1):i]))) {
        if (speed.method == "last to first"){
          a.sec <- as.vector(difftime(movements$arrival[i], movements$departure[i - 1], units = "secs"))
          my.dist <- dist.mat[movements$station_name[i], gsub(" ", ".", movements$station_name[i - 1])]
        }
        if (speed.method == "last to last"){
          a.sec <- as.vector(difftime(movements$departure[i], movements$departure[i - 1], units = "secs"))
          my.dist <- dist.mat[movements$station_name[i], gsub(" ", ".", movements$station_name[i - 1])]
        }
        movements$swimtime_s[i] <<- round(a.sec, 6)
        movements$swimdistance_m[i] <<- round(my.dist, 6)
        movements$speed_m_s[i] <<- round(my.dist/a.sec, 6)
        rm(a.sec, my.dist)
      } else {
        movements$speed_m_s[i] <<- NA_real_
      }
    })
  }
  return(movements)
}


speed <- movementSpeeds(gudena, "last to first", gudena_distances)
View(speed)

