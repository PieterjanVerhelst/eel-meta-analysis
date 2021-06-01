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
gudena$Array <- gudena$station_name

# Load distance matrix
gudena_distances <- read_csv("./data/external/distance_matrices/distancematrix_2004_gudena.csv")
gudena_distances %<>% rename(station_name = X1)


movementSpeeds <- function(movements, speed.method, dist.mat) {
  #appendTo("debug", "Running movementSpeeds.")
  movements$Average.speed.m.s[1] <- NA
  if (nrow(movements) > 1) {
    capture <- lapply(2:nrow(movements), function(i) {
      if (movements$Array[i] != movements$Array[i - 1] & all(!grep("^Unknown$", movements$Array[(i - 1):i]))) {
        if (speed.method == "last to first"){
          a.sec <- as.vector(difftime(movements$arrival[i], movements$departure[i - 1], units = "secs"))
          my.dist <- dist.mat[movements$station_name[i], gsub(" ", ".", movements$station_name[i - 1])]
        }
        if (speed.method == "last to last"){
          a.sec <- as.vector(difftime(movements$departure[i], movements$departure[i - 1], units = "secs"))
          my.dist <- dist.mat[movements$station_name[i], gsub(" ", ".", movements$station_name[i - 1])]
        }
        movements$Average.speed.m.s[i] <<- round(my.dist/a.sec, 6)
        rm(a.sec, my.dist)
      } else {
        movements$Average.speed.m.s[i] <<- NA_real_
      }
    })
  }
  return(movements)
}


speed <- movementSpeeds(gudena, "last to first", gudena_distances)
View(speed)

