# Create plots with travelled distance and store as .pdf
# by Ine Pauwels, Pieterjan Verhelst & Stijn Bruneel
# ine.pauwels@inbo.be, Pieterjan.Verhelst@UGent.be, Stijn.Bruneel@ugent.be

library(tidyverse)
library(lubridate)


# Upload dataset
data <- read_csv('./data/interim/speed/speed_2004_gudena.csv') 
data$X1 <- NULL
data$tag_id <- factor(data$tag_id)

# Create plot for all eels in single pdf
#data$arrival <- ymd_hms(data$arrival)
#data$arrival <- as.POSIXct(strptime(data$arrival,"%Y-%m-%d %H:%M:%S"))


mydfnew.split.eel <- split(data, data$tag_id) # split dataset based on tag IDs
pdf("./figures/distance_tracks/distance_tracks_2004_gudena.pdf") # Create pdf


for (i in 1:length(mydfnew.split.eel)){ #i van 1 tot aantal transmitters
  mydfnew.temp<-mydfnew.split.eel[[i]] #for loop wordt doorlopen voor elke i transmitter
  g <- ggplot()
  g <- g + geom_line(aes(arrival, distance_to_source_m), data = mydfnew.temp, colour = "black", size = 1)
  g <- g + geom_point(aes(arrival, distance_to_source_m), data = mydfnew.temp, shape = 1, size = 5, colour = "black")
  g <- g + theme(plot.title = element_text(lineheight=.8, face="bold", size=20))
  g <- g + scale_y_continuous(limit = c(0, 60000),breaks = c(0,10000, 20000, 30000, 40000, 50000, 60000), labels = c(0,10,20,30,40,50,60))
  g <- g + ggtitle(mydfnew.temp$tag_id)
  g <- g + ylab("Distance (km)")
  g <- g + xlab("Date")
  g <- g + geom_hline(yintercept = mydfnew.temp$distance_to_source_m, colour = "gray", size = 0.5, linetype = "dashed")
  g <- g + annotate("text",x = mydfnew.temp$arrival[1], y = mydfnew.temp$distance_to_source_m, label = mydfnew.temp$station_name, hjust=0, colour="red", size = 5)
  print(g)
}

dev.off()





## Create single plot

# Select individual
data2 <- data[which(data$tag_id == "A69-1008-214"), ]
#data2=data2[order(as.POSIXct(strptime(data2$Arrival,"%d/%m/%Y %H:%M"))),]
data2 <- data2[order(as.POSIXct(strptime(data2$arrival,"%Y-%m-%d %H:%M:%S"))),]


# Create plot
ggplot() + geom_line(aes(arrival, distance_to_source_m), data = data2, colour = "black", size = 1) + geom_point(aes(arrival, distance_to_source_m), data = data2, shape = 1, size = 5, colour = "black") + ggtitle(data2$tag_id) +  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + ylab("Distance (km)") + xlab("Date") + 
  scale_y_continuous(limit = c(0, 60000),breaks = c(0,10000, 20000, 30000, 40000, 50000, 60000), labels = c(0,10,20,30,40,50,60)) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22)) +
  geom_hline(yintercept = data2$distance_to_source_m, colour = "gray", size = 0.5, linetype = "dashed") +
  annotate("text",x = data2$arrival[1], y = data2$distance_to_source_m, label = data2$station_name, hjust=0, colour="red", size = 5)





