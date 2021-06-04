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
  g <- g + geom_line(aes(arrival, totaldistance_m), data = mydfnew.temp, colour = "black", size = 1)
  g <- g + geom_point(aes(arrival, totaldistance_m), data = mydfnew.temp, shape = 1, size = 5, colour = "black")
  g <- g + theme(plot.title = element_text(lineheight=.8, face="bold", size=20))
  g <- g + scale_y_continuous(limit = c(0, 90000),breaks = c(0,10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000), labels = c(0,10,20,30,40,50,60,70,80,90))
  g <- g + ggtitle(mydfnew.temp$tag_id)
  g <- g + ylab("Distance (km)")
  g <- g + xlab("Date")
  g <- g + geom_hline(yintercept = mydfnew.temp$totaldistance_m, colour = "gray", size = 0.5, linetype = "dashed")
  g <- g + annotate("text",x = mydfnew.temp$arrival[1], y = mydfnew.temp$totaldistance_m, label = mydfnew.temp$station_name, hjust=0, colour="red", size = 5)
  print(g)
}

dev.off()


## Create single plot

# Select individual
data2=data[which(data$tag_id == "A69-1008-200"), ]
#data2=data2[order(as.POSIXct(strptime(data2$Arrival,"%d/%m/%Y %H:%M"))),]
data2=data2[order(as.POSIXct(strptime(data2$arrival,"%Y-%m-%d %H:%M:%S"))),]


# Create column with arrival date format 'yyyy-mm-dd'
data2$Arrival2<-as.POSIXct(strptime(data2$Arrival,"%Y-%m-%d"))


# Create plot
ggplot() + geom_line(aes(arrival, totaldistance_m), data = data2, colour = "black", size = 1) + geom_point(aes(arrival, totaldistance_m), data = data2, shape = 1, size = 5, colour = "black") + ggtitle(data2$tag_id) +  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + ylab("Distance (km)") + xlab("Date") + 
  scale_y_continuous(limit = c(0, 100000),breaks = c(0,10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000), labels = c(0,10,20,30,40,50,60,70,80,90,100)) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22)) +
  geom_hline(yintercept = data2$totaldistance_m, colour = "gray", size = 0.5, linetype = "dashed") +
  annotate("text",x = data2$arrival[1], y = data2$totaldistance_m, label = data2$station_name, hjust=0, colour="red", size = 5)





# Create plot without stations and line between ZS and WS
ggplot() + geom_line(aes(Arrival2, Station_distance), data = data2, colour = "black", size = 1) + geom_point(aes(Arrival2, Station_distance), data = data2, shape = 1, size = 5, colour = "black") + ggtitle(data2$Transmitter) +  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + ylab("Distance (km)") + xlab("Date") + 
  scale_y_continuous(limit = c(0, 160000),breaks = c(0,20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000), labels = c(0,20,40,60,80,100,120,140,160)) + 
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22)) +
  geom_hline(yintercept = data2$Station_distance, colour = "gray", size = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 97781, colour = "black", size = 1.0) 



# Zoom in on specific area
ggplot() + geom_line(aes(Arrival2, Station_distance), data = data2, colour = "black", size = 1) + geom_point(aes(Arrival2, Station_distance), data = data2, shape = 1, size = 5, colour = "black") + ggtitle(data2$Transmitter) +  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + ylab("Distance (km)") + xlab("Date") + 
  scale_y_continuous(limit = c(60000, 82000),breaks = c(60000, 82000), labels = c(60,82)) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22)) +
  geom_hline(yintercept = data2$Station_distance, colour = "gray", size = 0.5, linetype = "dashed") +
  annotate("text",x = data2$Arrival2[1], y = data2$Station_distance, label = data2$station, hjust=0, colour="red", size = 5)


