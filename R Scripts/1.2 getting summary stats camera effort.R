##distance between traps 
library(tidyverse)
library(geosphere)
#read in coords

MScoords <- read.csv(file = "Raw Data/LatLongSites.csv", header = TRUE)

#filter to just useful 
MScoords <- MScoords %>%
  filter(!Site.ID == "MS003",
         !Site.ID == "MS004",
         !Site.ID == "MS008",
         !Site.ID == "MS014",
         !Site.ID == "MS020",
         !Site.ID == "MS022",
         !Latitude == "NA")

distances <- as.matrix(MScoords[, c("Longitude", "Latitude")], fun = distHaversine)


mean_distance <- distm(distances, fun = distHaversine)


min_distances <- apply(mean_distance, 1, function(row) min(row[row > 0]))

average_min_distance <- mean(min_distances)
closest_pair_range <- range(min_distances)


###Now for camera servicing : 

servtimes <- read.csv(file = "Raw Data/Camera Service Times.csv", header = T)

servtimes <- 
  pivot_longer(servtimes,
               cols =c(MS001:MS026),
               names_to = "Site",
               values_to = "ServTime")%>%
  filter(!ServTime == "NA")


print(servtimes)

mean(servtimes$ServTime)
range(servtimes$ServTime)

#End