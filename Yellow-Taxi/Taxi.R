setwd("C:/Users/Christopher/Desktop/Yellow Taxi/")
library(data.table)
yellowtaxi <- as.data.frame(fread("Yellow Taxi Data (Clean).csv"))

pickup <- as.data.frame(cbind(yellowtaxi$pickup_longitude, yellowtaxi$pickup_latitude))
dropoff <- as.data.frame(cbind(yellowtaxi$dropoff_longitude, yellowtaxi$dropoff_latitude))
names(pickup) <- c("lon", "lat")
names(dropoff) <- c("lon", "lat")

new.york <- map_data(map = "state", region = "New York")
ny.dat <- map_data(map = "county", region = "New York")



library(ggmap)
library(maps)
library(ggplot2)
library(scales)

InNewYork <- function(data){
  # New York is within -71.88 and -79.77 longitude, 40.49 and 45.01 latitude
  # Data must be input in the following format: 2 columns, longitude then latitude
  names(data) <- c("lon", "lat")
  bound.check <- subset(data, (lon >= -79.77 & lon <= -71.88) & lat >= 40.49 & lat <= 45.01)
  return(bound.check)
}

dropoff.clean <- InNewYork(dropoff)
pickup.clean <- InNewYork(pickup)



dev.new()
# map of new york with subregions
ny.map <- ggplot() + geom_polygon(aes(long,lat, group=group), fill="grey65", data=new.york) + geom_polygon(aes(long,lat, group=group), color='white', fill=NA, data=ny.dat) + theme_bw() + theme(axis.text = element_blank(), axis.title=element_blank()) + coord_map("polyconic")

library(globe)
ny.map + geom_hline(yintercept = c(40.49, 45.01), aes()) + geom_vline(xintercept = c(-71.88, -79.77)) + geom_point(data = geocode("JFK"), aes(x = lon, y = lat, color = "red")) + geom_point(data = geocode("Empire State Building"), aes(x = lon, y = lat, color = "blue")) + geom_point(data = pickup.clean, aes(x = lon, y = lat, color = "green"))


devtools::install_github("zachcp/nycmaps")
library(maps)
library(nycmaps)
map(database="nyc")
nyc <- map_data("nyc")

nyc.map <- ggplot() + geom_map(data = nyc, map = nyc, aes(x = long, y = lat, map_id = region), fill = "grey65")


InNewYorkCity <- function(pickup, dropoff){
  # New York City is within -74.26 and -73.70 longitude, 40.50 and 40.92 latitude
  # Data must be input in the following format: 2 data frames each with 2 columns, longitude then latitude
  bound.check <- subset(data, (lon >= -74.26 & lon <= -73.70) & lat >= 40.50 & lat <= 40.92)
  return(bound.check)
}

nyc.map + geom_hline(yintercept = c(40.50, 40.92), aes()) + geom_vline(xintercept = c(-73.70, -74.26)) + geom_point(data = pickup.clean, aes(x = lon, y = lat), color = "green") + ggtitle("NYC Pickups")

dev.new()
nyc.map + geom_hline(yintercept = c(40.50, 40.92), aes()) + geom_vline(xintercept = c(-73.70, -74.26)) + geom_point(data = dropoff.clean, aes(x = lon, y = lat), color = "blue") + ggtitle("NYC Dropoffs")



a1 <- dropoff[1, ]
a2 <- pickup[1, ]
dev.new()
nyc.map + geom_point(data = a1, aes(x = lon, y = lat), color = "green") + geom_point(data = a2, aes(x = lon, y = lat), color = "green") + geom_path(data = cbind(a1, a2), aes(x = lon, y = lat), color = "red")


# take out rows with one subset matching pickup to dropoff to ensure that the number of pickups and dropoffs is equal!!!
#




#### R Google Maps approach ####
library(RgoogleMaps)
nyc <- GetMap("New York City", zoom = 12)

dev.new()
PlotOnStaticMap(nyc, lon = pickup.clean$lon[1], lat = pickup.clean$lat[1], col = "green")

PlotOnStaticMap(nyc, lon = dropoff.clean$lon, lat = dropoff.clean$lat, col = "blue", pch = 20)


dev.new()
PlotOnStaticMap(nyc, lat = c(40.702147,40.711614,40.718217), 
                lon = c(-74.015794,-74.012318,-73.998284), 
                col=c('red', 'blue', 'green'), pch = 20, points(x = 40.702148, y = NULL ), add=TRUE)

