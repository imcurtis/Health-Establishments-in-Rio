library(ggplot2)
install.packages("ggmap")
library(ggmap)
install.packages("plyr")
library(plyr)
library(maps)
library(maptools)
install.packages("maptools")

install.packages("Rcpp")

#Type in a Rio neighborhood and get a heat map of all the health establishments there.

#Type in neighborhood and zoom and get a map of all the health establishements in that
#part of RIO

#Main decision axes, what neighborhood you are in and the type of establishment (service)

#Heatmap, establishment vs. neighborhood, and count

#recommendation algorithm for establishing new kind of health service in a community

###########

# load the data
rio_health <- read.csv("estabelecimentos_.csv", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

health_small <- na.omit(rio_health[c(4,7,10:11,18,19)])
View(health_small)

#change column names
names(health_small)[names(health_small)=="V11"] <- "lon"
names(health_small)[names(health_small)=="V10"] <- "lat"
names(health_small)[names(health_small)=="V4"] <- "logradouro"
names(health_small)[names(health_small)=="V7"] <- "Bairro"
names(health_small)[names(health_small)=="V19"] <- "TipoDeEstablecimento"
names(health_small)[names(health_small)=="V18"] <- "TipoDaUnidade"

#deleting first row
health_small <- health_small[-1,]
View(health_small)

#count of different variables
count(health_small, "TipoDeEstablecimento")
count(health_small, "Bairro")

#isolating the clinics
clinica <- subset(health_small, TipoDeEstablecimento == "CLINICA/CENTRO DE ESPECIALIDADE")
count(clinica, "Bairro")

#isolating private practice
isolado <- subset(health_small, TipoDeEstablecimento == "CONSULTORIO ISOLADO")
count(isolado, "Bairro")

# Download the base map
row.names(health_small) <- NULL
View(health_small)

for (i in unique(health_small$Bairro)){
rio_map <- get_map(location =  "i", maptype = "satellite", zoom = 14)
}

rio_map <- get_map(location =  "leblon", maptype = "satellite", zoom = 12)

rio_map <- get_map(location = c(-22.9068,-43.1729), maptype = "satellite", zoom = 11)

View(rio_map)

(lon = -22.9068, lat = -43.1729)

#maybe numerics will do the trip
health_small$lon<-as.numeric(as.character(health_small$lon))
health_small$lat<-as.numeric(as.character(health_small$lat))


#health_small <- health_small[-1,]
View(health_small)

as.factor(health_small$lon)
as.factor(health_small$lat)

as.numeric(health_small$lon)
as.numeric(health_small$lat)

#shapefile
install.packages("rgdal")
library(rgdal)
rio.map.shp<- readOGR(dsn = "/Users/amcurtis91/Rio Data Sets and Analysis/LimiteMunicipioCEPERJ", layer = "LimiteMunicipioCEPERJ")
sh <- readOGR(dsn = "/Users/amcurtis91/Rio Data Sets and Analysis/Limite_Bairro", layer = "Limite_Bairro")

plot(sh)
plot(rio.map.shp)

#GGPLOT 
points <- fortify(sh, region = "coords")

# Plot the neighborhoods
rio <- qmap("Rio de Janeiro", zoom=10)
rio +geom_polygon(aes(x=long,y=lat, group=group, alpha=0.25), data=points, fill='white') +
  geom_polygon(aes(x=long,y=lat, group=group), data=points, color='black', fill=NA)

# Draw the heat map
ggmap(rio_map, extent = "device") + geom_density2d(data = health_small, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = health_small, 
  aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)


ggmap(rio_map_g_str, extent = "device") + geom_density2d(data = health_small, 
  aes(x = lon, y = lat), size = 0.3) + stat_density2d(data = health_small, 
  aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
  bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

#plotting points on a map

mapPoints <- ggmap(rio_map_g_str) +
  + geom_point(aes(x = lon, y = lat, size = 0.3, data = health_small, alpha = .5)

ggmap(rio_map_g_str) + geom_point(data = health_small, aes(x = lon, y = lat), color="red",size = 2, alpha=0.5)
  
#custom zoom on map

for (i in unique(health_small$Bairro)){
  rio_map <- get_map(location =  "i", maptype = "satellite", zoom = 14)
}

for (i in unique(health_small$Bairro)){
  rio_map <- get_map(location =  i, maptype = "satellite", zoom = 14)  
  ggmap(rio_map) + 
  geom_point(data= health_small,
             aes(x = lon, y = lat),
             colour = "pink",
             alpha = 0.5,
             size = 4) + ggtitle(i)
  ggsave(paste(i,".png",sep=""), dpi=300, width=6, height=5)
}

#iterate the pink dots for the neighborhoods
#shapefile

#for every rio neighborhood in location...make a map with zoom 14

#map for every major thoroughfare with many health establishments


