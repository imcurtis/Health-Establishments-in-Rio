install.packages("ggplot2")
library(ggplot2)
install.packages("ggmap")
library(ggmap)
install.packages("plyr")
library(plyr)
install.packages("dplyr")
library(dplyr)
library(maps)
library(maptools)
install.packages("maptools")

library(devtools)
install_version("ggplot2", version = "2.1.0", repos = "http://cran.us.r-project.org")

###ggplot2 stopped working
remove.packages(c("ggplot2"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)

###don't need data.table
install.packages('data.table', dependencies = TRUE)


Library(devtools)
devtools::install_github("hadley/ggplot2@v2.2.0") 
devtools::install_github("dkahle/ggmap")

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

View(rio_health)

#standardize neighborhoods



health_small <- na.omit(rio_health[c(2,3,4,7,9,10:11,18,19)])


write.csv(health_small, "health_small_1.csv")

health_small <- read.csv("health_small_1.csv")
View(health_small)

str(health_small)

####ALL COLUMNS#####

count(rio_health$V13)


p <-ggplot(rio_health, aes(V7, V13))
p +geom_bar(stat = "identity")


#type of establishment, grouped by neighborhood.
bairro <- group_by(health_small,V7,V13)
( sumbar <- summarize(bairro,count=n()) )

bairro <- health_small %>%
  count(V7,V13, sort = T)

bairro_2 <- rio_health %>%
  count(V7, sort = T)

View(bairro_2)

bai_sim <- group_by(rio_health,V7)
sim <- summarize(bai_sim,count=n())

street <- group_by(rio_health,V7,V4)
( sum <- summarize(street,count=n()) )

#most populated streets
streets <- rio_health %>%
  count(V7,V4, sort = T)

#change column names
names(health_small)[names(health_small)=="V11"] <- "lon"
names(health_small)[names(health_small)=="V10"] <- "lat"
names(health_small)[names(health_small)=="V4"] <- "logradouro"
names(health_small)[names(health_small)=="V7"] <- "Bairro"
names(health_small)[names(health_small)=="V19"] <- "TipoDeEstablecimento"
names(health_small)[names(health_small)=="V18"] <- "TipoDaUnidade"
names(health_small)[names(health_small)=="V2"] <- "Razo Social"
names(health_small)[names(health_small)=="V3"] <- "NomeDeFantasia"
names(health_small)[names(health_small)=="V9"] <- "Telefone"

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

single_map <- get_map(location =  "imperial de sao cristovao", maptype = "satellite", zoom = 13)

ggmap(single_map)


#maybe numerics will do the trick
health_small$lon<-as.numeric(as.character(health_small$lon))
health_small$lat<-as.numeric(as.character(health_small$lat))

##cleaning the bairro columns
health_small$Bairro[health_small$Bairro=="SAO CRISTOVAO"] <- "IMPERIAL DE SAO CRISTOVAO"

bairro <- rio_health %>%
  count(V7,V13, sort = T)

View(bairro)

bairro_2 <- health_small %>%
  count(Bairro, sort = T)

bairro_3 <- health_small %>%
  count(Bairro, sort = T)

View(bairro_3)

bai_small <- group_by(health_small,Bairro)
sim <- summarize(bai_small,count=n())

View(sim)

#maybe numerics will do the trick
health_small$lon<-as.numeric(as.character(health_small$lon))
health_small$lat<-as.numeric(as.character(health_small$lat))

##cleaning the bairro columns
health_small$Bairro[health_small$Bairro=="SAO CRISTOVAO"] <- "IMPERIAL DE SAO CRISTOVAO"

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

##plotting just the neighborhoods now



leblon_map <- get_map(location = "leblon", maptype = "satellite", zoom = 15)  

ggmap(leblon_map) + 
  geom_point(data= subset(health_small, Bairro=="LEBLON"),
              aes(x = lon, y = lat),
              colour = "pink",
              alpha = 0.5,
              size = 1) + ggtitle("leblon")

##custom lat/lon

hood <- subset(health_small, Bairro=="VISTA ALEGRE")
mean(hood[["lat"]])
mean(hood[["lon"]])

single <- get_map(location = c(lon=-43.31854, lat=-22.8313), maptype = "satellite", zoom = 16)

ggmap(single) + 
  geom_point(data= subset(health_small, Bairro=="VISTA ALEGRE"),
             aes(x = lon, y = lat),
             colour = "pink",
             alpha = 1,
             size = 3) + ggtitle("VISTA ALEGRE")

ggmap(single) + 
  geom_point(data= subset(health_small, Bairro==c("CIDADE NOVA","ESTACIO","SANTO CRISTO")),
             aes(x = lon, y = lat, colour = Bairro),
             alpha = 1,
             size = 2) + ggtitle("CIDADE NOVA+ESTACIO+SANTO CRISTO")

ggmap(single) + 
  geom_point(aes(x = lon, y = lat, colour = "pink"),
             data= subset(health_small, Bairro=="ACARI"),
             alpha = 0.5, size = 2) + ggtitle("ACARI")

#####FOR PRODUCING MAPS######

for (i in unique(health_small$Bairro)){
  rio_map <- get_map(location =  i, maptype = "satellite", zoom = 12)  
  ggmap(rio_map) + 
  geom_point(data= health_small,
             aes(x = lon, y = lat),
             colour = "pink",
             alpha = 0.5,
             size = 4) + ggtitle(i)
  ggsave(paste(i,".png",sep=""), dpi=300, width=6, height=5)
}

###130 maps

for (i in unique(health_small$Bairro)){
  rio_map <- get_map(location =  i, maptype = "satellite", zoom = 13)  
  ggmap(rio_map) + 
    geom_point(data= subset(health_small, Bairro==i),
               aes(x = lon, y = lat),
               colour = "pink",
               alpha = 0.5,
               size = 2) + ggtitle(i)
  ggsave(paste(i,".png",sep=""), dpi=300, width=6, height=5)
}

#writing health_small

write.csv(health_small, "health_small_1.csv")

#iterate the pink dots for the neighborhoods
#shapefile

#for every rio neighborhood in location...make a map with zoom 14

#map for every major thoroughfare with many health establishments

####INTERACTIVE MAP

install.packages("leaflet")
library(leaflet)
library(htmlwidgets)

m <- leaflet(health_small) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-43.1729, lat=-22.9068, popup="Rio de Janeiro") %>%
  addCircles(~lon, ~lat, popup=paste("Neighbohood:", health_small$Bairro, "<br>",
                                     "Name:", health_small$NomeDeFantasia, "<br>",
                                     "Street:", health_small$logradouro, "<br>",
                                     "Phone:", health_small$Telefone, "<br>",
                                     "Establishment Type:", health_small$TipoDeEstablecimento,"<br>"), 
             weight=3, radius=40,
             color="#FFC0CB", stroke = TRUE, fillOpacity = 0.8)
  saveWidget(m, file="map1.html", selfcontained=FALSE)
m  # Print the map


#### MAPVIEW
install.packages("mapview")
