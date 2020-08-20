
# ---- VECTOR OPERATIONS ----

#In this example, we will create buffers of 50 m around a series of camera traps in 
#Honduras, and then clip those buffers to Honduras land area

library(sf)

#read in the shapefile with st_read
PAs <- st_read("G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Honduras/Honduras_Protected_Areas_2007.shp")

#let's see what the columns of the attribute table are
names(PAs)

#alternatively, to see the top rows of data
head(PAs)

#I wonder what the coordinate system is?
st_crs(PAs)

#What is our extent?
st_bbox(PAs) 

#how many features are in the object?
nrow(PAs)

#can also get all this information just by running the name of the multipolygon
PAs


#let's plot this multipolygon
library(ggplot2)
ggplot() + 
  geom_sf(data = PAs, size = 1, color = "black", fill = "darkgreen") + 
  ggtitle("PAs in Honduras")

#let's explore the different names of PAs in Honduras
names(PAs)
PAs$NOMBRE

#let's say we want to extract only those PAs that are National Parks (Parque Nacional)
library(dplyr)
PAs$CATEGORIA

NationalParks <- PAs %>% 
  filter(CATEGORIA == "Parque Nacional")

#how many PAs are NPs?
nrow(NationalParks)

#what if there is a numerical condition?
PAs$HECTARES

BigPAs <- PAs %>% 
  filter(HECTARES > 50000)
#how many PAs are greater than 5000 ha in area?
nrow(BigPAs)

#We can also adjust our colors to give each PA a different color
ggplot() + 
  geom_sf(data = BigPAs, aes(color = factor(NOMBRE)), size = 1.5) +
  labs(color = 'NOMBRE') +
  ggtitle("Large PAs in Honduras", subtitle = "Subtitle option if you want it!")

#now let's add an outline of honduras, shall we?
#fun little preview of using online data to get boundaries of countries (can do US states too!)
library(rnaturalearth)
countries <- ne_download(scale = "large", type = 'countries', returnclass="sf" )
names(countries)
honduras <- countries %>% filter(NAME == "Honduras")
ggplot() + 
  geom_sf(data = BigPAs, aes(color = factor(NOMBRE)), size = 1.5) +
  geom_sf(data = honduras, fill=NA, size = 1) +
  labs(color = 'NOMBRE') +
  ggtitle("Large PAs in Honduras", subtitle = "Subtitle option if you want it!")

#what if we are interested in selecting only those large PAs that intersect Honduras roads?
#read in the shapefile with st_read
honduras_roads <- st_read("G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Honduras/Honduras_Roads_1999_CCAD.shp")
#let's see what happens if we try to intersect them
PAs_road_isect <- PAs[honduras_roads,]

#oh man! coordinate systems aren't the same. we need to project the roads to the same coord system
honduras_roads_UTM <- st_transform(honduras_roads, crs = "+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
PAs_road_isect <- PAs[honduras_roads_UTM,]

#let's see what we get
ggplot() + 
  geom_sf(data = PAs_road_isect, colour= "darkgreen",size = 1.5) +
  geom_sf(data = honduras_roads_UTM, lwd = 1) +
  ggtitle("PAs that intersect roads in Honduras", subtitle = "Subtitle option if you want it!")
