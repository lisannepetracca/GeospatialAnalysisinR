#let's set our working directory first
setwd("C:\\Users\\lspetrac\\Desktop\\Geospatial_Analysis_in_R")

#and let's load all the libraries we need
library(sf)
library(ggplot2)
library(dplyr)
library(raster)
library(ggmap)

# ---- LET'S HAVE SOME FUN WITH MAPPING! ----

# ---- VECTOR ONLY ----

#we will return to our Zimbabwe data for the vector mapping
#first, let's read in our shapefile of Hwange NP (polygon)
HwangeNP <- st_read("Example_Zimbabwe\\Hwange_NP.shp")
#then our roads (line)
roads <- st_read("Example_Zimbabwe\\ZWE_roads.shp")
#then our waterholes (point)
waterholes <- st_read("Example_Zimbabwe\\waterholes.shp")

#let's project roads to wGS 1984 UTM Zone 35S to match the others
roads <- st_transform(roads, crs = 32735)

#and now let's clip the roads to Hwange NP
roads_isect <- roads[HwangeNP,]

ggplot() +
  geom_sf(data = HwangeNP, color = "darkgreen", fill = "white", size=2) +
  geom_sf(data=roads_isect, color = "black", size=1)+
  geom_sf(data=waterholes, color= "blue", size=3)+
  ggtitle("Roads and Waterholes in Hwange NP", subtitle = "2020")+
  coord_sf()

#let's add a legend object, with "TYPE" of waterhole in the legend
#how can we see unique values of "waterhole type?
unique(waterholes$TYPE)

#we are making two small changes here
#the "aes" argument tells ggplot to apply a different color to each value of waterhole TYPE
#"labs" in this case gives a title to the legend
ggplot() +
  geom_sf(data = HwangeNP, color = "darkgreen", fill = "white", size=2) +
  geom_sf(data=waterholes, aes(color=factor(TYPE)), size=3)+
  labs(color = 'Waterhole type')+
  ggtitle("Waterhole types in Hwange NP", subtitle = "2020")+
  coord_sf()

#what if we don't like these colors? how can we change them?
#can see color options here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
waterhole_colors <- c("purple", "orange", "deepskyblue3")

#now we basically need to tell ggplot to use these colors
ggplot() +
  geom_sf(data = HwangeNP, color = "darkgreen", fill = "white", size=2) +
  geom_sf(data=roads_isect, color = "black", size=1)+
  geom_sf(data=waterholes, aes(color=factor(TYPE)), size=3)+
  scale_color_manual(values=waterhole_colors)+
  labs(color = 'Waterhole type')+
  ggtitle("Waterhole types in Hwange NP", subtitle = "2020")+
  coord_sf()

#we can change other aspects of the legend using theme()
ggplot() +
  geom_sf(data = HwangeNP, color = "darkgreen", fill = "white", size=2) +
  geom_sf(data=roads_isect, color = "black", size=1)+
  geom_sf(data=waterholes, aes(color=factor(TYPE)), size=3)+
  scale_color_manual(values=waterhole_colors)+
  labs(color = 'Waterhole type')+
  ggtitle("Waterhole Types in Hwange NP")+
  theme(plot.title = element_text(size=20), #this changes size of plot title
        legend.position="bottom", #changes legend position
        legend.title=element_text(size=16), #changes size of legend title
        legend.text = element_text(size = 16), #changes size of element text in legend
        legend.box.background = element_rect(size = 1)) + #adds a legend box of width 1
  coord_sf()

#ok, so that's great for plotting a single shapefile
#what if we are interested in plotting multiple shapefiles?

#let's go back to our original map with the polygon, lines, and points

ggplot() +
  geom_sf(data = HwangeNP, color = "darkgreen", fill = "white", size=2) +
  geom_sf(data=roads_isect, color = "black", size=1)+
  geom_sf(data=waterholes, color= "blue", size=3)+
  ggtitle("Roads and Waterholes in Hwange NP", subtitle = "2020")+
  coord_sf()

#and let's say we want to have waterhole type AND roads in the legend
#each vector needs an aes & scale argument
ggplot() +
  geom_sf(data = HwangeNP, color = "darkgreen", fill = "white", size=2) +
  geom_sf(data=roads_isect, aes(fill = F_CODE_DES), size=1)+
  geom_sf(data=waterholes, aes(color=factor(TYPE)), size=3)+
  scale_fill_manual(values = "black", name = "")+
  scale_color_manual(values = waterhole_colors, name = "Waterhole type") +
  ggtitle("Roads and Waterholes in Hwange NP", subtitle = "2020")+
  coord_sf()

#Let's make the waterholes diamonds instead of circles
#see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html for a lot of ggplot aesthetics
#here, shape=18 corresponds to a diamond
#got rid of title here bc titles are unusual in publications
ggplot() +
  geom_sf(data = HwangeNP, color = "darkgreen", fill = "white", size=2) +
  geom_sf(data=roads_isect, aes(fill = F_CODE_DES), size=1)+
  geom_sf(data=waterholes, aes(color=factor(TYPE)), size=4, shape=18)+
  scale_fill_manual(values = "black", name = "")+
  scale_color_manual(values = waterhole_colors, name = "Waterhole type") +
  coord_sf()

#And, for a final foray with vector mapping, let's explore different basemaps
#be sure that you have a 64-bit version of java installed if you are using 64-bit R
#first, you need to register for an API key here:  https://cloud.google.com/maps-platform/
#then you need to type in your key in order to enable Google's map services
register_google(key = "TYPE KEY HERE", write = TRUE)

#once you have registered, you should save your key to a .csv in an easily-accessible place
#use a single column named "Key" and paste the key underneath
key<-read.csv("C:\\Users\\lspetrac\\Dropbox\\google_api.csv")
register_google(key = key$Key)

#let's get the bounding box of Zimbabwe
#country boundaries from https://gadm.org/
#yet another means of getting country boundaries
Zimbabwe <- getData("GADM",country="Zimbabwe",level=0)
#convert to sf object
Zimbabwe_sf <- st_as_sf(Zimbabwe)
#plot it
plot(Zimbabwe_sf[1])
#get the extent
extent <- st_bbox(Zimbabwe_sf)

#let's do "watercolor" and "terrain" themes from "stamen" source

#we need to provide the coordinates for the extent, as well as the zoom
#lower numbers means coarser zoom; can go from 3-21
#these lines will get the basemap tiles
stamen_watercolor <- get_stamenmap(bbox = c(extent[[1]], extent[[2]], extent[[3]], extent[[4]]),
                                   maptype = "watercolor", 
                                   crop = T, zoom=8)
stamen_terrain <- get_stamenmap(bbox = c(extent[[1]], extent[[2]], extent[[3]], extent[[4]]),
                                maptype = "terrain", 
                                crop = T, zoom=7)
#now we will extract files from google "satellite" and "hybrid" basemaps
#getting google tiles is a bit different because they work on a center coordinate
google_satellite <- get_googlemap(center = c(29.5, -19),
                                  maptype = "satellite", 
                                  crop = F, zoom=6)
google_hybrid <- get_googlemap(center = c(29.5, -19),
                               maptype = "hybrid", 
                               crop = F, zoom=6)

ggmap(stamen_watercolor) +
  geom_sf(data = Zimbabwe_sf, color = "black", fill = NA, size=2, inherit.aes = FALSE) +
  geom_sf(data = HwangeNP, color = "darkgreen", fill = NA, size=1, inherit.aes = FALSE)+
  ggtitle("Source = 'stamen', type= 'watercolor'")
ggmap(stamen_terrain) +
  geom_sf(data = Zimbabwe_sf, color = "black", fill = NA, size=2, inherit.aes = FALSE) +
  geom_sf(data = HwangeNP, color = "darkgreen", fill = NA, size=1, inherit.aes = FALSE) +
  ggtitle("Source = 'stamen', type= 'terrain'")
ggmap(google_satellite) +
  geom_sf(data = Zimbabwe_sf, color = "black", fill = NA, size=2, inherit.aes = FALSE) +
  geom_sf(data = HwangeNP, color = "white", fill = NA, size=1, inherit.aes = FALSE) +
  coord_sf(xlim=c(25,34), ylim=c(-23,-15))+
  ggtitle("Source = 'google', type= 'satellite'")
ggmap(google_hybrid) +
  geom_sf(data = Zimbabwe_sf, color = "black", fill = NA, size=2, inherit.aes = FALSE) +
  geom_sf(data = HwangeNP, color = "white", fill = NA, size=1, inherit.aes = FALSE) +
  coord_sf(xlim=c(25,34), ylim=c(-23,-15))+
  ggtitle("Source = 'google', type= 'hybrid'")

#### ---- INCLUDING RASTER DATA ----####

#let's return to plotting elevation in Hwange NP
#let's read in that cropped elevation file we already made

elev <- raster("Example_Zimbabwe\\elev_Hwange.tif")

#and let's see it quickly using the plot function in raster
plot(elev)

#to plot a raster in ggplot, remember that we need to convert it to a data frame first
elev_df <- as.data.frame(elev, xy=TRUE)
#see what the data frame looks like
head(elev_df)

#now we can get started
#first: what if we wanted to plot elevation in four classes?
#on the one hand, we can have dplyr determine those breaks 
#give the column name of the raster values to cut (in our case, "elev_Hwange")
#will distribute the values of the raster into 4 bins

elev_df_fourgroups <- elev_df %>%
  mutate(elev_brk = cut(elev_Hwange, breaks = 4))

#you can then see how many pixels fall into each group
elev_df_fourgroups %>%
  group_by(elev_brk) %>%
  count()

#man, we have NA values. where are they? 
#we can use "na.value = "color"" to show where those pixels are
ggplot() +
  geom_raster(data = elev_df_fourgroups , aes(x = x, y = y, fill = elev_Hwange)) +
  scale_fill_viridis_c(na.value = 'red') 

#ok, they are some border cells
#can use trim() argument in raster package to get rid of these cells, but we're ok for now
#trim gets rid of NAs in the outer rows and columns 

#one trick of getting around NAs is removing those rows where the raster value is NA
#otherwise NA will show up in the legend & this is annoying
elev_df_fourgroups_noNA <- elev_df_fourgroups[!is.na(elev_df_fourgroups$elev_Hwange), ]

ggplot() +
  geom_raster(data = elev_df_fourgroups_noNA, aes(x = x, y = y, fill = elev_brk)) 

#what if we want these breaks to be manual?
#you do need to start the breaks at the lowest value you want represented (rather than
#where you want the breaks to be, as in ArcGIS)

#let's get rid of NA values in main elev_df
elev_df <- elev_df[!is.na(elev_df$elev_Hwange), ]

#now we can set our breaks
breaks <- c(800, 900, 1000, 1100, 1200)

elev_df_manualbrk <- elev_df %>%
  mutate(elev_brk_manual = cut(elev_Hwange, breaks = breaks))

#how many pixels fall into each group?
#you can then see how many pixels fall into each group
elev_df_manualbrk %>%
  group_by(elev_brk_manual) %>%
  count()

#now let's plot these four classes
ggplot() +
  geom_raster(data = elev_df_manualbrk , aes(x = x, y = y, fill = elev_brk_manual)) 

#now let's plot these four classes with R's terrain.colors palette
ggplot() +
  geom_raster(data = elev_df_manualbrk , aes(x = x, y = y, fill = elev_brk_manual)) +
  scale_fill_manual(values = terrain.colors(4))

#now let's move onto aesthetics & customization
#we'll begin by changing the legend title
#then changing how the elevation range is presented
#and then removing x and y axis labels

ggplot() +
  geom_raster(data = elev_df_manualbrk , aes(x = x, y = y, fill = elev_brk_manual)) +
  scale_fill_manual(values = terrain.colors(4), name = "Elevation (m)",
                    labels=c("800 - 900", "900 - 1000", 
                             "1000 - 1100", "1100 - 1200"))+
  theme(axis.title = element_blank()) 

#cool. how would things look if this were a continuous surface?
ggplot() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill=elev_Hwange)) +
  scale_fill_viridis_c() +
  theme(axis.title = element_blank())

#and how would we change some aesthetics?
#let's change legend name
#move legend theme to bottom
#adjust size of legend name and labels
ggplot() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill=elev_Hwange)) +
  scale_fill_viridis_c(name = "Elevation (m)") +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text = element_text(size = 10), 
        legend.box.background = element_rect(size = 1))

#let's place the elevation raster in a map with Hwange NP and waterholes
#order matters!
#layers that should be on the bottom go first
#notice that the fill for Hwange is now "NA" so we can see underlying elevation
ggplot() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill=elev_Hwange)) +
  geom_sf(data = HwangeNP, color = "black", fill = NA, size=2) +
  geom_sf(data=waterholes, aes(color=factor(TYPE)), size=3)+
  scale_fill_viridis_c(name = "Elevation (m)")+
  scale_color_manual(values = waterhole_colors, name = "Waterhole type") +
  theme(axis.title = element_blank())+
  coord_sf()


########### MAKING MULTIPLOTS


sample_vp_1 <- viewport(x = 0, y = 0, 
                        width =0.5, height = 1,
                        just = c("left", "bottom"))
sample_vp_2 <- viewport(x = 0.5, y = 0, 
                        width = 0.5, height = 1,
                        just = c("left", "bottom"))
sample_vp_3 <- viewport(x = 0, y = .8, 
                        width = 1, height = .1,
                        just = c("left", "bottom"))

dev.off()
pushViewport(sample_vp_1)
grid.draw(rectGrob(gp = gpar(col = "green",lwd=4)))
popViewport(1)

pushViewport(sample_vp_2)
grid.draw(rectGrob(gp = gpar(col = "blue",lwd=4)))
popViewport(1)

pushViewport(sample_vp_3)
grid.draw(rectGrob(gp = gpar(col = "black",lwd=4)))
popViewport(1)


map<-ggmap(stamen_watercolor) +
  geom_sf(data = Zimbabwe_sf, color = "black", fill = NA, size=2, inherit.aes = FALSE) +
  geom_sf(data = HwangeNP, color = "darkgreen", fill = NA, size=1, inherit.aes = FALSE)
map2<-ggmap(google_satellite) +
  geom_sf(data = Zimbabwe_sf, color = "black", fill = NA, size=2, inherit.aes = FALSE) +
  geom_sf(data = HwangeNP, color = "white", fill = NA, size=1, inherit.aes = FALSE) +
  coord_sf(xlim=c(25,34), ylim=c(-23,-15))


dev.off()
pushViewport(sample_vp_1)
grid.draw(ggplotGrob(map))
popViewport(1)


pushViewport(sample_vp_2)
grid.draw(ggplotGrob(map2))
popViewport(1)

pushViewport(sample_vp_3)
grid.text("Just looking at some maps")
popViewport(1)



w<-ggplot() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill=elev_Hwange)) +
  geom_sf(data = HwangeNP, color = "black", fill = NA, size=2) +
  geom_sf(data=waterholes, aes(color=factor(TYPE)), size=3)+
  scale_fill_viridis_c(name = "Elevation (m)")+
  scale_color_manual(values = waterhole_colors, name = "Waterhole type") +
  theme(axis.title = element_blank())+
  coord_sf()

dev.off()
grid.draw(ggplotGrob(w))
md_inset <- viewport(x = 0, y = 0, 
                     just = c("left", "bottom"),
                     width = 0.3, height = 0.32)
pushViewport(md_inset)
grid.draw(rectGrob(gp = gpar(alpha = 0.5, col = "white")))
grid.draw(ggplotGrob(map2))
grid.draw(rectGrob(gp = gpar(fill = NA, size=2,col = "black")))
popViewport()
