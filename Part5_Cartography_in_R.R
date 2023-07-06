#let's set our working directory first
setwd("C:/Users/lspetrac/Desktop/Geospatial_Analysis_in_R")

#and let's load all the libraries we need
library(terra)
library(ggplot2)
library(viridis)
library(tidyterra)
library(ggspatial) #scale bars and north arrows
library(grid) #viewports
library(cowplot) #for insets





# ---- LET'S HAVE SOME FUN WITH MAPPING! ----

# ---- VECTOR ONLY ----

#we will return to our Zimbabwe data for the vector mapping
#first, let's read in our shapefile of Hwange NP (polygon)
HwangeNP <- vect("Example_Zimbabwe/Hwange_NP.shp")
#then our roads (line)
roads <- vect("Example_Zimbabwe/ZWE_roads.shp")
#then our waterholes (point)
waterholes <- vect("Example_Zimbabwe/waterholes.shp")

#do the coordinate systems match? let's see
crs(HwangeNP, describe=T)
crs(roads, describe=T)
crs(waterholes, describe=T)

#roads do not match. let's project roads to WGS 1984 UTM Zone 35S to match the others
roads <- project(roads, "EPSG:32735")

#let's make sure it worked
crs(roads, describe=T)

#and now let's select the roads that intersect Hwange NP
roads_Hwange <- roads[HwangeNP,]

#and now let's plot what we have
ggplot() +
  geom_spatvector(data = HwangeNP, color = "darkgreen", fill = "white", lwd=2) +
  geom_spatvector(data=roads_Hwange, color = "black", lwd=1)+
  geom_spatvector(data=waterholes, color= "blue", lwd=3)+
  ggtitle("Roads and Waterholes in Hwange NP", subtitle = "2020")+
  coord_sf() #ensures all layers use common coordinate system

#let's add a legend object, with "TYPE" of waterhole in the legend
#how can we see unique values of "waterhole type?
unique(waterholes$TYPE)

#we are making two small changes here
#the "aes" argument tells ggplot to apply a different color to each value of waterhole TYPE
#"labs" in this case gives a title to the legend
ggplot() +
  geom_spatvector(data = HwangeNP, color = "darkgreen", fill = "white", lwd=2) +
  geom_spatvector(data=roads_Hwange, color = "black", lwd=1)+
  geom_spatvector(data=waterholes, aes(color=factor(TYPE)), lwd=3)+
  #in this case, "labs" gives a title to the legend
  labs(color = 'Waterhole type')+
  ggtitle("Waterhole types in Hwange NP", subtitle = "2023")+
  coord_sf()
  

#what if we don't like these colors? how can we change them?
#can see color options here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
waterhole_colors <- c("purple", "orange", "deepskyblue3")

#now we basically need to tell ggplot to use these colors with "scale_color_manual"
ggplot() +
  geom_spatvector(data = HwangeNP, color = "darkgreen", fill = "white", lwd=2) +
  geom_spatvector(data=roads_Hwange, color = "black", lwd=1)+
  geom_spatvector(data=waterholes, aes(color=factor(TYPE)), lwd=3)+
  scale_color_manual(values=waterhole_colors)+
  ggtitle("Waterhole types in Hwange NP", subtitle = "2023")+
  coord_sf() 

#we can change other aspects of the legend using theme()
ggplot() +
  geom_spatvector(data = HwangeNP, color = "darkgreen", fill = "white", lwd=2) +
  geom_spatvector(data=roads_Hwange, color = "black", lwd=1)+
  geom_spatvector(data=waterholes, aes(color=factor(TYPE)), lwd=3)+
  scale_color_manual(values=waterhole_colors)+
  labs(color = 'Waterhole type')+
  ggtitle("Waterhole Types in Hwange NP")+
  theme(plot.title = element_text(size=20), #this changes size of plot title
        legend.position="bottom", #changes legend position
        legend.title=element_text(size=16), #changes size of legend title
        legend.text = element_text(size=16), #changes size of element text in legend
        legend.box.background = element_rect(size = 1)) + #adds a legend box of width 1
  coord_sf()

#ok, so that's great for plotting a single shapefile
#what if we are interested in plotting multiple shapefiles?

#let's go back to our original map with the polygon, lines, and points
ggplot() +
  geom_spatvector(data = HwangeNP, color = "darkgreen", fill = "white", lwd=2) +
  geom_spatvector(data=roads_Hwange, color = "black", lwd=1)+
  geom_spatvector(data=waterholes, color= "blue", lwd=3)+
  ggtitle("Roads and Waterholes in Hwange NP", subtitle = "2023")+
  coord_sf()

#and let's say we want to have waterhole type AND roads in the legend
#each vector needs an aes & scale argument
ggplot() +
  geom_spatvector(data = HwangeNP, color = "darkgreen", fill = "white", lwd=2) +
  geom_spatvector(data=roads_Hwange, aes(color = F_CODE_DES), linetype=1, lwd=0.25)+
  geom_spatvector(data=waterholes, aes(fill=factor(TYPE)), size=4, shape=21, stroke=0)+
                  #, show.legend="line")+
  scale_color_manual(values = "black", name = "")+
  guides(shape=25)+
  scale_fill_manual(values = waterhole_colors, name = "Waterhole type") +
  ggtitle("Roads and Waterholes in Hwange NP", subtitle = "2023")+
  coord_sf()
#I'm not really feeling this one because the road displays as a polygon and that isn't correct


#we essentially need to create a legend that combines the roads and waterholes
ggplot()+
  geom_spatvector(data = HwangeNP, color = "darkgreen", fill="white", lwd=2) +
  geom_spatvector(data = roads_Hwange, aes(colour = factor(F_CODE_DES)), size = 1, linetype = "solid", show.legend = 'line') +
  geom_spatvector(data = waterholes,   aes(colour = factor(TYPE)), size = 3) +
  scale_color_manual("Legend", labels = c("road", "pan", "riverpool", "spring"),
                     values = c("Road" = "black", 
                                          "pan" = "purple",
                                          "riverpool" = "orange",
                                          "spring" = "deepskyblue3"))+ 
  theme_bw()+
  theme(legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12,face='bold')) +
  guides(color = guide_legend(override.aes = list(linetype = c(1,0,0,0),#   # 
                                     shape=c(NA,16,16,16))))

#Let's make the waterholes diamonds instead of circles
#see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html for a lot of ggplot aesthetics
#here, shape=18 corresponds to a diamond
#got rid of title here bc titles are unusual in publications
ggplot() +
  geom_spatvector(data = HwangeNP, color = "darkgreen", fill = "white", lwd=2) +
  geom_spatvector(data=roads_Hwange, aes(fill = F_CODE_DES), lwd=1)+
  geom_spatvector(data=waterholes, aes(color=factor(TYPE)), lwd=4, shape=18)+
  scale_fill_manual(values = "black", name = "")+
  scale_color_manual(values = waterhole_colors, name = "Waterhole type") +
  coord_sf()

#And, for a final foray with vector mapping, let's explore different basemaps
#be sure that you have a 64-bit version of java installed if you are using 64-bit R
#first, you need to register for an API key here:  https://cloud.google.com/maps-platform/
#then you need to type in your key below in order to enable Google's map services
#uncomment the below line when you have an API key
#register_google(key = "TYPE KEY HERE", write = TRUE)

#once you have registered, you should save your key to a .csv in an easily-accessible place
#use a single column named "Key" and paste the key underneath
key<-read.csv("C:/Users/lspetrac/Dropbox/google_api.csv")
register_google(key = key$Key)

#let's get the bounding box of Zimbabwe
#country boundaries from https://gadm.org/
#yet another means of getting country boundaries
Zimbabwe <- getData("GADM",country="Zimbabwe",level=0)

#if the above line doesn't work if a server is down, uncomment the below line and read it in
#Zimbabwe <- vect("Example_Zimbabwe/Zimbabwe.shp")

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
  geom_spatvector(data = Zimbabwe_sf, color = "black", fill = NA, lwd=2, inherit.aes = FALSE) +
  geom_spatvector(data = HwangeNP, color = "darkgreen", fill = NA, lwd=1, inherit.aes = FALSE)+
  ggtitle("Source = 'stamen', type= 'watercolor'")  #IGNORE WARNING
ggmap(stamen_terrain) +
  geom_spatvector(data = Zimbabwe_sf, color = "black", fill = NA, lwd=2, inherit.aes = FALSE) +
  geom_spatvector(data = HwangeNP, color = "darkgreen", fill = NA, lwd=1, inherit.aes = FALSE) +
  ggtitle("Source = 'stamen', type= 'terrain'")     #IGNORE WARNING
ggmap(google_satellite) +
  geom_spatvector(data = Zimbabwe_sf, color = "black", fill = NA, lwd=2, inherit.aes = FALSE) +
  geom_spatvector(data = HwangeNP, color = "white", fill = NA, lwd=1, inherit.aes = FALSE) +
  coord_sf(xlim=c(25,34), ylim=c(-23,-15))+
  ggtitle("Source = 'google', type= 'satellite'")   #IGNORE WARNING
ggmap(google_hybrid) +
  geom_spatvector(data = Zimbabwe_sf, color = "black", fill = NA, lwd=2, inherit.aes = FALSE) +
  geom_spatvector(data = HwangeNP, color = "white", fill = NA, lwd=1, inherit.aes = FALSE) +
  coord_sf(xlim=c(25,34), ylim=c(-23,-15))+
  ggtitle("Source = 'google', type= 'hybrid'")      #IGNORE WARNING

#### ---- INCLUDING RASTER DATA ----####

#let's return to plotting elevation in Hwange NP
#let's read in that cropped elevation file we already made

elev <- rast("Example_Zimbabwe/elev_Hwange.tif")

#and let's see it quickly using the plot function
plot(elev)

#let's see what the raster looks like
head(elev)
#ok, so there are NAs

#where are these NA values?
#we can use "na.value = "color"" to show where those pixels are
ggplot() +
  geom_spatraster(data = elev) +
  #brief aside on Viridis - package with nice color templates
  #colorblind-friendly
  #see:https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  scale_fill_viridis_c(na.value = 'red')

#ok, so they are some border cells
#let's get back to mapping

#now we can get started
#first: what if we wanted to plot elevation in four classes?
#we can have dplyr determine those breaks 
#give the column name of the raster values to cut (in our case, "aster_image_20160624")
#will distribute the values of the raster into 4 bins

elev_fourgroups <- elev %>%
  mutate(elev_brk = cut(aster_image_20160624, breaks = 4))

#you can then see how many pixels fall into each group
elev_fourgroups_df <- as.data.frame(elev_fourgroups)

elev_fourgroups_df %>%
  group_by(elev_brk) %>%
  count()

#what if we want these breaks to be manual?
#one way is to use "reclassify" as we learned in Part 4
#now we'll use cut()
#you do need to start the breaks at the lowest value you want represented (rather than
#where you want the breaks to be, as in ArcGIS)

#now we can set our breaks
breaks <- c(800, 900, 1000, 1100, 1200)

#and give those breaks to function in dplyr
elev_manualbrk <- elev %>%
  mutate(elev_brk_manual = cut(aster_image_20160624, breaks = breaks))

#how many pixels fall into each group?
#you can then see how many pixels fall into each group
elev_manualbrk_df <- as.data.frame(elev_manualbrk)

elev_manualbrk_df %>%
  group_by(elev_brk_manual) %>%
  count()

#now let's plot these four classes and do so WITHOUT NAs
ggplot() +
  geom_spatraster(data = elev_manualbrk, aes(fill=elev_brk_manual)) +
  scale_fill_discrete(na.translate=F)

#now let's plot these four classes with R's terrain.colors palette
ggplot() +
  geom_spatraster(data = elev_manualbrk, aes(fill=elev_brk_manual)) +
  scale_fill_manual(values = terrain.colors(4), na.translate=F)

#now let's move onto aesthetics & customization
#we'll begin by changing the legend title
#then change text of legend labels
#and then removing x and y axis labels

ggplot() +
  geom_spatraster(data = elev_manualbrk, aes(fill=elev_brk_manual))+
  scale_fill_manual(values = terrain.colors(4), name = "Elevation (m)",
                    labels=c("800 - 900", "900 - 1000", 
                             "1000 - 1100", "1100 - 1200"), na.translate=F)+
  theme(axis.title = element_blank()) 

#cool. how would things look if this were a continuous surface?
#we are using the viridis color palette for the continuous surface
ggplot() +
  geom_spatraster(data = elev) +
  scale_fill_viridis_c() +
  theme(axis.title = element_blank())

#and how would we change some aesthetics?
#let's change legend name
#move legend theme to bottom
#adjust size of legend name and labels
ggplot() +
  geom_spatraster(data = elev) +
  scale_fill_viridis_c(option="H",name = "Elevation (m)") + #changes legend name
  theme(axis.title = element_blank(), #removes x and y axis titles
        legend.position = "bottom", #moves legend to bottom
        #adjust size of legend name and labels
        legend.title=element_text(size=12), 
        legend.text = element_text(size=10), 
        legend.box.background = element_rect(size=1))

#now let's place the elevation raster in a map with Hwange NP and waterholes
#order matters!
#layers that should be on the bottom go first
#notice that the fill for Hwange is now "NA" so we can see underlying elevation
ggplot() +
  geom_spatraster(data = elev) +
  geom_spatvector(data = HwangeNP, color = "black", fill = NA, lwd=2) +
  geom_spatvector(data=waterholes, aes(color=factor(TYPE)), lwd=3)+
  scale_fill_viridis_c(name = "Elevation (m)")+
  scale_color_manual(values = waterhole_colors, name = "Waterhole type") +
  theme(axis.title = element_blank())+
  coord_sf()
#wowzers! that is one excellent-looking map

#Okay lets make it look a bit nicer by adding lat and long lines
#and making the elevation colors a bit less intense
#and giving ggplot a nicer looking theme to work from

ggplot() +
  geom_spatraster(data = elev) +
  geom_spatvector(data = HwangeNP, color = "black", fill = NA, lwd=2) +
  geom_spatvector(data=waterholes, aes(color=factor(TYPE)), lwd=3)+
  #lets make the background a bit less bright by changing the alpha values
  scale_fill_viridis_c(option='H',name = "Elevation (m)",alpha=0.7)+
  scale_color_manual(values = waterhole_colors, name = "Waterhole type") +
  #ggplot has a number of standard themes - I like theme_bw as a template
  #see https://ggplot2.tidyverse.org/reference/ggtheme.html for full list of themes
  theme_bw()+
  #add grid lines for lat and long 
  theme(
    #lets add grid lines!!
    panel.grid.major =
      element_line(color = gray(.5), 
                   linetype = 'dashed', size = 0.75))+
  coord_sf()

#and the last thing we need is a north arrow and a scale bar to make our map official 
ggplot() +
  geom_spatraster(data = elev) +
  geom_spatvector(data = HwangeNP, color = "black", fill = NA, lwd=2) +
  geom_spatvector(data=waterholes, aes(color=factor(TYPE)), lwd=3)+
  scale_fill_viridis_c(option='H',name = "Elevation (m)",alpha=0.7)+
  scale_color_manual(values = waterhole_colors, name = "Waterhole type") +
  #Let's add the scale bar
  annotation_scale(style="bar", #alternative style is "ticks"
    height=unit(.5,"cm"),
    pad_x = unit(1, "cm"),
    pad_y = unit(0.7, "cm"))+
  #Let's add the north arrow - adjust height and padding for your individual screen
  annotation_north_arrow(
    height=unit(1.5, "cm"),
    width=unit(1.25, "cm"),
    pad_x = unit(9.5, "cm"),
    pad_y = unit(8, "cm"))+
  theme_bw()+
  theme(
    panel.grid.major =
      element_line(color = gray(.5), 
                   linetype = 'dashed', size = 0.75))+
  coord_sf()

########### MAKING MULTIPLOTS

#plotting multiple objects side by side
#here we are using package cowplot, but also see viewports() in package grid

#lets save some of the maps from earlier as objects map and map2
map <- ggplot() +
  geom_spatraster(data = elev) +
  geom_spatvector(data = HwangeNP, color = "black", fill = NA, lwd=2) +
  geom_spatvector(data=waterholes, aes(color=factor(TYPE)), lwd=3)+
  scale_fill_viridis_c(name = "Elevation (m)")+
  scale_color_manual(values = waterhole_colors, name = "Waterhole type") +
  theme_void()

map2 <- ggplot() +
  geom_spatraster(data = elev_manualbrk, aes(fill=elev_brk_manual)) +
  geom_spatvector(data = HwangeNP, color = "black", fill = NA, lwd=2) +
  geom_spatvector(data=waterholes, aes(color=factor(TYPE)), lwd=3)+
  scale_fill_manual(name = "Elevation (m)", values = terrain.colors(4), na.translate=F)+
  scale_color_manual(values = waterhole_colors, name = "Waterhole type") +
  theme_void()

#let's place these together and make sure they're aligned vertically
combined_map <- plot_grid(map, map2, labels = "AUTO", align="v")

#INSET MAPS

#let's add an inset to what we call "map" above
#our inset map will be an outline of Zimbabwe only

Zimbabwe <- vect("Example_Zimbabwe/Zimbabwe.shp")

inset <- ggplot() +
  geom_spatvector(data = Zimbabwe, color = "black", fill = "white", lwd=1) +
  theme_void()+
  #we'll add a border to the inset
  theme(
    panel.border = element_rect(fill = NA, colour = "black"),
    plot.background = element_rect(fill = "grey95")
  )

map_w_inset <- ggdraw() +
  draw_plot(map) +
  draw_plot(inset,
            height = 0.2,
            x = -0.35, #you will have to play with these values a bit to get them right!
            y = 0.06)

#ok. that' cool. but how do we get a box around a smaller part of the study area?
#we basically need to create a spatVector from the extent of the other map!

extent <- ext(elev) #get the extent
box <- vect(extent, crs="EPSG:32735") #and make this extent a spatVector

#now we add a new line to the inset map & redo!
inset <- ggplot() +
  geom_spatvector(data = Zimbabwe, color = "black", fill = "white", lwd=1) +
  geom_spatvector(data = box, color = "red", fill = NA, lwd=1) + #NEW LINE
  theme_void()+
  #we'll add a border to the inset
  theme(
    panel.border = element_rect(fill = NA, colour = "black"),
    plot.background = element_rect(fill = "grey95")
  )

map_w_inset <- ggdraw() +
  draw_plot(map) +
  draw_plot(inset,
            height = 0.2,
            x = -0.35, #you will have to play with these values a bit to get them right!
            y = 0.06)
