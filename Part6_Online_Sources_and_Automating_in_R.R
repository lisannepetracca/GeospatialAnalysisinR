# ---- PART 4: ONLINE DATA SOURCES AND AUTOMATING ----

#set your working directory
setwd("C:/PASTE YOUR WORKING DIRECTORY HERE")

#get the working directory and save as an object wd
wd<-getwd()

#install.packages(c("terra","tidyterra","ggplot2", "viridis","ggspatial","rnaturalearth","rnaturalearthdata"))

library(terra)
library(tidyterra)
library(ggplot2)
library(viridis)
library(ggspatial)
library(rnaturalearth)
library(rgbif)
library(ggmap)
library(move2)
library(adehabitatHR)
library(FedData)
library(raster)

###Fun with loops

#make a simple loop printing every instance (i) in 1:10
for (i in 1:10){
  print(i)
}

#make loop that reads through a vector, prints 'yay!' if the instance = b
x<-c("a","b","c","b","d")
for (i in x){
  if(i=="b"){
    print("Yay!")
  }
}

#make loop that reads through a vector and
#prints 'yay!' if the instance = b and ':(' if the instance does not =b
for (i in x){
  if(i=="b"){
    print("Yay!") 
  }else {print(":(")}
}

#Troubleshooting loops
#give the loop a starting point
i=1
#for (i in x){ #remove the for statment

if(i=="b"){ #run individual lines of code to check for errors
  print("Yay!") 
}else {print(":(")}
#}

##NOTE: if loop breaks after starts running--> run i to check iteration that
#caused issue, run individual lines at that instance to find error
#can use next to skip iteration based on condition (like if error)



################################################################
################################################################
##Downloading shapefiles from URL

#cool! I've just gotten a project to survey the SMAMMALS at TX WMAS

#lets get an idea of where those are

#here we are going to grab elevation and the boundaries of Texas Wildlife Management Areas  
#we are going to do this using loops to practice

#because in zip files need to create pathways for each zip file to be downloaded to
tx<-("/TX_WMAs") #Texas WMA boundaries
ele<-("/ele") #ele stored in new folder ele
rds<-("/roads")
root<- c(tx,ele,rds) #going to bind folder pathways into vector to reference later

#get URLS from internet for zip files
files<-c(
  "https://tpwd.texas.gov/gis/resources/wildlife-management-areas.zip",
  "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Grid_ZipFiles/mn30_grd.zip",
  "https://www2.census.gov/geo/tiger/TIGER2019/PRISECROADS/tl_2019_48_prisecroads.zip")


#write a loop to batch download URLS
#this might take a few minutes to run
for (i in 1:length(files)){ #iterate through each instance of files (aka run through 1:2 here)
  #note I prefer length(files) as opposed to 1:2, because I can add or delete files to root and files and this still works
  download.file(files[i],paste(wd,root[i],sep="")) #download file from that instance (i) usin download.file() function
  #of files into the working directory with the corresponding root
}

#look in detail at loop
i=1 #set iteration 
files[i] #check files at that iteration
paste(wd,root[i],sep="") #see where we are storing it

#unzip each file
for (i in 1:length(files)){ #for each instance in files (1:2 in this case)
  unzip(paste(wd, root[i], sep=""))} #unzip the folder corresponding to wd + particular root 

#read in TX WMAs and roads shapefiles & elevation raster
TX_WMA<-vect("WildlifeManagementAreas/WildlifeManagementAreas.shp")#this one is nested in another folder
roads<-vect("tl_2019_48_prisecroads.shp")
#Plot NY shapefile and view
ele<-rast("mn30_grd")#DONT PLOT,load raster file-this is for the entire US, LARGE FILE!!

#since elevation is a big file what if we just cropped it to the extent of our
#WMA layer
cropped.ele<- crop(ele,(TX_WMA))
plot(cropped.ele)
plot(TX_WMA,add=T) 
plot(roads,add=T,col="red")

#let's see how many WMAs I need to survey
length(TX_WMA$LoName)

#I need to provide maps of all the study areas to give to my techs
# or was it for permitting? either way
#wow that's a lot of study site maps to make!

#lets start I guess...
#starting with the map from cartography

#what if we want it more earth toned
#And the last thing wee need is a north arrow and a scale bar to make our map official 

#lets get the extent for the first WMA
sub<-TX_WMA[TX_WMA$LoName==unique(TX_WMA$LoName[[1]]),]
x.min<-xmin(sub)
y.min<-ymin(sub)
x.max<-xmax(sub)
y.max<-ymax(sub)

#total extent
bbox <- c(left = x.min-0.015, bottom = y.min-0.015, right = x.max+0.015, top = y.max+0.015)

#we will use ggmap which has decent basemaps but we need to convert to a data frame w/ coordinates
coords<-data.frame(crds(sub))
geo_dat_coords<-cbind(data.frame(sub),coords)

#get out basemap using get_stamenmap -lots of options here!
map<-get_stamenmap(bbox,maptype = "terrain",zoom=13)



#and map it!
ggmap(map) +
  geom_polygon(data = geo_dat_coords,aes(x=x,y=y), color = "black", fill = "darkseagreen4",alpha=0.5, lwd=1) +
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.text.x = element_text(angle = -25))+
  coord_sf()

#lets create a folder for it
dir.create(paste0(getwd(),"/SiteMaps"))

#And save it!
pdf("SiteMaps/SiteMap1.pdf",height=5,width=5)
ggmap(map) +
  geom_polygon(data = geo_dat_coords,aes(x=x,y=y), color = "black", fill = "darkseagreen4",alpha=0.5, lwd=1) +
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.text.x = element_text(angle = -25))+
  coord_sf()
dev.off()


#Only 83 more to go!
#jsut kidding lets loop it

for (i in 1:length(unique(TX_WMA$LoName))){
  #lets set the extent for the i th  WMA
  sub<-TX_WMA[TX_WMA$LoName==unique(TX_WMA$LoName)[[i]],]
  x.min<-xmin(sub)
  y.min<-ymin(sub)
  x.max<-xmax(sub)
  y.max<-ymax(sub)
  
  #total extent
  bbox <- c(left = x.min-0.015, bottom = y.min-0.015, right = x.max+0.015, top = y.max+0.015)
  
  #we will use ggmap which has decent basemaps but we need to convert to a data frame w/ coordinates
  coords<-data.frame(crds(sub))
  geo_dat_coords<-cbind(data.frame(sub),coords)
  
  #get out basemap using get_stamenmap -lots of options here!
  map<-get_stamenmap(bbox,maptype = "terrain",zoom=13)
  
  site.map<- ggmap(map) +
    geom_polygon(data = geo_dat_coords,aes(x=x,y=y), color = "black", fill = "darkseagreen4",alpha=0.5, lwd=1) +
    theme_bw()+
    xlab("Longitude")+
    ylab("Latitude")+
    theme(axis.text.x = element_text(angle = -25))+
    coord_sf()
  #and now make and save a new map, make sure to save based on i to not overwrite
  pdf(paste0("SiteMaps/",unique(TX_WMA$LoName)[i],".pdf"),height=5,width=5)
  plot(site.map)
  dev.off()
  if (i==7)break## add this because we don't really need to go thru and map all 84
}


###########################################################
######################GBIF Exercise ###Map species

#establish search criteria - searching for family Canidae
#many search criteria available check out the rgbif guide
key <- name_backbone(name = 'Canidae', rank='family')$usageKey

#run search and download 2000 records with coordinates -->
###Download takes a moment so skip to line 124 if uploading csv

Canidae<-occ_search(taxonKey=key,limit=2000,hasCoordinate = TRUE)#this takes a bit of time

#inspect Canidae -returns output summary
Canidae

#inspect slots in Canidae - we want data
names(Canidae)

#save data as csv in working directory
write.csv(Canidae$data,"Canidae_occ.csv")

#data is in tibble which is a modified data frame- lets change it to data frame to be consistent and store it in candat
can<-data.frame(Canidae$data)

#can<-read.csv("Example_Canidae/Canidae_occ.csv") #OR JUST READ IN CSV

#look at data
summary(can)
names(can)
str(can)
#gbif data has too many columns, we want:
#lat=decimalLatitude
#long=decimalLongitude
#species=species

#convert data frame to simple feature
candat<-vect(can, geom = c("decimalLongitude" ,"decimalLatitude"), crs = ("EPSG:4326"))

#lets look at the data colored by species
plot(candat,col=as.factor(candat$species),pch=16)

#Well that is not a great looking map, lets make better ones using ggplot 

#load global country boundaries shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

#Or read in directly (L91) and run line 92
#world<-vect("Example_Canidae/world.shp")
#world$name<-world$CNTRY_NAME

##plot the world
ggplot(data = world)+
  #plot continents
  geom_spatvector(color = "black", fill = "antiquewhite", lwd=0.5) +
  #add scale
  annotation_scale(
    pad_x = unit(0, "cm"),
    pad_y = unit(0.05, "cm"))+
  #add North arrow
  annotation_north_arrow(
    style = north_arrow_fancy_orienteering,
    height=unit(1.5, "cm"),
    width=unit(1.5, "cm"),
    pad_x = unit(0.25, "cm"),
    pad_y = unit(1.7, "cm"))+
  #add grid lines
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = 'dashed', linewidth = 0.5),
        panel.background = element_rect(fill = 'aliceblue'))+               
  #add title and axis labels
  ggtitle("Map of the World")+
  xlab("Longitude") +
  ylab("Latitude")+
  #define plotting bounds
  scale_x_continuous(limits = c(-150,150), breaks=(seq(-180,180,50)))+
  scale_y_continuous(limits = c(-65,75), breaks=(seq(-180,180,50)))+
  #ensures everything in matching CRS
  coord_sf()

#plot Canidae Richness
ggplot(data = world)+
  geom_spatvector(color = "black", fill = "antiquewhite", lwd=0.5) +
  #convert points to binned hexagons
  stat_summary_hex(
    data=can,aes(
      x=decimalLongitude,
      y=decimalLatitude,
      z=speciesKey),
    fun=function(z){length(unique(z))},
    binwidth=c(4,4))+
  #color by viridis color scale G
  scale_fill_viridis("Richness",option='G',begin=0.25,end=.85,alpha=0.9)+
  annotation_scale(
    pad_x = unit(0, "cm"),
    pad_y = unit(0.05, "cm"))+
  annotation_north_arrow(
    style = north_arrow_fancy_orienteering,
    height=unit(1.5, "cm"),width=unit(1.5, "cm"),
    pad_x = unit(0.25, "cm"),
    pad_y = unit(1.7, "cm"))+
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = 'dashed', linewidth = 0.5),
        panel.background = element_rect(fill = 'aliceblue'))+               
  ggtitle("Canidae species richness")+
  xlab("Longitude") +
  ylab("Latitude")+
  scale_x_continuous(limits = c(-150,150), breaks=(seq(-180,180,50)))+
  scale_y_continuous(limits = c(-65,75), breaks=(seq(-180,180,50)))+
  coord_sf()
##warning ok just plotting at smaller scale than data



#########For US only and add labels

#make labels
world_points <- centroids(world,inside=T)

#crud what is a MULTIPOLYGON? this is a sf object called MULTIPOLYGON - 
#lets make 'world' a spatvector object and try again
world.sv<-vect(world)

#what is we want to add labels based on country name?
world_points <- centroids(world.sv,inside=T)

#get coordinates
world_points$x<-crds(world_points)[,1]
world_points$y<-crds(world_points)[,2]


ggplot(data = world.sv)+
  geom_spatvector(color = "black", fill = "antiquewhite", lwd=0.5) +
  stat_summary_hex(data=can,aes(x=decimalLongitude,y=decimalLatitude,z=speciesKey),
                   fun=function(z){length(unique(z))},
                   binwidth=c(2,2))+
  scale_fill_viridis("Richness",option='G',begin=0.25,end=.85,alpha=0.75)+
  annotation_scale(
    pad_x = unit(0, "cm"),
    pad_y = unit(0.05, "cm"))+
  annotation_north_arrow(
    style = north_arrow_fancy_orienteering,
    height=unit(1.5, "cm"),width=unit(1.5, "cm"),
    pad_x = unit(0.25, "cm"),
    pad_y = unit(1.7, "cm"))+
  theme(panel.grid.major =
          element_line(color = gray(.5), 
                       linetype = 'dashed', linewidth = 0.5),
        panel.background = element_rect(fill = 'aliceblue'))+
  #add text for labels
  geom_text(data= world_points,aes(x=x,y=y, label=name), 
            color = "gray20", size=4,
            fontface = "italic", check_overlap = TRUE) +
  ggtitle("Canidae species richness")+
  xlab("Longitude") +
  ylab("Latitude")+
  # Change limits
  scale_x_continuous(limits = c(-130,-55), breaks=(seq(-180,180,25)))+
  scale_y_continuous(limits = c(20,50), breaks=(seq(-180,180,10)))+
  coord_sf()
##warning ok just plotting at smaller scale than data


####map locations by species

for (i in 1:length(unique(can$species))) {  #running through 1: number of species
  sub<-candat[candat$species==unique(candat$species)[i],] #for species i, subset candat to only that species's data
  map<-  ggplot()+
    geom_sf(data = world, color = "black", fill = "white", size=0.5) +
    geom_sf(data=sub, color = i,alpha=0.5)+
    ggtitle(paste((sub$species[1])))+
    coord_sf()
  plot(map) # plot the map in the R environment
  
  pdf(paste(unique(sub$species)[1],Sys.Date(),".pdf",sep=""),height=6,width=6) #print a pdf to your working directory of species i, 
  #title it by species i and the date
  plot(map) #you are plotting the map in the pdf
  dev.off() #closing the graphics device for the pdf
  if (i==5) break
} #close the loop



#######################################################################################
###########################  MOVEBANK #################################################

#UNLESS YOU HAVE A MOVEBANK USERNAME AND PASSWORD SKIP TO LINE 408!!!!!

#Neat, okay lets try GPS movement data from movebank, by accessing the movebank API 
#through the move2 package. We will grab fisher data from NY. We can use the data
#study ID to direclty download open access data using the movebank_download_study() function:
movebank_store_credentials(username="" ,password ="")
s<-	movebank_download_study(6925808)

#you will get a note that you need to approve the license and copy 
#'license-md5'='0cc38362d9a726efbc0347a0d8e65fcf' #into the previous line- 
#'let's do that
s<-	movebank_download_study(6925808,'license-md5'='0cc38362d9a726efbc0347a0d8e65fcf')

#this should take less that 20 or so seconds with a fast connection

#lets convert it data frame and add coordinates using the sf package (compatable with the move package)
fisher<-data.frame(s,sf::st_coordinates(s))


#save to csv
write.csv(fisher,"fisherdata.csv")

#read in CSV instead of downloading; read next lines
#fisher<-read.csv("Part 6 Data/fisherdata.csv")#'Martes pennanti LaPoint New York_121719/Martes pennanti LaPoint New York.csv')
fisher<-vect(fisher, c("X", "Y"), crs = "+proj=longlat +datum=WGS84 +no_defs") #if loading data frame

#look at data
head(fisher)

#Oof lots of NA coordinates that going to be problematic later -lets remove NA coordinates
fisher<-na.omit(fisher,geom=T)

#look at how many points per individual
table(fisher$individual_local_identifier)

#look at the fisher data to make sure everything looks right
plot(fisher,col=as.factor(fisher$individual_local_identifier))

#Excellent! Now we can move on to home ranges

#####calculate home ranges for individuals
#we are goiung to use minimum convex polygons to estimate the home range for each individual using the 
#mcp() function in adehabitatHR. This package requires sp classes so we need to transform our data
#from spatVector to sp. This package has some other useful movement statistics - check it out!

#to run home range we need only the individual id and coords in a spatial points class
fisher.drop<-as((fisher[,"individual_local_identifier"]),"Spatial")

#lets create a separate file for each individual
#initalize vector to store names of home range shapefiles
l<-rep(NA,length(unique(fisher.drop$individual_local_identifier)))

#loop through all unique individuals, calculate 95% Minimum convex polygon, rename shapefile by individual, save as shapefile,
#plot the MCP and add the name of the shapefile to l
for (i in 1:length(unique(fisher.drop$individual_local_identifier))) { #for 1: number of individuals
  sub<-fisher.drop[fisher.drop$individual_local_identifier==unique(fisher.drop$individual_local_identifier)[i],]
  #subset data for that individual
  sub$individual_local_identifier<-droplevels(sub$individual_local_identifier)
  #drop other individul factor levels because it was throwing an error
  mcp<-mcp(sub, percent=95)
  #calculate 95% mcp - for that individual store in object mcp
  assign(paste("MCP.",unique(fisher.drop$individual_local_identifier)[i],sep=""),mcp)
  #rename mcp object as MCP.[individual id] using assign() - I'm a fan of this function
  vect(get(paste("MCP.",unique(fisher.drop$individual_local_identifier)[i],sep="")),
       paste("95p_MCP_",unique(fisher.drop$individual_local_identifier)[i],".shp",sep=""),overwrite=TRUE)
  #write a shapefile for the newly assigned MCP.[individual id] object, name it 95p_MCP_[individual Id]
  #the get() function takes the text and gets the r object with that name
  print(paste("MCP.",unique(fisher.drop$individual_local_identifier)[i],sep=""))
  #print the MCP.[individual Id] to track progress
  l[i]<-c(paste("MCP.",unique(fisher.drop$individual_local_identifier)[i],sep=""))
  #store the MCP.[individual.Id] in vector to access later
  
  #and a simple plot for each to examine
  plot(mcp, col=i)
  plot(sub,add=T,pch=16)
}

#so we did all that but if we just wanted the home ranges all in one shapefile we could do this
fast<-(mcp(fisher.drop, percent=95))

#look at the home ranges
plot(fast)


#####GATHER COVARIATES
#so now we have MCPs we need to get our covariates organized. We are using canopy and elevation
#we already brought in elevation (ele), but its for the country so too big to work with
#we can get canopy from NLCD but NLCD is also too big to work with rapidly at the country level

#get landscape data - Tree canopy cover from NLCD and the FedData package
#this package is awesome because it crops NLCD as it brings it in otherwise the dataset is HUGE
nlcd<-get_nlcd(template =fisher , year = 2016, dataset = "landcover", label = "fisher Land", force.redo = T)

#convert back to terra object
nlcd.terra<-rast(nlcd)
plot(nlcd.terra)

#bring in canopy -can also get from get_NLCD but not working recently
canopy<-rast("Part 6 Data/nlcd_canopy.tif")#Alternitively load raster from file

#everything needs to be in the same crs  -  because NLCD is the largest we will transform everything to 
#the crs of the NLCD layer because this will be faster

#Transform points to raster CRS 
fisher<-project(fisher,crs(canopy))
canopy<-crop(canopy,fisher)

#look at it
plot(canopy)

#look at them on top of NLCD canopy layer
plot(fisher,add=T,pch=16)

#okay now lets crop our elevation layer
crs(canopy)==crs(ele)

#project elevation to nlcd
ele.p<-project(ele,canopy)
ele.p
canopy

ele.p<-crop(ele.p,canopy)
plot(ele.p)

####Transform home ranges CRS and plot in a loop
#remember what l is
l
for (i in 1 :length(l)){ #for every instance in 1:number of home ranges stored in l
  out<-vect(get(l[i])) #get the object l[i] and convert to SpatVector class 
  #because we will need it in this class to sample random points in a minute
  out<-project(out,crs(canopy))#transform it to the crs of nlcd
  assign(paste(l[i],sep=""),out) #assign it the same name it had MCP.[individual Id]
  plot(out,add=T,col=i)#plot it on the NLCD canopy layer that is open
}



############### BASIC RESOURSE SELECTION FUCTION (RSF)  EXERCISE  ##############

#okay we have our spatial data squared away- now we need to prep our data for RSF - 
# we have used points, these are our GPS data-lets create a dummy variable 'used' and input a 1 
#for these data
#add new column to fisher to name used points with 1
fisher$used<-1

#plot the used points
plot(fisher,col=(fisher$used)+1,pch=16,cex=0.5)

#look at fisher
names(fisher)

#subset fisher to just used and individual columns, call it fish
fish<-fisher[,c("used","individual_local_identifier")]
fish

#Okay now we need to compare used to what was available to each individual in their home range
#loop through each home range and sample as many points as there are points for that individual, 
#add a column used with the label 0, bind to the used GPS points
for (i in 1:length(l)){#for 1: number of home ranges
  out <- spatSample(get(l[i]), nrow(fisher[fisher$individual_local_identifier==
                                             unique(fisher.drop$individual_local_identifier)[i],]), method="random")
  #sample X random points from home range i, where X=the number of used points for that individual
  out$used<-0
  #add a column to out that has tthe dummy variable used, but used =0 (i.e., unused)
  fish<-rbind(fish,out)
  #bind the new available points to the used data and all previously sampled available points
}

#plot the used and available points
plot(fish,col=(fish$used)+1,pch=16,cex=0.5)

#The next step is to extract covariate values to the points

#extract raster data to points
fish$LC<-extract(canopy,fish,ID=F,method="bilinear")
fish$ele<-extract(ele.p,fish,ID=F,method="bilinear")

model<-glm(used~-1+scale(LC)+scale(as.numeric(ele)), data=fish,family='binomial') #takes a few moments to run 
#look at our model summary
summary(model)


##Okay thats super cool fishers like canopy cover and avoid higher elevations here but what does that
#look like on the landscape??? To look at suitability across a landscape we need to plug in the  
#values of each pixel on the landscape to our model and see the suitability - we can do this using 
#the predict() function if we have those landscape values

#so lets get values for our covariates across the landscape
#create grid of points across landscape
samp<-spatSample (ext(fisher), size=100000, method="regular",lonlat=F)
plot(samp)

pred.temp<-vect(samp)

#extract values
pred.temp$LC<-extract(canopy,pred.temp,ID=F,method="bilinear")
pred.temp$ele<-extract(ele.p,pred.temp,ID=F,method="bilinear")

pred.temp<-na.omit(pred.temp,geom=T)

#Create newdata from these files
newdata<-data.frame(pred.temp)

#create predictions across surface
pred.temp$pred<-as.numeric(predict(model,newdata=newdata,type='response'))

#scale predictions so values are between 0 and 1
pred.temp$pred<-(pred.temp$pred-min(pred.temp$pred,na.rm=T))/(max(pred.temp$pred,na.rm=T)-min(pred.temp$pred,na.rm=T))

#NLCD is super fine scale and takes too much memory to process
#need to lower resolution of NLCD raster for processing
#convert our backgound point predictions to a raster 
rsf.preds<-rasterize(pred.temp,aggregate(canopy,fact=6),field="pred")

#plot it!
plot(rsf.preds)

#add the home ranges to the map
for (i in 1:length(l)){
  plot((get(l[i])),lwd=3,add=T)
}


