# ---- PART 4: ONLINE DATA SOURCES AND AUTOMATING ----

#set your working directory
setwd("C:/PASTE YOUR WORKING DIRECTORY HERE")
setwd("E:/OneDrive - Texas A&M University - Kingsville/Presentations/Geospatial_Analysis_in_R")

#get the working directory and save as an object wd to access later
wd<-getwd()
library(terra)
library(tidyterra)
library(ggplot2)
library(viridis)
library(ggspatial)
library(rgbif)
library(ggmap)
library(move2)
library(adehabitatHR)
library(FedData)
library(raster)
library(mapview)
library(sf)
library(landscapemetrics)
library(tidyverse)
library(prism)
library(rnaturalearth)
library(hexbin)
library(rnaturalearthdata)
library(keyring)

# ---- USING LOOPS ----

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



# ---- DOWNLOADING SHAPEFILES FROM URL ----

#Cool! I've just gotten a project to survey the SMAMMALS at TX WMAS

#lets get an idea of where those are

#here we are going to grab elevation and the boundaries of Texas Wildlife Management Areas  
#we are going to do this using loops to practice

#need to define names for each file to be downloaded to
# Define directory names (relative or absolute paths)
tx <- "TX_WMAs"
rds <- "roads" #texas roads
state <- "state"
root <- c(tx, rds, state) #going to bind folder pathways into vector to reference later

# Create directories if they don't exist
for (dir in root) {
  if (!dir.exists(dir)) dir.create(dir)
}

#get URLS from internet for zip files
files<-c(
  "https://tpwd.texas.gov/gis/resources/wildlife-management-areas.zip",
  "https://www2.census.gov/geo/tiger/TIGER2019/PRISECROADS/tl_2019_48_prisecroads.zip",
  "https://www.depts.ttu.edu/geospatial/center/Data/TxStateLayers/Tx_Boundaries/Tx_Bndry_General_TIGER5m.zip")

#write a loop to batch download URLS
#this might take a few minutes to run
# Download each file into the corresponding folder with a specific filename
for (i in 1:length(files)) { #iterate through each instance of files (aka run through 1:3 here)
  #note I prefer length(files) as opposed to 1:3, because I can add or delete files to root and files and this still works
  zip_path <- file.path(wd, root[i], paste0("data", i, ".zip"))  # Define zip file path
  download.file(files[i], destfile = zip_path, mode = "wb") #download file from that instance (i) using download.file() function of files into the working directory with the corresponding root
  
  # Unzip the file into its folder
  unzip(zip_path, exdir = file.path(wd, root[i])) #unzip the folder corresponding to wd + particular root
}


#look in detail at loop
i=1 #set iteration 
files[i] #check files at that iteration
paste(wd,root[i],sep="") #see where we are storing it

#read in TX WMAs, roads shapefiles, & elevation raster
TX_WMA<-vect("TX_WMAs/WildlifeManagementAreas/WildlifeManagementAreas.shp")#this one is nested in another folder
roads<-vect("roads/tl_2019_48_prisecroads.shp")
state_bound<-vect("state/Tx_Bndry_General_TIGER5m.shp")

#Alternatively, read them in from file
#TX_WMA<-vect("Example_TX/WildlifeManagementAreas/WildlifeManagementAreas.shp")
#roads<-vect("Example_TX/tl_2019_48_prisecroads.shp")

# ---- WORKING WITH PRISM CLIMATE DATA ----

#Now you have all of the roads and WMAs in Texas downloaded
#But hey, it gets hot in Texas! Let's use an online climate database to show temps across the state in June
#We will now read in data from Oregon State's PRISM service (https://prism.oregonstate.edu/)
#PRISM only has data available for the continental U.S.; however, other online databases 
#(such as WorldClim, https://worldclim.org/) have global climate data free to download

prism_set_dl_dir(wd) #Tell PRISM where your working directory is
# Download the climate normals for mean temperature between January and February at 800 m resolution
get_prism_normals("tmean", "800m", mon = 1:6, keepZip = FALSE) #ignore warning
#the term "climate normals" refers to the most recent 30 year average
#this can take a minute or two

#Here, we want to select the June temperature normal
junetemp <- prism_archive_subset(
  "tmean", "monthly normals", mon = 6, resolution = "800m"
)

temprast <- pd_to_file(junetemp)#Here, we export he prism data to our working directory
tmean_rast <- rast(temprast)#We then read the prism file back in as a raster using the terra package

#since our temperature raster is a big file what if we just crop it to the extent of our
#WMA layer

mask.temp<- mask(tmean_rast, state_bound) #IGNORE WARNING
plot(mask.temp, type = "continuous", xlim = c(-110, -90), ylim = c(25, 38))
plot(roads, add=T,col="black")
plot(TX_WMA, add=T, border=NA, col="orange") 

# ---- WORKING WITH NLCD LAND COVER DATA ----

#Now lets move on to the individual WMAs
#let's see how many WMAs I need to survey
length(TX_WMA$LoName)

#I need to provide maps of all the study areas to give to my techs
# or was it for permitting? either way
#wow that's a lot of study site maps to make!

#first let's project the WMAs to the same projection as the NLCD data
TX_WMA <- project(TX_WMA, "EPSG:5070")

#lets get the extent for the first WMA (Cedar Creek WMA - Big Island Uni)
sub<-TX_WMA[TX_WMA$LoName==unique(TX_WMA$LoName[[1]]),]
x.min<-xmin(sub)
y.min<-ymin(sub)
x.max<-xmax(sub)
y.max<-ymax(sub)

#We need to convert to a data frame w/ coordinates
coords<-data.frame(crds(sub))
geo_dat_coords<-cbind(data.frame(sub),coords)

#Excellent, but let's say you want to give your techs an idea of the landscape in each unit
#Here, we will use the FedData package (which we will revisit later) to show landcover types acoss the WMA
nlcd_wma <- get_nlcd(sub, year = 2016, dataset = "landcover", label = "Texas Landcover", force.redo = T)
plot(nlcd_wma)

# #IF FEDDATA PACKAGE DOESN'T WORK, USE THE BELOW LINES
# nlcd <- rast("Example_TX/NLCD_Texas.tiff")
# nlcd_wma <- crop(nlcd, sub)

#And map it!

ggplot() + geom_spatraster(data = nlcd_wma) + 
  geom_polygon(data = geo_dat_coords,aes(x=x,y=y), 
               color = "black", fill = NA, alpha=0.5, lwd=1) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max))

#lets create a folder for it
dir.create(paste0(getwd(),"/SiteMaps"))

#And save it!
pdf("SiteMaps/SiteMap1.pdf",height=5,width=5)
ggplot() + geom_spatraster(data = nlcd_wma) + 
  geom_polygon(data = geo_dat_coords,aes(x=x,y=y), 
               color = "black", fill = NA, alpha=0.5, lwd=1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max))

dev.off()


#Only 83 more to go!
#just kidding lets loop it

for (i in 1:length(unique(TX_WMA$LoName))){
  #lets set the extent for the i th  WMA
  sub<-TX_WMA[TX_WMA$LoName==unique(TX_WMA$LoName)[[i]],]
  x.min<-xmin(sub)
  y.min<-ymin(sub)
  x.max<-xmax(sub)
  y.max<-ymax(sub)
  
  #We need to convert to a data frame w/ coordinates
  coords<-data.frame(crds(sub))
  geo_dat_coords<-cbind(data.frame(sub),coords)
  
  #Excellent, but let's say you want to give your techs an idea of the landcover
  #Here, we will use the FedData package (which we will revisit later) to show landcover types acoss the WMA
  nlcd_wma <- get_nlcd(sub, year = 2016, dataset = "landcover", label = "Texas Landcover", force.redo = T)
  
  #Alternatively, if using NLCD read in from file
  #nlcd_wma <- crop(nlcd, sub)
  
  #And map it!
  site.map <- ggplot() + geom_spatraster(data = nlcd_wma) + 
    geom_polygon(data = geo_dat_coords,aes(x=x,y=y), 
                 color = "black", fill = NA, alpha=0.5, lwd=1) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max))
  
  #and now make and save a new map, make sure to save based on i to not overwrite
  pdf(paste0("SiteMaps/",unique(TX_WMA$LoName)[i],".pdf"),height=5,width=5)
  plot(site.map)
  dev.off()
  if (i==7)break## add this because we don't really need to go thru and map all 84
}



# ---- USING SPECIES OCCURRENCE DATA FROM GBIF ----


#establish search criteria - searching for family Canidae
#many search criteria available check out the rgbif guide
key <- name_backbone(name = 'Canidae', rank='family')$usageKey

#run search and download 2000 records with coordinates -->
###Download takes a moment so skip to line 229 if uploading csv

Canidae<-occ_search(taxonKey=key,limit=2000,hasCoordinate = TRUE) #this takes a bit of time

#inspect Canidae -returns output summary
Canidae

#inspect slots in Canidae - we want data
names(Canidae)

#save data as csv in working directory
write.csv(Canidae$data,"Canidae_occ.csv")

#data is in tibble which is a modified data frame- lets change it to data frame to be consistent and store it in candat
canid_data <-data.frame(Canidae$data)

#canid_data<-read.csv("Example_Canidae/Canidae_occ.csv") #OR JUST READ IN CSV

#look at data
summary(canid_data)
names(canid_data)
str(canid_data)
#gbif data has too many columns, we want:
#lat=decimalLatitude
#long=decimalLongitude
#species=species

#convert data frame to SpatVector
canid_vec <-vect(canid_data, geom = c("decimalLongitude" ,"decimalLatitude"), crs = ("EPSG:4326"))

#lets look at the data colored by species
plot(canid_vec,col=as.factor(canid_vec$species),pch=16)

#Well that is not a great looking map, lets make better ones using ggplot 

#load global country boundaries shapefile
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

#let's convert to spatvector
world <- vect(world_sf)

#Or read in directly below
#world<-st_read("Example_Canidae/world.shp")
  #reading in at simple features as that is how ne_countries downloads
#world$name<-world$CNTRY_NAME

#plot the world
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
    data=canid_data,aes(
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
#warning ok just plotting at smaller scale than data


#For US only and add labels

#make labels
world_points <- centroids(world,inside=T)

#get coordinates
world_points$x<-crds(world_points)[,1]
world_points$y<-crds(world_points)[,2]

#and plot with labels scaling to just the US
ggplot(data = world)+
  geom_spatvector(color = "black", fill = "antiquewhite", lwd=0.5) +
  stat_summary_hex(data=canid_data,aes(x=decimalLongitude,y=decimalLatitude,z=speciesKey),
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


# Map locations by species

for (i in 1:length(unique(canid_data$species))) {  #running through 1: number of species
  sub<-canid_vec[canid_vec$species==unique(canid_vec$species)[i],] #for species i, subset candat to only that species's data
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



# ---- USING MOVEMENT DATA FROM MOVEBANK ----

#Neat, okay lets try GPS movement data from movebank, by accessing the movebank API 
#through the move2 package. We will grab fisher data from NY. We can use the data
#study ID to directly download open access data using the movebank_download_study() function:
movebank_store_credentials(username="" , password ="") #INPUT CREDENTIALS IF YOU HAVE THEM; OTHERWISE SKIP AND UNCOMMENT LINE 449 AND 450
s <- movebank_download_study(6925808)

#you will get a note that you need to approve the license and copy 
#'license-md5'='0cc38362d9a726efbc0347a0d8e65fcf' #into the previous line- 
#'let's do that
s <- movebank_download_study(6925808, 'license-md5'='0cc38362d9a726efbc0347a0d8e65fcf')

#this should take less that 20 or so seconds with a fast connection

#lets convert it data frame and add coordinates using the sf package (compatable with the move package)
fisher <- data.frame(s,sf::st_coordinates(s))
fisher <- vect(fisher, c("X", "Y"), crs = "+proj=longlat +datum=WGS84 +no_defs") #if loading data frame

#look at data
head(fisher)

#save to .shp
writeVector(fisher,"fisherdata.shp")

#read in shapefile instead of downloading; read next 2 lines
#fisher<-vect("Example_Fisher/fisherdata.shp")
#fisher$individual_local_identifier<-as.factor(fisher$individual)

#Oof lots of NA coordinates that going to be problematic later -lets remove NA coordinates
fisher <- na.omit(fisher,geom=T)

#look at how many points per individual
table(fisher$individual_local_identifier)

#look at the fisher data to make sure everything looks right
plot(fisher,col=as.factor(fisher$individual_local_identifier))
#let's take a quick look with mapview as well to see where this data is located
mapview::mapview(fisher, zcol = "individual_local_identifier")

#Excellent! Now we can move on to home ranges

#Calculate home ranges for individuals
#we are going to use minimum convex polygons to estimate the home range for each individual using the 
#mcp() function in adehabitatHR. This package requires sp classes so we need to transform our data
#from spatVector to sp. This package has some other useful movement statistics - check it out!

#to run home range we need only the individual id and coords in a spatial points class
fisher.drop <- as((fisher[,"individual_local_identifier"]), "Spatial")

#lets create a separate file for each individual
#initalize vector to store names of home range shapefiles
l <- rep(NA, length(unique(fisher.drop$individual_local_identifier)))

#loop through all unique individuals, calculate 95% Minimum convex polygon, rename shapefile by individual, save as shapefile,
#plot the MCP and add the name of the shapefile to l
for (i in 1:length(unique(fisher.drop$individual_local_identifier))) { #for 1: number of individuals
  sub <- fisher.drop[fisher.drop$individual_local_identifier==unique(fisher.drop$individual_local_identifier)[i],]
  #subset data for that individual
  sub$individual_local_identifier <- droplevels(sub$individual_local_identifier)
  #drop other individul factor levels because it was throwing an error
  mcp <- mcp(sub, percent=95)
  #calculate 95% mcp - for that individual store in object mcp
  assign(paste("MCP.", unique(fisher.drop$individual_local_identifier)[i], sep=""), mcp)
  #rename mcp object as MCP.[individual id] using assign() - I'm a fan of this function
  vect(get(paste("MCP.", unique(fisher.drop$individual_local_identifier)[i], sep="")),
       paste("95p_MCP_", unique(fisher.drop$individual_local_identifier)[i], ".shp", sep=""), overwrite=TRUE)
  #write a shapefile for the newly assigned MCP.[individual id] object, name it 95p_MCP_[individual Id]
  #the get() function takes the text and gets the r object with that name
  print(paste("MCP.",unique(fisher.drop$individual_local_identifier)[i], sep=""))
  #print the MCP.[individual Id] to track progress
  l[i] <- c(paste("MCP.", unique(fisher.drop$individual_local_identifier)[i], sep=""))
  #store the MCP.[individual.Id] in vector to access later
  
  #and a simple plot for each to examine
  plot(mcp, col=i)
  plot(sub, add=T, pch=16)
}

#so we did all that but if we just wanted the home ranges all in one shapefile we could do this
fisher.mcp.all <- (mcp(fisher.drop, percent=95))

#look at the home ranges
plot(fisher.mcp.all)

#####GATHER COVARIATES
#so now we have MCPs we need to get our covariates organized. We are using canopy and elevation
#we already brought in elevation (ele), but its for the country so too big to work with
#we can get canopy from NLCD but NLCD is also too big to work with rapidly at the country level

#get landscape data - tree canopy cover from NLCD and the FedData package
#this package is awesome because it crops NLCD as it brings it in otherwise the dataset is HUGE
#canopy<-get_nlcd(template = fisher , year = 2016, dataset = "canopy", label = "fisher canopy", force.redo = T)
#bring in canopy layer if get_nlcd() is not working
canopy <- rast("Example_Fisher/nlcd_canopy.tif")#Alternatively load raster from file

#everything needs to be in the same crs and matching extents. We can reproject our NLCD layers to match the fisher locations
#NLCD uses EPSG:5070 for their products.
canopy <- project(canopy, crs(fisher))
canopy <- crop(canopy, fisher)
plot(canopy)

#let's also pull in an elevation raster using FedData
elev <- get_ned(template = fisher, label = "fisher elev", force.redo = T)
elev <- project(elev, canopy)
elev <- crop(elev, canopy)
plot(elev)

fisher_sf <- st_as_sf(fisher.drop)

#let's look at our canopy layer with the fisher locations
ggplot() + geom_spatraster(data = canopy) + #maxcell = ncell(canopy), if you want to display all cells
  geom_sf(data = fisher_sf, color = "red", alpha = 0.2) +
  scale_fill_viridis(name = "canopy cover (%)", na.value = "transparent") + theme_bw() +
  facet_wrap(~individual_local_identifier, ncol = 2)

#we can also plot single individual home ranges and use a loop 
#need to transform home ranges CRS

#let's convert the fisher mcp that are a SpatialPolygonsDataFrame object to an sf object
fast_sf <- st_as_sf(fast)
#get the unique ids
fisher_ids <- unique(fast_sf$id)
for (i in 1:length(fisher_ids)){ #for every instance in 1:number of home ranges stored in l
  out <- fast_sf[fast_sf$id == fisher_ids[i],] #get all rows that match our fisher id
  #because we will need it in this class to sample random points in a minute
  out <- st_transform(out, crs(canopy))#transform it to the crs of our canopy layer
  #assign(paste(l[i],sep=""),out) #assign it the same name it had MCP.[individual Id]
  p.out <- ggplot() + geom_spatraster(data = canopy) + #maxcell = ncell(canopy), if you want to display all cells
    geom_sf(data = out, color = "red", fill = NA, lwd = 2) +
    scale_fill_viridis(name = "canopy cover (%)", na.value = "transparent") + theme_bw()
  print(p.out)
  #use ggsave to save outputs as pdfs/tifs/jpegs
  #ggsave(paste0("MCP_95_", fisher_ids[i], ".pdf"))  #example
}

# ---- RESOURCE SELECTION FUNCTION ANALYSIS ----

#okay we have our spatial data squared away- now we need to prep our data for RSF - 
# we have used points, these are our GPS data-lets create a dummy variable 'used' and input a 1 
#for these data
#add new column to fisher to name used points with 1
fisher$used <- 1

#plot the used points
plot(fisher, col=(fisher$used)+1, pch=16, cex=0.5)

#look at fisher
names(fisher)

#subset fisher to just used and individual columns, call it fish
fisher.subset <- fisher[,c("used", "individual_local_identifier")]
fisher.subset

#Okay now we need to compare used to what was available to each individual in their home range
#loop through each home range and sample as many points as there are points for that individual, 
#add a column used with the label 0, bind to the used GPS points
for (i in 1:length(l)){#for 1: number of home ranges
  out <- spatSample(vect(get(l[i])), 
            nrow(fisher[fisher$individual_local_identifier==unique(fisher.drop$individual_local_identifier)[i],]), method="random")
  #sample X random points from home range i, where X=the number of used points for that individual
  out$used <- 0
  #add a column to out that has tthe dummy variable used, but used =0 (i.e., unused)
  fish <- rbind(fish,out)
  #bind the new available points to the used data and all previously sampled available points
}

#plot the used and available points
plot(fish,col=(fish$used)+1, pch=16, cex=0.5)

#The next step is to extract covariate values to the points

#extract raster data to points
fish$LC <- terra::extract(canopy,fish,ID=F,method="bilinear")
fish$ele <- terra::extract(elev,fish,ID=F,method="bilinear")

model<-glm(used ~ -1 + scale(LC) + scale(as.numeric(ele)), data=fish, family='binomial') #takes a few moments to run 
#look at our model summary
summary(model)

##Okay thats super cool fishers like canopy cover and avoid higher elevations here but what does that
#look like on the landscape??? To look at suitability across a landscape we need to plug in the  
#values of each pixel on the landscape to our model and see the suitability - we can do this using 
#the predict() function if we have those landscape values

#so lets get values for our covariates across the landscape
#create grid of points across landscape
samp <- spatSample(ext(fisher), size=100000, method="regular", lonlat=F)
plot(samp)

pred.temp <- vect(samp)

#extract values
pred.temp$LC <- terra::extract(canopy,pred.temp, ID=F, method="bilinear")
pred.temp$ele <- terra::extract(elev,pred.temp, ID=F, method="bilinear")

#remove NAs for predict
pred.temp <- na.omit(pred.temp, geom=T)

#Create newdata from these files
newdata <- data.frame(pred.temp)

#create predictions across surface
pred.temp$pred <- as.numeric(predict(model, newdata=newdata, type='response'))

#scale predictions so values are between 0 and 1
pred.temp$pred <- (pred.temp$pred-min(pred.temp$pred,na.rm=T))/(max(pred.temp$pred,na.rm=T)-min(pred.temp$pred,na.rm=T))

#NLCD is super fine scale and takes too much memory to process
#need to lower resolution of NLCD raster for processing
#convert our background point predictions to a raster 
rsf.preds <- rasterize(pred.temp, aggregate(canopy, fact=6), field="pred")

#plot it!
plot(rsf.preds)

#add the home ranges to the map
for (i in 1:length(l)){
  plot((get(l[i])), lwd=3, add=T)
}


#### BONUS (if time allows): LANDSCAPE METRICS OF MCPs   #############################################################################################################

#what if we are interested in the landscape composition, configuration, or connectivity within our MCPs?
#we could use the r package landscapemetrics for calculating landscape metrics of categorical landscape patterns 
#https://r-spatialecology.github.io/landscapemetrics/

#let's get nlcd land cover using the FedData package again
nlcd <- get_nlcd(fisher, year = 2016, dataset = "landcover", label = "Fisher Landcover", force.redo = T)

#Alternatively let's use our downloaded raster file
#nlcd <- rast("Example_Fisher/NLCD_Fish.tiff")
  
#we will need our MCPs to be the same crs as nlcd. we can keep using our sf mcp object (fast_sf)
crs(nlcd, describe=T)
fast_sf <- st_transform(fast_sf, crs = crs(nlcd)) #NLCD uses EPSG:5070 that is a NAD83 datum and Albers projection

#for this we will use our nlcd land cover raster
#first we need to mask and crop our raster to each individual mcp
#we can do this in a simple for loop and put the results in a list
mcp_lc_stack = list() #create an empty list to store our individual mcps

for(i in 1:nrow(fast_sf)){
  mcp.crop <- crop(nlcd, fast_sf[i,]) #crop nlcd raster to the ith row in mcp
  mcp.mask <- mask(mcp.crop, fast_sf[i,]) #mask nlcd raster to the ith row in mcp
  mcp_lc_stack[[i]] <-  mcp.mask #set ith element of list to masked mcp
}
names(mcp_lc_stack) <- fast_sf$id #reassign individual names to our list elements

#plot one from the list for an example
plot(mcp_lc_stack[[1]])

#we can use lapply to run a named vector of landscape metrics on individual MCPs
#lapply will run a function on every element of a list (like a for loop, but usually quicker processing)
#in this example for each class we can calculate the proportion of land cover (pland), edge density (ed), and contiguity value (contig, i.e., connectivity)
#to do this we call the calculate_lsm() function and using c() to input all of the metrics we would like to calculate
#https://r-spatialecology.github.io/landscapemetrics/
class_metrics <- lapply(mcp_lc_stack, function(x) calculate_lsm(x, what = c("lsm_c_pland", "lsm_c_ed", "lsm_c_contig_mn")))

#say we just want one of the classes, we can filter by class value, pivot, and unlist so we have a dataframe
#NLCD class value 42 is Evergreen Forest
evergreen_list <- lapply(class_metrics, function(x) filter(x, class == 42)) #apply the filter() function to each element in the list
#pivot to a wide format so each individual mcp is a row and columns are the different metrics
evergreen_metrics <- lapply(evergreen_list, function(x) pivot_wider(x, names_from = metric, values_from = value)) 
evergreen_df <-  bind_rows(evergreen_metrics, .id = "individual") #combines our list elements into a dataframe
evergreen_df #take a look

#we can also retain all cover class and output as a csv
metrics_wider <- lapply(class_metrics, function(x) pivot_wider(x, names_from = metric, values_from = value))
class_metrics_df <-  bind_rows(metrics_wider, .id = "column_label")

#write as a csv to your working directory
write.csv(class_metrics_df, paste0(wd, "/mcp_class_metrics.csv"), row.names = FALSE)

