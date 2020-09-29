#install.packages("devtools")
#devtools::install_github("ropensci/FedData")##Probably need to install this & devtools first - may need to close instances of R / restart computer
#install.packages(c("sp", "sf", "raster", "rgeos"))
#install.packages(c("move","adehabitatHR","rgbif","ggmap"))

library(sp)
library(sf)
library(raster)
library(move)
library(FedData)
library(adehabitatHR)
library(rgbif)
library(ggmap)


#Make sure working directory is also saved as object wd
wd<-setwd("C:/Users/acheesem/Desktop/ESF/Classes Taught/GIS in R workshop")#Change to your working directory path

###Fun with loops

#make a simple loop printing every instance in 1:10
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

#here we are going to grab elevation and New York Sate boundaries 
  #we are going to do this using loops to practice

#because in zip files need to create pathways for each zip file to be downloaded to
ny<-("/ny") #boundaries stored in new folder NY
ele<-("/ele") #ele stored in new folder ele
root<- c(ny,ele) #going to bind folder pathways into vector to reference later

#get URLS from internet for zip files
files<-c(
  "http://gis.ny.gov/gisdata/fileserver/?DSID=927&file=NYS_Civil_Boundaries.shp.zip",
  "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Grid_ZipFiles/mn30_grd.zip")


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

#read in NY boundary shapefile & elevation raster
NY<-st_read("NYS_Civil_Boundaries_SHP/Counties_Shoreline.shp")#this one is nested in another folder
#Plot NY shapefile and view
plot(st_geometry(NY)) 
ele<-raster("mn30_grd")#DONT PLOT,load raster file-this is for the entire US, LARGE FILE!!


###########################################################
######################GBIF Exercise ###Map multiple species

#establish search criteria - searching for genus panthera
  #many search criteria available check out the rgbif guide
key <- name_backbone(name = 'Panthera', rank='genus')$usageKey

#run search and download 2000 records with coordinates -->
###Download takes a moment so skip to line 124 if uploading csv

panthera<-occ_search(taxonKey=key,limit=2000,hasCoordinate = TRUE)#this takes a bit of time

#inspect panthera -returns output summary
panthera

#inspect slots in panthera - we want data
names(panthera)

#save data as csv in working directory
write.csv(panthera$data,"panthera_occ.csv")

#data is in tibble which is a modified data frame- lets change it to data frame to be consistent and store it in pandat
pandat<-data.frame(panthera$data)

#pandat<-read.csv("panthera_occ.csv") #OR JUST READ IN CSV

#look at data
summary(pandat)
names(pandat)
str(pandat)
#gbif data has too many columns, we want:
  #lat=decimalLatitude
  #long=decimalLongitude
  #species=species

#convert data frame to simple feature
pandat<-st_as_sf(pandat, coords = c("decimalLongitude" ,"decimalLatitude"), crs = CRS("+proj=longlat +datum=WGS84"))

#lets look at the data colored by species
plot(st_geometry(pandat),col=as.factor(pandat$species),pch=16)

#Well that is not a great looking map, lets make better ones using ggmap and google imagery
  #and lets make a map for each species 

#First we need to load out google API key
  #I put my key in a saved file that can be easily referenced
key<-read.csv("C:/Users/acheesem/Desktop/Mammal Atlas/Data/Google_API.csv")
register_google(key = key$Key)  #need your google key, will probably need to register for a google API project using static maps https://console.developers.google.com

#load the background map outside the loop if it does not change, center map on mean of pandat coordinates
bgmaps <- get_map(location=c(lon=mean(st_coordinates(pandat)[,2]), lat=mean(st_coordinates(pandat)[,1])), 
                  zoom=1, scale=2,maptype="satellite")


#loop over each species in pandat, subset data for that species, then map it and print pdf saved to wd
for (i in 1:length(unique(pandat$species))) {  #running through 1: number of species
  sub<-pandat[pandat$species==unique(pandat$species)[i],] #for species i, subset pandat to only that species's data
  map<-ggmap(bgmaps) +  #call google map in ggmap
    geom_point(aes(x = st_coordinates(sub)[,1], y = st_coordinates(sub)[,2]) , #add points from subset data
               data = sub, colour="yellow",alpha = .5)+ #color points yellow, partly transparent
    labs(x = 'latitude', y = 'longitude') + ggtitle(paste((sub$species[1]))) #label axis and title
  plot(map) # plot the map in the R environment
  
  
  pdf(paste(unique(sub$species)[1],Sys.Date(),".pdf",sep=""),height=6,width=6) #print a pdf to your working directory of species i, 
    #title it by species i and the date
  plot(map) #you are plotting the map in the pdf
  dev.off() #closing the graphics device for the pdf
} #close the loop

#look through the maps in pdfs or in RStudio


#######################################################################################
###########################  MOVEBANK #################################################
#Neat, okay lets try GPS movement data from movebank, this is marten data from NY
  #we need to first access the Movebank API using the move package, we can use the data
  #repository doi to direclty download open access data using the getDataRepositoryData() function

s<-getDataRepositoryData("doi:10.5441/001/1.2tp2j43g")# alternitively skip to 198 to upload from saved file

#lets look at what we downloaded
s
#its a Movestack 

#lets convert it to a sf so we can work with it
marten<-st_as_sf(s)

#look at data
head(marten)
str(marten) #note missing individual ID and also timestamp is in the wrong format

#pull individual ID from MoveStack
str(s)#look to see where ID is stored

#ID is in trackId slot- to call it we use the @ symbol and store as indivdual.local.identifier
marten$individual.local.identifier<-as.factor(s@trackId)

#read in CSV instead of downloading; read next 3 lines
#m<-read.csv("martendata.csv")#'Martes pennanti LaPoint New York_121719/Martes pennanti LaPoint New York.csv')
#marten<-st_as_sf(m, coords = c("X", "Y"), crs = "+proj=longlat +datum=WGS84 +no_defs") #if loading data frame
#marten$individual.local.identifier<-as.factor(marten$individual.local.identifier)

#convert timestamp to date using the as.POSIXct() function
marten$timestamp<-as.POSIXct(marten$study.local.timestamp)
str(marten) #check to see it worked

#look at how many points per individual
table(marten$individual.local.identifier)

#look at the marten data to make sure everything looks right
plot(st_geometry(marten),col=as.factor(marten$individual.local.identifier))
  
#Excellent! Now er can move on to home ranges

#####calculate home ranges for individuals
  #we are goiung to use minimum convex polygons to estimate the home range for each individual using the 
  #mcp() function in adehabitatHR. Thos package requires sp classes so we need to transform our data
  #from sf to sp. This package has some other useful movement statistics - check it out!

#to run home range we need only the individual id and coords in a spatial points class
marten.drop<-as_Spatial(marten[,"individual.local.identifier"])


#lets create a separate file for each individual
#initalize vector to store names of home range shapefiles
l<-rep(NA,length(unique(marten.drop$individual.local.identifier)))

#loop through all unique individuals, calculate 95% Minimum convex polygon, rename shapefile by individual, save as shapefile,
  #plot the MCP and add the name of the shapefile to l
for (i in 1:length(unique(marten.drop$individual.local.identifier))) { #for 1: number of individuals
  sub<-marten.drop[marten.drop$individual.local.identifier==unique(marten.drop$individual.local.identifier)[i],]
    #subset data for that individual
  sub$individual.local.identifier<-droplevels(sub$individual.local.identifier)
    #drop other individul factor levels because it was throwing an error
  mcp<-mcp(sub, percent=95)
    #calculate 95% mcp - for that individual store in object mcp
  assign(paste("MCP.",unique(marten.drop$individual.local.identifier)[i],sep=""),mcp)
    #rename mcp object as MCP.[individual id] using assign() - I'm a fan of this function
  shapefile(get(paste("MCP.",unique(marten.drop$individual.local.identifier)[i],sep="")),
            paste("95p_MCP_",unique(marten.drop$individual.local.identifier)[i],".shp",sep=""),overwrite=TRUE)
    #write a shapefile for the newly assigned MCP.[individual id] object, name it 95p_MCP_[individual Id]
    #the get() function takes the text and gets the r object with that name
  print(paste("MCP.",unique(marten.drop$individual.local.identifier)[i],sep=""))
    #print the MCP.[individual Id] to track progress
  l[i]<-c(paste("MCP.",unique(marten.drop$individual.local.identifier)[i],sep=""))
    #store the MCP.[individual.Id] in vector to access later
}
#ignore the warning ' In proj4string(xy) : CRS object has comment, which is lost in output' if you get it 
  #it relates to recent package PROJ6/GDAL3 updates not being consistent need to download development versiosn to eliminate


#so we did all that but if we just wanted the home ranges all in one shapfile we could do this
fast<-(mcp(marten.drop, percent=95))

#look at the home ranges
plot(fast)



#####GATHER COVARIATES
#so now we have MCPs we need to get our covariates organized. We are using canopy and elevation
  #we already brought in elevation (ele), but its for the country so too big to work with
  #we can get canopy from NLCD but NLCD is also too big to work with rapidly at the country level


#set bounding box for downloading & working with rasters as covariates, setting using sp 
  #because we will need it for the raster crop and get_nlcd() function
ext<-extent(as_Spatial(marten))+c(-0.25,0.25,-0.25,0.25)

#lets crop that earlier elevation layer
ele.p<-crop(ele, ext)
plot(ele.p)

#get landscape data - Tree canopy cover from NLCD and the FedData package
  #this package is awesome because it crops NLCD as it brings it in otherwise the dataset is HUGE

nlcd<-get_nlcd(template = polygon_from_extent(ext,
  proj4string=paste(crs(marten))), year = 2016, dataset = "Tree_Canopy", label = "Marten Land", force.redo = T)

#nlcd<-raster("nlcd_canopy.tif")#Alternitively load raster from file

#ignore the warnings abount 'Discarded datum WGS_1984 in CRS definition' if you get it 
#it relates to recent package PROJ6/GDAL3 updates not being consistent need to download development versiosn to eliminate
#THIS WILL CONTINUE POPPING UP, CONTINUE TO IGNORE FOR THIS, IDEALLY USE VERSION OF R & PACKAGES THAT DOES NOT GIVE ERROR

#look at it
nlcd
plot(nlcd)

#poject elevation to nlcd
ele.p<-projectRaster(ele.p,nlcd)
ele.p
nlcd

#everything needs to be in the same crs  -  because NLCD is the largest we will transform everything to 
  #the crs of the NLCD layer because this will be faster
#Transform points to raster CRS 
marten<-st_transform(marten,crs(nlcd))

#look at them on top of NLCD canopy layer
plot(st_geometry(marten),add=T,pch=16)

####Transform home ranges CRS and plot in a loop
#remember what l is
l
for (i in 1 :length(l)){ #for every instance in 1:number of home ranges stored in l
  out<-st_as_sf(get(l[i])) #get the object l[i] and convert to simple features class 
    #because we will need it in this class to sample random points in a minute
  out<-st_transform(out,crs(nlcd))#transform it to the crs of nlcd
  assign(paste(l[i],sep=""),out) #assign it the same name it had MCP.[individual Id]
  plot(st_geometry(out),add=T,col=i)#plot it on the NLCD canopy layer that is open
}



############### BASIC RESOURSE SELECTION FUCTION (RSF)  EXERCISE  ##############

#okay we have our spatial data squared away- now we need to prep our data for RSF - 
  # we have used points, these are our GPS data-lets create a dummy variable 'used' and input a 1 
  #for these data
#add new column to marte to name used points with 1
marten$used<-1

#plot the used points
plot(st_geometry(marten),col=(marten$used)+1,pch=16,cex=0.5)


#look at marten
names(marten)

#subset marten to just used and individual columns, call it mart
mart<-marten[,c("used","individual.local.identifier")]
mart

#Okay now we need to compare used to what was available to each individual in their home range
  #loop through each home range and sample as many points as there are points for that individual, 
  #add a column used with the label 0, bind to the used GPS points
for (i in 1:length(l)){#for 1: number of home ranges
out <- st_sample(get(l[i]), nrow(marten[marten$individual.local.identifier==
                unique(marten.drop$individual.local.identifier)[i],]), type="random", exact=T)
  #sample X random points from home range i, where X=the number of used points for that individual
out<-st_sf(individual.local.identifier=unique(marten.drop$individual.local.identifier)[i],used=rep(0,length(out)),geometry=out)
  #add a column to out that has the unique id for individual and the dummy variable used, but used =0
mart<-rbind(mart,out)
  #bind the new available points to the used data and all previously sampled available points
}

#plot the used and available points
plot(st_geometry(mart),col=(mart$used)+1,pch=16,cex=0.5)

#The next step is to extrat covariate values to the points - but we should double check the crs match

#check that coord systems align
crs(mart)
crs(nlcd)
crs(ele.p)

#great!!!

#extract raster data to points
mart$LC<-extract(nlcd,mart)
mart$ele<-extract(ele.p,mart)

#run rsf
model<-glm(used~-1+scale(LC)+scale(ele), data=mart,family='binomial') #takes a few moments to run 
#look at our model summary
summary(model)



##Okay thats super cool martens like land cover and avoid higher elevations here but what does that
  #look like on the landscape??? To look at suitability across a landscape we need to plug in the  
  #values of each pixel on the landscape to our model and see the suitability - we can do this using 
  #the predict() function if we have those landscape values

#so lets get values for our covariates across the landscape
#create grid of points across landscape
samp<-st_sample (st_as_sf(polygon_from_extent(extent(nlcd),
                                     proj4string=paste(crs(nlcd)))), 105625, type="regular", exact=T)

#make grid of points an sf object
samp<-st_sf(samp)

#extract raster data to background points
samp$ele<-extract(ele.p,samp)
samp$LC<-extract(nlcd,samp)

#create predictions across surface
samp$pred<-as.numeric(predict(model,newdata=samp,type='response'))

#scale predictions so values are between 0 and 1
samp$pred<-(samp$pred-min(samp$pred,na.rm=T))/(max(samp$pred,na.rm=T)-min(samp$pred,na.rm=T))

#NLCD is super fine scale and takes too much memory to process
  #need to lower resolution of NLCD raster for processing
p.ras<-aggregate(nlcd, fact=12)

#convert our backgound point predictions to a raster 
d<-rasterize(as_Spatial(samp),p.ras,"pred",na.rm=F)

#plot it!
plot(d)

#add the home ranges to the map
for (i in 1:length(l)){
plot(st_geometry(get(l[i])),lwd=3,add=T)
}

