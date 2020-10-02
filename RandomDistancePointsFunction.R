
###Create function to thin points by a certain buffer distance
thinpoints<-function(points,buffer_dist){
  require(sf)
  i=1 #initiate
  repeat( {
    buffer <- st_buffer(points[i,], buffer_dist ) #  create buffer of buffer_dist around i-th point
    offending <- points %>%  # start with the intersection of master points... 
      st_intersects(buffer, sparse = F) # ... and the buffer, as a vector
    offending[i] <- FALSE #mark point i as not offending as it will always intersect the buffer
    points<-points[which(offending==FALSE),]#select to keep only non offending points
    if ( i >= length(points)) {
      # the end was reached; no more points to process
      break 
    } else {
      i <- i + 1 # rinse & repeat
      #adapted from https://www.jla-data.net/eng/creating-and-pruning-random-points-and-polygons/
    }
      } )
  
  
points} #save final points as function output


#Example of function use
PAs <- st_read("Example_Honduras\\Honduras_Protected_Areas_2007.shp")#call in shapefiile

PicoBonito <- PAs[PAs$NOMBRE == "Pico Bonito-Zona Nucleo",] #subest to only Pico bonito
PicoBonito <- PAs[1,] #remove weird NAs that popped up


random_points <- st_sample (PicoBonito, 250, type="random", exact=T)#sample random points (will need to sample more than neede consider degree of thinning)
#random_points<-st_sf(random_points)
plot(random_points,pch=16) #plot the original random points

  
new<-thinpoints(points=random_points,buffer_dist=1000)
plot(st_geometry(new),col="blue",pch=16,add=T)
