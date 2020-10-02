
###Create function to thin points by a certain buffer distance
thinpoints<-function(points,buffer_dist){#points sf class
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

#example
#points needs to be sf class,  buffer distance in scale of crs
new<-thinpoints(points=random_points,buffer_dist=10000)
plot(st_geometry(new),col="red",pch=16,add=T)
