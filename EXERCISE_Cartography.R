#for this exercise, we will
#(1) read in the boundary for Hwange NP (Hwange_NP.shp) & distance to waterhole (Dist_Waterhole_Hwange.tif)
#(2) convert distance to waterhole to a data frame for use in ggplot
#(3) create ten breaks within the distance to waterhole values
#(4) plot those ten classes!

library(sf)
library(raster)
library(dplyr)
library(ggplot2)

#read in Hwange NP 
Hwange <- st_read("Example_Zimbabwe/Hwange_NP.shp")

#read in distance to waterhole
distwater <- raster("Example_Zimbabwe/Dist_Waterhole_Hwange.tif")

#convert to data frame for use with ggplot
distwater_df <- as.data.frame(distwater, xy=TRUE)

#check out the name of the column with raster values (this will be the column we supply to cut() )
#ok, it is called "Dist_Waterhole_Hwange"
head(distwater_df)

#create ten breaks
distwater_df_10groups <- distwater_df %>%
  mutate(distwater_brk = cut(distwater_df[,3], breaks = 10))

#how many pixels fall into each group?
distwater_df_10groups %>%
  group_by(distwater_brk) %>%
  count()

#now let's plot these ten classes
ggplot() +
  geom_raster(data = distwater_df_10groups , aes(x = x, y = y, fill = distwater_brk)) +
  geom_sf(data = Hwange, color = "black", fill = NA, size=2) 
  
