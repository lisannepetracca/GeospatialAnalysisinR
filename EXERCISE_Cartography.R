setwd("C:/Users/lspetrac/Desktop/Geospatial_Analysis_in_R")

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
head(distwater_df)

#create ten breaks
distwater_df_10groups <- distwater_df %>%
  mutate(distwater_brk = cut(Dist_Waterhole_Hwange, breaks = 10))

#how many pixels fall into each group?
distwater_df_10groups %>%
  group_by(distwater_brk) %>%
  count()

#now let's plot these ten classes
ggplot() +
  geom_raster(data = distwater_df_10groups , aes(x = x, y = y, fill = distwater_brk)) +
  geom_sf(data = Hwange, color = "black", fill = NA, size=2) 
  
