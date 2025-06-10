#for this exercise, we will
#(1) read in the boundary for Hwange NP (Hwange_NP.shp) & distance to waterhole (Dist_Waterhole_Hwange.tif)
#(2) convert distance to waterhole to a data frame for use in ggplot
#(3) create ten breaks within the distance to waterhole values
#(4) plot those ten classes!

#change this to your working directory
setwd("YOUR WORKING DIRECTORY HERE")

library(terra)
library(tidyterra)
library(ggplot2)

#read in Hwange NP 
Hwange <- vect("Example_Zimbabwe/Hwange_NP.shp")

#read in distance to waterhole
distwater <- rast("Example_Zimbabwe/Dist_Waterhole_Hwange.tif")

#check out the name of the column with raster values (this will be the column we supply to cut() )
#ok, it is called "Dist_Waterhole_Hwange"
head(distwater)

#create ten breaks
distwater_10groups <- distwater %>%
  mutate(distwater_brk = cut(Dist_Waterhole_Hwange, breaks = 10))

#how many pixels fall into each group?
distwater_df_10groups <- as.data.frame(distwater_10groups)
distwater_df_10groups %>%
  group_by(distwater_brk) %>%
  count()

#now let's plot these ten classes
ggplot() +
  geom_spatraster(data = distwater_10groups , aes(fill = distwater_brk)) +
  geom_spatvector(data = Hwange, color = "black", fill = NA, size=2) 

#let's write this to a .jpg using ggsave()
ggsave("Part5_Cartography_Outputs/exercise_Hwange_distwater_10groups.jpg", height=4, width=6, units = "in", dpi=300)

