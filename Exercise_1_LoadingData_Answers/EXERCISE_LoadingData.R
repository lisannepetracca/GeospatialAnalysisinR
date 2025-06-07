library(terra)
#Change to your working directory path
setwd("./Exercise_1_LoadingData_Answers") #CMN changed

#load in a .tif image
ras<-rast("moon.tif") # unknown extent
plot(ras)

#inspect ras to get the min and max x and y values
ras
x<-runif(25,1,358)
y<-runif(25,1,500)
df<-data.frame(x=x,y=y)
points<-vect(df,geom=c("x","y"), crs=crs(ras))


plot(ras,add=F)
plot(points,col="red",pch=16,add=T)


