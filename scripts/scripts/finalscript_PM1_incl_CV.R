###################
########Final Script- PM1 _part of Cross Validation
###################
########Ipsit Dash
#Removing all previous environment variables
rm(list = ls())

#calling libraries
library(MASS)
require(gstat)
require(spacetime)
library(ggplot2)
library(stats)
require(graphics)
require(sp)
library(maptools)
library(rgdal)
library(lsr)
library(BBmisc)
library(sp)
library(spacetime)
library(gstat)
library(maptools)
library(mapdata)
library(maps)
library(rgdal)

###############################################
#importing dataset
load("aball.RData")
ls()

#importing subsetted dataset for PM1
pm101 <- as.data.frame(read.csv("subset_data/pm1_1.csv", head = T, sep =","))
pm101_16 <- as.data.frame(read.csv("subset_data/pm1_1_cv_st16.csv", head = T, sep =","))
#pm11 <- as.data.frame(t(pm101))
#pm1010 <- as.data.frame(read.csv("subset_data/pm1_only_data.csv", head = T, sep =","))
##############################################
##Follow this
pm1010_t <- as.data.frame(read.csv("subset_data/pm1_only_data_transposed.csv", head = T, sep =","))
pm10101_t <- as.data.frame(t(pm1010_t))
pm1010_16t <- as.data.frame(read.csv("subset_data/pm1_only_data_transposed_cv_st16.csv", head = T, sep =","))
pm10101_16t <- as.data.frame(t(pm1010_16t))

#############################################
#pm101 <- as.data.frame(tFrame(pm101))
#pm251 <- read.csv("subset_data/pm2.5.csv", head = T, sep =",")
#pm1001 <- read.csv("subset_data/pm10.csv", head = T, sep =",")

#pm1 <- read.csv("subset_data/pm1.csv", head = T, sep =",")
#pm12 <- as.data.frame(tFrame(pm1))
#names(pm12) <- gsub("V", "Station", names(pm1))
#pm2.5 <- read.csv("subset_data/pm2.5.csv", head = T, sep =",")
#pm2.5 <- as.data.frame(tFrame(pm2.5))
#names(pm2.5) <- gsub("V", "Station", names(pm2.5))
#pm10 <- read.csv("subset_data/pm10.csv", head = T, sep =",")
#pm10 <- as.data.frame(tFrame(pm10))
#names(pm10) <- gsub("V", "Station", names(pm10))
##############################################

#############################################
#Creating a STFDF Object
#############################################

#Creating a SP Object
sp_new = cbind(x = c(pm101$lon), y = c(pm101$lat))
sp2 = SpatialPoints(sp_new)
sp_new_16 = cbind(x = c(pm101_16$lon), y = c(pm101_16$lat))
sp2_16 = SpatialPoints(sp_new_16)

#sp1 = SpatialPointsDataFrame(data.frame(pm101$lon, pm101$lat), pm101)
#sp_new_NE = sp_new
#sp_new_NE = SpatialPoints(sp_new_NE)
#proj4string(sp_new_NE) = CRS(proj4string(st.all.obs))
#sp_new-NE = spTransform(,CRS("+proj=longlat +ellps=GRS80") )
###############################################
#Creating a Temporal Object
time365_1 = as.POSIXct("2014-10-01")+3600*(seq(1,336, by=1))

#time365 = as.Date(as.POSIXct("2014-10-01")+3600*(seq(0,336, by=1)))

##############################################
#Creating the PM1 observations as a vector and then as a matrix

#timeIsInterval(time365) = TRUE
#mydata_new1= as.vector(t(pm101[1:32,4:339]))
#mydata_new3= as.numeric(as.vector(t(pm1[1:32,6:341])))

mydata_new2= as.vector(t(pm1010_t))
mydata_new2_16= as.vector(t(pm1010_16t[3:338,]))
#mydata_new4= as.vector(a)
#a =convertColsToList(pm1010, name.list = FALSE, name.vector = FALSE,factors.as.char = FALSE, as.vector = TRUE)
#mydata_new2 = as.list(t(pm101[1:32,4:339]))
#mydata_new1 = as.data.frame(t(pm101[1:32,4:339]))
#mydata_new2 =c(unname(as.data.frame(pm101[1:32,4:339])))
#mydata1 = data.frame(value =pm101[1:32,4:339])


############################################################
IDs = paste("ID",1:length(mydata_new2))
#IDs = paste("ID",1:10752)
mydata2 = data.frame(value=mydata_new2, ID=IDs)#try to do without IDs

#Creating the STFDF object
st.all.obs = STFDF(sp2,time365_1,mydata2)

#Giving a CRS projection for the STFDF object

st.all.obs@sp@proj4string = CRS("+init=epsg:3035")

##############################################################
#For cross validation
IDs_16 = paste("ID",1:length(mydata_new2_16))
#IDs = paste("ID",1:10752)
mydata2_16 = data.frame(value=mydata_new2_16, ID=IDs_16)#try to do without IDs

#Creating the STFDF object
st.all.obs_16 = STFDF(sp2_16,time365_1,mydata2_16)

#Giving a CRS projection for the STFDF object

st.all.obs_16@sp@proj4string = CRS("+init=epsg:3035")
#################################################
#importing the shapefile
ceShape.shp <- readShapePoly("C:/Users/Ipsit Dash/Documents/Module13/MiniProject/shapefile/Wijk_Buurt_projected_3035.shp")
proj4string(ceShape.shp) <- CRS("+init=epsg:3035")
tmp = spTransform(ceShape.shp,CRS("+proj=longlat +ellps=GRS80") )

#plot the shapefile
plot(ceShape.shp)


################################################
dutch_map <- map("worldHires","Netherlands",plot=F, fill=T)
plot(dutch_map)
#ger_map <- map("worldHires","Germany",plot=F, fill=T)
#IDs1 <- sapply(strsplit(dutch_map$names, ":"), function(x) x[1])
#dutch <- map2SpatialPolygons(dutch_map,IDs=IDs1,CRS(proj4string(st.all.obs))
#dutch_lines <- map2SpatialLines(dutch_map,IDs=IDs1,CRS(proj4string(st.all.obs))
#dutch <- spTransform(dutch,CRS(proj4string(st.all.obs)))
#dutch_lines <- spTransform(dutch_lines,CRS(proj4string(st.all.obs)))
#dutch_oct <- st.all.obs[dutch,"2014-10","value"]
#dutch_oct <- as(dutch_oct, "STFDF")



# check for bounding box
tmp@bbox
spl1 <- list("sp.polygons", tmp, first = FALSE) 
spl2 <- list("sp.points",sp2, pch="+", sp2, col=2, cex=2)
spl <- list(spl1, spl2)

xy  <- expand.grid(x=seq(5.35, 5.55, by=0.015), y=seq(51.38,51.51, by=0.015))
dutch_grid <- SpatialPoints(xy)
gridded(dutch_grid) <- TRUE
proj4string(dutch_grid) <- CRS("+init=epsg:3035")
plot(dutch_grid, axes=T)
points(sp2, col=2, pch=19)

dutch_gridded <- STF(dutch_grid, st.all.obs@time[1:24])
dutch_gridded@sp@proj4string = CRS(proj4string(st.all.obs))
plot(dutch_gridded)


####change 1:336 - Its for all the  observations
stplot(st.all.obs[,1:2,"value"], col.regions=bpy.colors(), 
       sp.layout = list("sp.polygons", tmp),
       cuts=0:6*5, main="")

stplot(st.all.obs[,1:2],sp.layout = list("sp.polygons", tmp))

#################################################
#Spatio-temporal Variograms

## empirical st-variogram
empVgm1 <- variogramST(value~1, st.all.obs, tlags = 0:48)

##empirical st-variogram for CV
empVgm_16 <- variogramST(value~1, st.all.obs_16)
plot(empVgm1, wireframe=T, scales=list(arrows=F),
     col.regions=bpy.colors(),zlab=list(rot=90),zlim=c(0,20), main ="Empirical ST variogram: PM1", sub ="tlag =0:48")

## metric model
metricModel <- vgmST("metric", 
                     joint=vgm(0.8,"Exp", 150, 0.2),
                     stAni=100)
metricFit <- fit.StVariogram(empVgm,metricModel,lower=c(0,10,0,10))

separableModel <- vgmST("separable", 
                        space=vgm(0.3,"Exp", 100, 0.7),
                        time =vgm(0.04,"Exp", 22, 0.9),
                        sill=50)
separableFit_16 <- fit.StVariogram(empVgm_16,separableModel,lower=c(0,0,0,0),method ="L-BFGS-B")

attr(metricFit,"optim.output")$value # 1.30
attr(separableFit,"optim.output")$value # 1.30


plot(empVgm, list(metricFit,separableFit), map =F)
plot(empVgm, separableFit, map =F)
plot(empVgm, metricFit, wireframe=TRUE, all=TRUE)
plot(empVgm, separableFit, wireframe=TRUE, all=TRUE)

###############################################
# prediction
dutch_gridded <- STF(dutch_grid, st.all.obs@time[25:48])
dutch_gridded@sp@proj4string = CRS(proj4string(st.all.obs))#change with st.all.obs_16 for CV
#plot(dutch_gridded)


predMetric <- krigeST(value~1, st.all.obs[,25:48], 
                       dutch_gridded, 
                       separableFit)
X11()
stplot(predMetric,sp.layout = spl1)
X11()
stplot(predMetric[,8:9],sp.layout = spl1)
# stplot(predMetric,col.regions=bpy.colors(25), at=0:25,
#        sp.layout=list("sp.points",ger_june[,15],
#                       pch=1,col="black",cex=2))
# 
# spplot(predMetric[,2],col.regions=bpy.colors(25), at=0:25,
#        sp.layout=list("sp.points",ger_june[bools,15],fill=cols_sub,pch=22,col="black",cex=2),
#        main=paste("day",15))








#take 3 random stations one by one- remove all the observations pertaining to that station and then compare the observation and predictions..

rnd_stn_1 =round(32*runif(1)+0.5,0)

rnd_stn_2 =round(32*runif(1)+0.5,0)

rnd_stn_3 =round(32*runif(1)+0.5,0)

