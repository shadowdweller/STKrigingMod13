###################
########Final Script
###################
########Ipsit Dash
#Removing all previous environment variables
rm(list = ls())
dev.off()
#calling libraries
library(MASS)
require(gstat)
require(spacetime)
library(ggplot2)
library(stats)
require(graphics)
require(sp)
library(rgdal)
library(lsr)
library(BBmisc)
library(maptools)
library(mapdata)
library(maps)
library(rgdal)

###############################################
#importing pre-processed dataset
###############################################
#importing subsetted dataset for PM10 for SP attribute for STFDF object
pm10_1 <- as.data.frame(read.csv("subset_data/pm10_1.csv", head = T, sep =","))
pm10_1_cv_s16 <- as.data.frame(read.csv("subset_data/pm10_1_cv_st16.csv", head = T, sep =","))
pm10_1_cv_s32 <- as.data.frame(read.csv("subset_data/pm10_1_cv_st31.csv", head = T, sep =","))
pm10_1_cv_s27 <- as.data.frame(read.csv("subset_data/pm10_1_cv_st27.csv", head = T, sep =","))
pm10_1_cv_s16_27_32 <- as.data.frame(read.csv("subset_data/pm10_1_cv_st16_27_32.csv", head = T, sep =","))

##############################################
##importing subsetted dataset for PM10 for data attribute of STFDF object
pm1010_t <- as.data.frame(read.csv("subset_data/pm10_only_data_transposed.csv", head = T, sep =","))
pm1010_t <- as.data.frame(t(pm1010_t))

pm1010_t_cv_16 <- as.data.frame(read.csv("subset_data/pm10_only_data_transposed_cv_st16.csv", head = T, sep =","))
pm1010_t_cv_16 <- as.data.frame(t(pm1010_t_cv_16))

pm1010_t_cv_32 <- as.data.frame(read.csv("subset_data/pm10_only_data_transposed_cv_st32.csv", head = T, sep =","))
pm1010_t_cv_32 <- as.data.frame(t(pm1010_t_cv_32))

pm1010_t_cv_27 <- as.data.frame(read.csv("subset_data/pm10_only_data_transposed_cv_st27.csv", head = T, sep =","))
pm1010_t_cv_27 <- as.data.frame(t(pm1010_t_cv_27))

pm1010_t_cv_16_27_32 <- as.data.frame(read.csv("subset_data/pm10_only_data_transposed_cv_st16_32_27.csv", head = T, sep =","))
pm1010_t_cv_16_27_32 <- as.data.frame(t(pm1010_t_cv_16_27_32))


#############################################
#pm101 <- as.data.frame(tFrame(pm101))
pm251 <- read.csv("subset_data/pm2.5.csv", head = T, sep =",")
pm1001 <- read.csv("subset_data/pm10.csv", head = T, sep =",")

pm1 <- read.csv("subset_data/pm1.csv", head = T, sep =",")
pm12 <- as.data.frame(tFrame(pm1))
names(pm12) <- gsub("V", "Station", names(pm1))
pm2.5 <- read.csv("subset_data/pm2.5.csv", head = T, sep =",")
pm2.5 <- as.data.frame(tFrame(pm2.5))
names(pm2.5) <- gsub("V", "Station", names(pm2.5))
pm10 <- read.csv("subset_data/pm10.csv", head = T, sep =",")
pm10 <- as.data.frame(tFrame(pm10))
names(pm10) <- gsub("V", "Station", names(pm10))
##############################################

#############################################
#Creating a STFDF Object
#############################################

#Creating SP Objects
sp_new = cbind(x = c(pm10_1$lon), y = c(pm10_1$lat))
sp2 = SpatialPoints(sp_new)

sp_new_cv_s16 = cbind(x = c(pm10_1_cv_s16$lon), y = c(pm10_1_cv_s16$lat))
sp2_16 = SpatialPoints(sp_new_cv_s16)

sp_new_cv_s32 = cbind(x = c(pm10_1_cv_s32$lon), y = c(pm10_1_cv_s32$lat))
sp2_32 = SpatialPoints(sp_new_cv_s32)

sp_new_cv_s27 = cbind(x = c(pm10_1_cv_s27$lon), y = c(pm10_1_cv_s27$lat))
sp2_27 = SpatialPoints(sp_new_cv_s27)

sp_new_cv_s16_27_32 = cbind(x = c(pm10_1_cv_s16_27_32$lon), y = c(pm10_1_cv_s16_27_32$lat))
sp2_16_27_32 = SpatialPoints(sp_new_cv_s16_27_32)

#################################################
#sp1 = SpatialPointsDataFrame(data.frame(pm101$lon, pm101$lat), pm101)

#Creating Temporal Objects
time365_1 = as.POSIXct("2014-10-01")+3600*(seq(1,336, by=1))

#creating temporal object for cross validation ( use it one by one the 3 random stations)
time365_1_cv_1 = as.POSIXct("2014-10-01")+3600*(seq(1,304, by=1))

#creating temporal object for cross validation (use it for all 3 stations together)
time365_1_cv_3 = as.POSIXct("2014-10-01")+3600*(seq(1,240, by=1))


#time365 = as.Date(as.POSIXct("2014-10-01")+3600*(seq(0,336, by=1)))

#Creating the PM1 observations as a vector and then as a matrix
#timeIsInterval(time365) = TRUE
#mydata_new1= as.vector(t(pm101[1:32,4:339]))
#mydata_new3= as.numeric(as.vector(t(pm1[1:32,6:341])))

mydata_new2= as.vector(t(pm1010_t[,3:338]))

mydata_new2_cv_16= as.vector(t(pm1010_t_cv_16[,3:338]))

mydata_new2_cv_27= as.vector(t(pm1010_t_cv_27[,3:338]))

mydata_new2_cv_32= as.vector(t(pm1010_t_cv_32[,3:338]))

mydata_new2_cv_16_27_32= as.vector(t(pm1010_t_cv_16_27_32[,3:338]))

#mydata_new4= as.vector(a)
#a =convertColsToList(pm1010, name.list = FALSE, name.vector = FALSE,factors.as.char = FALSE, as.vector = TRUE)
#mydata_new2 = as.list(t(pm101[1:32,4:339]))
#mydata_new1 = as.data.frame(t(pm101[1:32,4:339]))
#mydata_new2 =c(unname(as.data.frame(pm101[1:32,4:339])))
#mydata1 = data.frame(value =pm101[1:32,4:339])



IDs = paste("ID",1:length(mydata_new2))

IDs_cv_16 = paste("ID",1:length(mydata_new2_cv_16))

IDs_cv_27 = paste("ID",1:length(mydata_new2_cv_27))

IDs_cv_32 = paste("ID",1:length(mydata_new2_cv_32))

IDs_cv_16_27_32 = paste("ID",1:length(mydata_new2_cv_16_27_32))

mydata2 = data.frame(value=mydata_new2, ID=IDs)

mydata2_cv_16 = data.frame(value=mydata_new2_cv_16, ID=IDs_cv_16)

mydata2_cv_27 = data.frame(value=mydata_new2_cv_27, ID=IDs_cv_27)

mydata2_cv_32 = data.frame(value=mydata_new2_cv_32, ID=IDs_cv_32)

mydata2_cv_16_27_32 = data.frame(value=mydata_new2_cv_16_27_32, ID=IDs_cv_16_27_32)
#############################################################################

#Creating the STFDF objects
st.all.obs = STFDF(sp2,time365_1,mydata2)

st.all.obs_cv_16 = STFDF(sp2_16,time365_1,mydata2_cv_16)

st.all.obs_cv_27 = STFDF(sp2_27,time365_1,mydata2_cv_27)

st.all.obs_cv_32 = STFDF(sp2_32,time365_1,mydata2_cv_32)

st.all.obs_cv_16_27_32 = STFDF(sp2_16_27_32,time365_1,mydata2_cv_16_27_32)

#Giving a CRS projection for the STFDF object

st.all.obs@sp@proj4string = CRS("+init=epsg:3035")

st.all.obs_cv_16@sp@proj4string = CRS("+init=epsg:3035")

st.all.obs_cv_27@sp@proj4string = CRS("+init=epsg:3035")

st.all.obs_cv_32@sp@proj4string = CRS("+init=epsg:3035")

st.all.obs_cv_16_27_32@sp@proj4string = CRS("+init=epsg:3035")

##############################################################################
#create a backup STFDF objects

stfdf_1 = st.all.obs
stfdf_16 = st.all.obs_cv_16
stfdf_27 = st.all.obs_cv_27
stfdf_32 = st.all.obs_cv_32
stfdf_16_27_32 = st.all.obs_cv_16_27_32

#################################################
#importing the shapefile of Eindhoven

dutch_map <- readShapePoly("C:/Users/Ipsit Dash/Documents/Module13/MiniProject/shapefile/Wijk_Buurt_projected_3035.shp")
proj4string(dutch_map) <- CRS("+init=epsg:3035")
plot(dutch_map)
#Coordinate transformtion
tmp = spTransform(dutch_map,CRS("+proj=longlat +ellps=GRS80") )


################################################
#dutch_map <- map("worldHires","Netherlands","Netherlands:Eindhoven",plot=F, fill=T)
plot(dutch_map)
#ger_map <- map("worldHires","Germany",plot=F, fill=T)
#IDs1 <- paste(dutch_map@data$WIJNAAM, 1:length(dutch_map@polygons))
#dutch <- map2SpatialPolygons(dutch_map,IDs =IDs1,CRS(proj4string(st.all.obs)))
#dutch_lines <- map2SpatialLines(dutch_map,IDs=IDs1,CRS(proj4string(st.all.obs))
#dutch <- spTransform(dutch,CRS(proj4string(st.all.obs)))
#dutch_lines <- spTransform(dutch_lines,CRS(proj4string(st.all.obs)))
#dutch_oct <- st.all.obs[dutch,"2014-10","value"]
#dutch_oct <- as(dutch_oct, "STFDF")


# check for bounding box
dutch_map@bbox
st.all.obs@sp@bbox
tmp@bbox
#dutch_grid <- SpatialGrid(GridTopology(c(5.42,51.39),c(1,1),cells.dim=c(180,150)), CRS(proj4string(st.all.obs)))
#meuse.sr = SpatialPolygons(list(Polygons(list(Polygon(dutch_map)),"dutch_map")))
#rv = list("sp.polygons", meuse.sr, fill = "lightblue")
spl1 <- list("sp.polygons", tmp, first = FALSE) 
spl2 <- list("sp.points",st.all.obs@sp, pch="+", sp2, col=2, cex=2)
spl <- list(spl1, spl2)


xy  <- expand.grid(x=seq(5.35, 5.55, by=0.015), y=seq(51.38,51.51, by=0.015))
dutch_grid <- SpatialPoints(xy)
gridded(dutch_grid) <- TRUE
proj4string(dutch_grid) <- CRS("+init=epsg:3035")
plot(dutch_grid, axes=T)
points(sp2, col=2, pch=19)

dutch_gridded <- STF(dutch_grid, st.all.obs@time[1:24])
dutch_gridded@sp@proj4string = CRS("+init=epsg:3035")
plot(dutch_gridded)
#####################################################################################
####change 1:336 - Its for all the  observations
stplot(st.all.obs[,1:2], col.regions=bpy.colors(), 
       sp.layout = list("sp.polygons", tmp),
       cuts=0:6*5, main="")

stplot(st.all.obs[,1:2],sp.layout = list("sp.polygons", tmp))
#################################################
#Spatio-temporal Variograms


#Spatial and Temporal Variograms and Pooled variogram
rs = sample(dim(st.all.obs)[2],336)
lst = lapply(rs, function(i){x = st.all.obs[,1];x$ti=i;x})
pts = do.call(rbind, lst)

v = variogram(value~ti, pts[!is.na(pts$value),],dX=0, cutoff =0.03, width =0.003)
plot(v)

vmod = fit.variogram(v,vgm(35,"Exp", 0.03,0.1))
plot(v,vmod)
v1 = variogram(value~1, st.all.obs, tlags =0:5)

plot(v1)
plot(v1, map =FALSE)


## empirical st-variogram
empVgm <- variogramST(value~1, st.all.obs)
plot(empVgm, wireframe =T, scales = list(arrows =F),col.regions=bpy.colors(),zlab=list(rot=90), main ="Empirical Variogram")
empVgm_48 <- variogramST(value~1, st.all.obs, tlags =0:48)
plot(empVgm_48, wireframe =T, scales = list(arrows =F),col.regions=bpy.colors(),zlab=list(rot=90), main = "Empirical Variogram ; time lag -0:48")

empVgm_16 <- variogramST(value~1, st.all.obs_cv_16)
plot(empVgm_16, wireframe =T, scales = list(arrows =F),col.regions=bpy.colors(),zlab=list(rot=90), main = "Emprirical Variogram (used for Cross Validation)", sub ="Station 16")

empVgm_27 <- variogramST(value~1, st.all.obs_cv_27)
plot(empVgm_27, wireframe =T, scales = list(arrows =F),col.regions=bpy.colors(),zlab=list(rot=90),main = "Emprirical Variogram (used for Cross Validation)", sub ="Station 27")

empVgm_32 <- variogramST(value~1, st.all.obs_cv_32)
plot(empVgm_32, wireframe =T, scales = list(arrows =F),col.regions=bpy.colors(),zlab=list(rot=90),main = "Emprirical Variogram (used for Cross Validation)", sub ="Station 32")

empVgm_16_27_32 <- variogramST(value~1, st.all.obs_cv_16_27_32)
plot(empVgm_16_27_32, wireframe =T, scales = list(arrows =F),col.regions=bpy.colors(),zlab=list(rot=90),main = "Emprirical Variogram (used for Cross Validation)", sub ="Station 16, 27, 32")

##################################################


#Fitting the ST Variogram
# separable model: spatial and temporal sill will be ignored
# and kept constant at 1-nugget respectively. A joint sill is used.
separableModel <- vgmST("separable", 
                        space=vgm(0.9,"Exp", 147, 0.1),
                        time =vgm(0.9,"Exp", 3.5, 0.1),
                        sill=40)

# product sum model: spatial and temporal nugget will be ignored and kept
# constant at 0. Only a joint nugget is used.
prodSumModel <- vgmST("productSum",
                      space=vgm(39, "Sph", 343, 0),
                      time= vgm(36, "Exp",   3, 0), 
                      sill=41, nugget=17)

# sum metric model: spatial, temporal and joint nugget will be estimated
sumMetricModel <- vgmST("sumMetric",
                        space=vgm( 6.9, "Lin", 200, 3.0),
                        time =vgm(10.3, "Lin",  15, 3.6),
                        joint=vgm(37.2, "Exp",  84,11.7),
                        stAni=77.7)

# simplified sumMetric model, only a overall nugget is fitted. The spatial, 
# temporal and jont nuggets are set to 0.
simpleSumMetricModel <- vgmST("simpleSumMetric",
                              space=vgm(20,"Lin", 150, 0),
                              time =vgm(20,"Lin", 10,  0),
                              joint=vgm(20,"Exp", 150, 0),
                              nugget=1, stAni=15)


## metric model and Fitting with empVgm
metricModel <- vgmST("metric", 
                     joint=vgm(8,"Exp", 122, 31),
                     stAni=330)


separableModel <- vgmST("separable", 
                        space=vgm(0.3,"Exp", 100, 0.7),
                        time =vgm(0.04,"Exp", 22, 0.9),
                        sill=50)

metricFit <- fit.StVariogram(empVgm,metricModel, method = "L-BFGS-B")
metricFit_48 <- fit.StVariogram(empVgm_48,metricModel)
metricFit_spatial <- fit.StVariogram(v1,metricModel)


separableFit <- fit.StVariogram(empVgm,separableModel, method ="L-BFGS-B")

separableFit <- fit.StVariogram(empVgm,separableModel, fit.method =3)

attr(metricFit,"optim.output")$value # 17.06
attr(metricFit_48,"optim.output")$value # 20.61
attr(metricFit_spatial,"optim.output")$value # 15.11
attr(separableFit,"optim.output")$value # 17.04



X11()
plot(empVgm, metricFit)
X11()
plot(empVgm, metricFit, wireframe=TRUE, all=TRUE)
X11()
plot(empVgm, separableFit, wireframe=TRUE, all=TRUE)

###############################################
# prediction
predMetric <- krigeST(value~1, st.all.obs, 
                       dutch_gridded, 
                       metricFit)

stplot(predMetric)
stplot(predMetric,col.regions=bpy.colors(25), at=0:25,
        sp.layout=list("sp.points",ger_june[,15],
                       pch=1,col="black",cex=2))
# 
# spplot(predMetric[,2],col.regions=bpy.colors(25), at=0:25,
#        sp.layout=list("sp.points",ger_june[bools,15],fill=cols_sub,pch=22,col="black",cex=2),
#        main=paste("day",15))



#############################################################################################################




#take 3 random stations one by one- remove all the observations pertaining to that station and then compare the observation and predictions..

rnd_stn_1 =round(32*runif(1)+0.5,0)

rnd_stn_2 =round(32*runif(1)+0.5,0)

rnd_stn_3 =round(32*runif(1)+0.5,0)

#converting the dataset to a timeseries- Just for visualization purposes
data1 <- as.ts(aball)

X11()
par(mfrow = c(2,1))
plot.ts(data1[1:1,5:340], plot.type= c("multiple"))
plot.ts(data1[2:2,5:340], plot.type= c("multiple"))
##### Getting the Map of Eindhoven
