#standard block in manthattan is 80 m (st block) × 274 m (avenue part)
#walking speed is assumed to be 80m/min (4km/hr)
source("parade_coordinates.R")
target<-parade
wait_points<-parade

xy <- data.frame(ID = 1:nrow(wait_points), X = wait_points[1:nrow(wait_points),1], Y = wait_points[1:nrow(wait_points),2])
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example

target <- as.data.frame(spTransform(xy, CRS("+proj=utm +zone=18 ellps=WGS84")))


target<- as.matrix(data.frame(target[,c("X","Y")],Z=15))
#target[,2] <- target[,2]+0.5

#Retrieving real wind CPNY macey's day 2009
n<-dim(target)[1]
real_wind<-read.csv("data/CPNY_wind_NYmacey.csv",stringsAsFactors =F)
index<-which(real_wind$date=="2009-11-26 12:00:00")
real_wind<-as.matrix(real_wind[index:(index+n),4:5])
real_wind<-cbind(real_wind,rep(0,n+1))
wind_ini<-real_wind[1,]

#Defining loss matrices
Q <- diag(1, 3, 3)
R <- diag(0, 3, 3)
type<-c("null","fixed","simulated_det","simulated","online_simulated")

#Smmary vectors
sim_loss<-rep(NA,length(type))
real_loss<-rep(NA,length(type))
