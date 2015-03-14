source("lib/controller.R")
source("lib/stochastics.R")
source("lib/viz.R")
source("lib/real_path.R")

# TESTING

# Central Park Rectangle, UTM 18T coordinates, starting at southern point and going clockwise
target <- matrix(
    c(586673.4, 4513101.97, 10,
      585969.8, 4513512.3, 10,
      587886.85, 4517117.2, 10,
      588637.65, 4516736.65, 10,
      586673.4, 4513101.97, 10),
    nrow = 5,
    ncol = 3,
    byrow = TRUE
)

#Retrieving real wind CPNY macey's day 2009
n<-dim(target)[1]
real_wind<-read.csv("data/CPNY_wind_NYeve.csv",stringsAsFactors =F)
index<-which(real_wind$date=="2009-12-31 10:00:00")
real_wind<-as.matrix(real_wind[index:(index+n),4:5])
real_wind<-cbind(real_wind,rep(0,n+1))
wind_ini<-real_wind[1,]

#plot wind
plot(1:(n+1),real_wind[,1],type="l",xlab="index time",ylab="m/s",main="Wind profile",
     ylim=c(min(real_wind),max(real_wind)),col="red")
lines(1:(n+1),real_wind[,2],col="blue")
legend("topright",c("X dir","Y dir"),lty=1,col=c("red","blue"),bg="#FFFFFFAA")

#Defining loss matrices
Q <- diag(1, 3, 3)
R <- diag(0, 3, 3)
type<-c("simulated","simulated_det","fixed","null","online_simulated")

#Check all types
for (i in 1:length(type)){
    # Find control
    sim <- perfect.info.lqr(
        target,
        list(Q = Q, R = R),
        ny.wind.model(n, wind.ini=wind_ini,type=type[i])
    )
    
    # Calculate real path with this control
    real<-real.path(target,sim$controls,real_wind,target[1,],Q,R)
    
    plot(sim$target,type="l",col="blue",main=type[i],xlab="X coordinates",ylab="Y coordinates")
    lines(sim$state,type="l",col="red")
    lines(real$path,type="l",col="green")
    legend("bottomright",legend=c("target","simulated","real"),lty=1,col=c("blue","red","green"))
    text(585800,4517000,paste0('Real loss: ',round(real$loss),'\nSim loss: ',round(sim$loss)),cex=0.6,pos=4)
}


#initiate GPS covariance
get.GPS.cov()