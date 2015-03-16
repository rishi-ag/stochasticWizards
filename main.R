source("lib/controller.R")
source("lib/stochastics.R")
source("lib/viz.R")
source("lib/real_path.R")


# PERFECT STATE TESTING

target <- matrix(
    c(100, 100, 100,
      100, -100, 200,
      -100, -100, 300,
      -100, 100, 200,
      100, 100, 100),
    nrow = 5,
    ncol = 3,
    byrow = TRUE
)

Q <- diag(1, 3, 3)
R <- diag(0, 3, 3)
n <- dim(target)[1]
real_wind <- read.csv("data/CPNY_wind_NYmacey.csv",stringsAsFactors =F)
index <- which(real_wind$date=="2009-11-26 12:00:00")
real_wind <- as.matrix(real_wind[index:(index+n),4:5])
real_wind<- cbind(real_wind,rep(0,n+1))
wind_ini <- real_wind[1,]

sim <- perfect.info.lqr(
    target,
    list(Q = Q, R = R),
    ny.wind.model(n, wind.ini=wind_ini)
    )


# IMPERFECT STATE LQR TESTING

target <- matrix(
    c(100, 100, 100,
      100, -100, 200,
      -100, -100, 300,
      -100, 100, 200,
      100, 100, 100),
    nrow = 5,
    ncol = 3,
    byrow = TRUE
)

Q <- diag(1, 3, 3)
R <- diag(0, 3, 3)
n<-dim(target)[1]
real_wind<-read.csv("data/CPNY_wind_NYmacey.csv",stringsAsFactors =F)
index<-which(real_wind$date=="2009-11-26 12:00:00")
real_wind<-as.matrix(real_wind[index:(index+n),4:5])
real_wind<-cbind(real_wind,rep(0,n+1))
wind_ini<-real_wind[1,]

sim <- imperfect.info.lqr(
    target,
    list(Q = Q, R = R),
    ny.wind.model(n, wind.ini=wind_ini),
    gps.model(n)
    )


# other stuff

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

Q <- diag(1, 3, 3)
R <- diag(0, 3, 3)
n<-dim(target)[1]
real_wind<-read.csv("data/CPNY_wind_NYmacey.csv",stringsAsFactors =F)
index<-which(real_wind$date=="2009-11-26 12:00:00")
real_wind<-as.matrix(real_wind[index:(index+n),4:5])
real_wind<-cbind(real_wind,rep(0,n+1))
wind_ini<-real_wind[1,]


#Retrieving real wind CPNY macey's day 2009
n<-dim(target)[1]
real_wind<-read.csv("data/CPNY_wind_NYmacey.csv",stringsAsFactors =F)
index<-which(real_wind$date=="2009-11-26 12:00:00")
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
type<-c("null","fixed","simulated_det","simulated","online_simulated")

#Smmary vectors
sim_loss<-rep(NA,length(type))
real_loss<-rep(NA,length(type))

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
    text(585900,4517000,paste0('Real loss: ',round(real$loss),'\nSim loss: ',round(sim$loss)),cex=0.6,pos=4)

    sim_loss[i]<-sim$loss
    real_loss[i]<-real$loss
}
#plot summary
barplot(real_loss-sim_loss,names.arg=type,main="Losses by type",cex.names=0.7,
        col=rainbow(length(type)))

# Now Monte-carlo simulations for 'simulated' and 'online simulated'
Nsim<-1000
index<-which(type %in% c('simulated','online_simulated'),arr.ind=T)
for (i in index[1]:index[2]){
    sim_loss[i]<-0
    real_loss[i]<-0
    for (j in 1:Nsim){
        # Find control
        sim <- perfect.info.lqr(
            target,
            list(Q = Q, R = R),
            ny.wind.model(n, wind.ini=wind_ini,type=type[i])
        )
        
        # Calculate real path with this control
        real<-real.path(target,sim$controls,real_wind,target[1,],Q,R)
        
        sim_loss[i]<-sim_loss[i]+sim$loss
        real_loss[i]<-real_loss[i]+real$loss
    }
    sim_loss[i]<-sim_loss[i]/Nsim
    real_loss[i]<-real_loss[i]/Nsim
}

#plot again, now averaged over stochastic types
barplot(real_loss-sim_loss,names.arg=type,main="Losses by type, Monte-Carlo",cex.names=0.7,
        col=rainbow(length(type)))


#initiate GPS covariance
get.GPS.cov()
