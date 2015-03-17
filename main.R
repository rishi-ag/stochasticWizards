source("lib/controller.R")
source("lib/stochastics.R")
source("lib/viz.R")
source("lib/plots.R")
source("lib/real_path.R")


# TESTING PARAMETERS
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


# OPEN LOOP TESTING
sim <- open.loop.lqr(
    target,
    list(Q = Q, R = R),
    function(n) ny.wind.model(n, wind.ini=wind_ini, type = "simulated_det")
)
real <- open.loop.lqr(
    target,
    list(Q = Q, R = R),
    function(n) ny.wind.model(n, wind.ini=wind_ini, type = "historical2")
)
plot.path(target,sim$state,real$state,sim_loss=sim$loss,real$loss,main="Open Loop Path")
real2<-real.path(target,sim$controls,real_wind,Q,R)
plot.path(target,sim$state,real2$path,sim_loss=sim$loss,real$loss,main="Open Loop Path")
lines(real$state,type="l",col="orange")

# PERFECT STATE TESTING
sim <- perfect.info.lqr(
    target,
    list(Q = Q, R = R),
    function(n) ny.wind.model(n, wind.ini=wind_ini)
)
real <- perfect.info.lqr(
    target,
    list(Q = Q, R = R),
    function(n) ny.wind.model(n, wind.ini=wind_ini,type="historical")
)
plot.path(target,sim$state,real$state,sim_loss=sim$loss,real$loss,main="Perfect State Path")

# IMPERFECT STATE TESTING
sim <- imperfect.info.lqr(
    target,
    list(Q = Q, R = R),
    function(n) ny.wind.model(n, wind.ini=wind_ini),
    gps.model
)
real <- imperfect.info.lqr(
    target,
    list(Q = Q, R = R),
    function(n) ny.wind.model(n, wind.ini=wind_ini,type="historical"),
    gps.model
)
plot.path(target,sim$est.state,real$est.state,sim_loss=sim$loss,real$loss,main="Imperfect State Path")

# real<-real.path(target,sim$controls,real_wind,target[1,],Q,R)

# -----------------------
## NY Macy's day analysis
# -----------------------

target <- matrix(
    c(586673.4, 4513101.97, 100,
      585969.8, 4513512.3, 100,
      587886.85, 4517117.2, 100,
      588637.65, 4516736.65, 100,
      586673.4, 4513101.97, 100),
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
# subset and add z component
real_wind<- cbind(real_wind,rep(0,n+1))
wind_ini <- real_wind[1,]

#plot wind
jpeg(filename = paste0('plots/wind.jpg'), units = "in", width = 10, height = 5, res = 400)
plot(1:(n+1),real_wind[,1],type="l",xlab="index time",ylab="m/s",main="Wind profile",
     ylim=c(min(real_wind),max(real_wind)),col="red")
lines(1:(n+1),real_wind[,2],col="blue")
legend("topright",c("X dir","Y dir"),lty=1,col=c("red","blue"),bg="#FFFFFFAA")
dev.off()


#Defining cases
runs<-c("Open loop fixed","Open loop sim_det","Perfect state fixed","Perfect state simulated",
        "Imperfect state fixed","Imperfect state simulated")
wind<-c("fixed","simulated_det","fixed","simulated","fixed","simulated")

### Single simulation, all cases ####

#Smmary vectors
sim_loss<-rep(NA,length(runs))
real_loss<-rep(NA,length(runs))

for (i in 1:length(runs)){ # Open loop cases
    if (i %in% c(1,2)){
        sim <- open.loop.lqr(
            target,
            list(Q = Q, R = R),
            function(n) ny.wind.model(n, wind.ini=wind_ini, type = wind[i])
        )
        real <- open.loop.lqr(
            target,
            list(Q = Q, R = R),
            function(n) ny.wind.model(n, wind.ini=wind_ini,type="historical")
        )
    } else if (i %in% c(3,4)){ #Perfect state cases
        sim <- perfect.info.lqr(
            target,
            list(Q = Q, R = R),
            function(n) ny.wind.model(n, wind.ini=wind_ini, type = wind[i])
        )
        real <- perfect.info.lqr(
            target,
            list(Q = Q, R = R),
            function(n) ny.wind.model(n, wind.ini=wind_ini,type="historical")
        )
    } else { # Imperfect state cases
        sim <- imperfect.info.lqr(
            target,
            list(Q = Q, R = R),
            function(n) ny.wind.model(n, wind.ini=wind_ini, type = wind[i]),
            gps.model
        )
        real <- imperfect.info.lqr(
            target,
            list(Q = Q, R = R),
            function(n) ny.wind.model(n, wind.ini=wind_ini,type="historical"),
            gps.model
        ) 
        sim$state<-sim$est.state
        real$state<-real$est.state
    }
    sim_loss[i]<-sim$loss
    real_loss[i]<-real$loss
    
    #save plot
    jpeg(filename = paste0('plots/',runs[i],'.jpg'), units = "in", width = 5, height = 5, res = 400)
    plot.path(target,sim$state,real$state,sim_loss[i],real_loss[i],main=runs[i])
    dev.off()    
}


#plot summary
jpeg(filename = paste0('plots/summary1run.jpg'), units = "in", width = 11, height = 5, res = 400)
plot.bars(sim_loss,real_loss,runs,"Losses")
dev.off()


### Monte-Carlo simulation (for stochastic ones), all cases ####

Nsim<-1000

for (i in c(2,3,4,5)){ # stochastic components, i.e. all but open loop fixed and perfect state fixed
    sim_loss[i]<-0
    real_loss[i]<-0
    for (j in 1:Nsim){
        if (i %in% c(1,2)){
            sim <- open.loop.lqr(
                target,
                list(Q = Q, R = R),
                function(n) ny.wind.model(n, wind.ini=wind_ini, type = wind[i])
            )
            real <- open.loop.lqr(
                target,
                list(Q = Q, R = R),
                function(n) ny.wind.model(n, wind.ini=wind_ini,type="historical")
            )
        } else if (i %in% c(3,4)){ #Perfect state cases
            sim <- perfect.info.lqr(
                target,
                list(Q = Q, R = R),
                function(n) ny.wind.model(n, wind.ini=wind_ini, type = wind[i])
            )
            real <- perfect.info.lqr(
                target,
                list(Q = Q, R = R),
                function(n) ny.wind.model(n, wind.ini=wind_ini,type="historical")
            )
        } else { # Imperfect state cases
            sim <- imperfect.info.lqr(
                target,
                list(Q = Q, R = R),
                function(n) ny.wind.model(n, wind.ini=wind_ini, type = wind[i]),
                gps.model
            )
            real <- imperfect.info.lqr(
                target,
                list(Q = Q, R = R),
                function(n) ny.wind.model(n, wind.ini=wind_ini,type="historical"),
                gps.model
            ) 
        }    
        sim_loss[i]<-sim_loss[i]+sim$loss
        real_loss[i]<-real_loss[i]+real$loss
    }
    sim_loss[i]<-sim_loss[i]/Nsim
    real_loss[i]<-real_loss[i]/Nsim
}

#plot summary
jpeg(filename = paste0('plots/summaryMtCarlo.jpg'), units = "in", width = 11, height = 5, res = 400)
plot.bars(sim_loss,real_loss,runs,"Monte-Carlo Losses")
dev.off()

#initiate GPS covariance
get.GPS.cov()
