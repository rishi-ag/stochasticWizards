library(MASS)
library("mvtnorm")


ar1.noise <- function(n, coefs, noise.cov,wind.ini=c(0,0,0)) {
    shocks <- mvrnorm(n, rep(0, dim(noise.cov)[1]), noise.cov)
    shocks[1,]<-wind.ini
    for (i in 2:dim(shocks)[1])
        shocks[i,] <- coefs %*% c(1, shocks[i-1,]) + shocks[i,]
    return(shocks)
}


ny.wind.model <- function(n, wind.ini=c(0,0,0),type="stochastic") {
    if (type=="stochastic"){ #AR1 shocks
        cov <- rbind(cbind(read.csv("data/cov_wind_residuals.csv", row.names = 1), 0), 0) # to build the gaussian shocks
        coefs <- t(rbind(cbind(read.csv("data/coefs_AR1_wind.csv", row.names = 1), 0), 0)) # coefs of the AR1 model
        draws <- ar1.noise(n, coefs, cov, wind.ini)
        means <- t(cbind(wind.ini, sapply(2:n, function(i) coefs %*% c(1, draws[i,]))))
    } else if (type=="deterministic"){ # suppose always same wind as initial
        draws<-matrix(rep(wind.ini,n),nrow=n,ncol=length(wind.ini),byrow=T)
        means<-matrix(rep(wind.ini,n),nrow=n,ncol=length(wind.ini),byrow=T)
    } else if (type=="no_wind"){
        draws<-matrix(0,nrow=n,ncol=length(wind.ini))
        means<-matrix(0,nrow=n,ncol=length(wind.ini))
    } else if (type=="real_stochastic"){ # real wind data affects dynamics, stochastic control
        # Stochastic means to generate control
        cov <- rbind(cbind(read.csv("data/cov_wind_residuals.csv", row.names = 1), 0), 0) # to build the gaussian shocks
        coefs <- t(rbind(cbind(read.csv("data/coefs_AR1_wind.csv", row.names = 1), 0), 0)) # coefs of the AR1 model
        draws <- ar1.noise(n, coefs, cov, wind.ini)
        means <- t(cbind(wind.ini, sapply(2:n, function(i) coefs %*% c(1, draws[i,]))))
        # Real wind that affects dynamics (overwrite draws, not means)
        real_wind<-read.csv("data/CPNY_wind_NYeve.csv",stringsAsFactors =F)
        index<-which(real_wind$date=="2009-12-31 23:50:00")
        real_wind<-as.matrix(real_wind[index:(index+n-1),3:4])
        real_wind<-cbind(real_wind,rep(0,n))
        draws<-real_wind
    } else if (type=="real_deterministic"){ # real wind data affects dynamics, deterministic control
        # Control only with initial wind
        means<-matrix(rep(wind.ini,n),nrow=n,ncol=length(wind.ini),byrow=T)
        # Real wind that affects dynamics
        real_wind<-read.csv("data/CPNY_wind_NYeve.csv",stringsAsFactors =F)
        index<-which(real_wind$date=="2009-12-31 23:45:00")
        real_wind<-as.matrix(real_wind[index:(index+n-1),3:4])
        real_wind<-cbind(real_wind,rep(0,n))
        draws<-real_wind
    } else if (type=="real_no_wind"){
        # Control as if there was no wind
        means<-matrix(0,nrow=n,ncol=length(wind.ini))
        # Effect of real wind on dynamics
        real_wind<-read.csv("data/CPNY_wind_NYeve.csv",stringsAsFactors =F)
        index<-which(real_wind$date=="2009-12-31 23:45:00")
        real_wind<-as.matrix(real_wind[index:(index+n-1),3:4])
        real_wind<-cbind(real_wind,rep(0,n))
        draws<-real_wind
    }
        
    return(list(draws = draws, means = means))
}



#Calculates GPS uncertainity cov matrix and writes it to data/covariance
get.GPS.cov <- function() {
  
  set.seed(1000)
  position <- data.frame(X = rnorm(1000000, mean = 0, sd = 5), 
                         Y = rnorm(1000000, mean = 0, sd = 5), 
                         Z = rnorm(1000000, mean = 0, sd = 9))
  sigma <- cov(as.matrix(position))
  write.table(x = sigma, file = "data/Covariance")
  sigma
}

get.gps.noise <- function(noShocks) {
  sigma <- as.matrix(read.table("data/Covariance", header = T))
  mvtnorm::rmvnorm(n = noShocks, mean = c(10,10,20), sigma = sigma)
}