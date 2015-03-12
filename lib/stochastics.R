library(MASS)
library("mvtnorm")

#' ar1.noise
#' 
#' Autorregressive model of lag 1, three variables
#'
#' @param n integer Number of predicted observations to be generated
#' @param coefs matrix 2x4 matrix of coefficients for the autorregressive model (three variables plus intercept), including intercept.
#' @param noise.cov Matrix Covariance matrix of the three values to generate gaussian shocks
#' @param ini Vector (length 3) Initial value 
#' @return A matrix of n shocks from the model
#' @export
#' @import
#' @examples
#' 
#' coefs<- rbind(c(1.1,0.5,0.2,0),c(1.3,0.2,0.6,0),c(0,0,0,0))
#' noise.cov<-diag(2,3,3)
#' ini<-c(1.1,0.4,0)
#' # get 10 shocks
#' shocks <- ar1.noise(10, coefs,noise.cov, ini)
#'
ar1.noise <- function(n, coefs, noise.cov,ini=c(0,0,0)) {
    shocks <- mvrnorm(n, rep(0, dim(noise.cov)[1]), noise.cov)
    shocks[1,]<-ini
    for (i in 2:dim(shocks)[1])
        shocks[i,] <- coefs %*% c(1, shocks[i-1,]) + shocks[i,]
    return(shocks)
}

#' ny.wind.model
#' 
#' Wind model for New York Central Park. Requires data/cov_wind_residuals.csv data/coefs_AR1_wind.csv and data/CPNY_wind_NYeve.csv to run ar1.noise function
#'
#' @param n integer Number of predicted observations to be generated
#' @param wind.ini Vector Initial wind speed (m/s) in x(east),y(north),z(vertical) directions
#' @param type Factor Type of data to generate. 'null' means all wind results set to 0, 'fixed' means all wind results set to initial value, 'simulated' means simulate an AR1 model, 'online_simulated' mean to consider every step the real wind data and generate a 1-step ahead prediction from that value.
#' @return A list of two matrices. Draws are the stochastic draws from an AR1 model. Means are the expected values for next step, considering only the deterministic part of AR1 model
#' @export
#' @import
#' @examples
#' 
#' #Retrieving real wind CPNY new year's eve 2009-2010
#' wind_ini<-as.vector(read.csv("data/wind_ini_CPNY.csv",stringsAsFactors =F))[,2]

#' # get 10 shocks
#' ny.wind.model(n=10, wind.ini=c(wind_ini,0),type="simulated")
#'
ny.wind.model <- function(n, wind.ini=c(0,0,0),type="simulated") {
    draws<-rep(NA,n)
    means<-rep(NA,n)
    if (type=="simulated"){ #AR1 shocks
        cov <- rbind(cbind(read.csv("data/cov_wind_residuals.csv", row.names = 1), 0), 0) # to build the gaussian shocks
        coefs <- t(rbind(cbind(read.csv("data/coefs_AR1_wind.csv", row.names = 1), 0), 0)) # coefs of the AR1 model
        draws <- ar1.noise(n, coefs, cov, wind.ini)
        means <- t(cbind(wind.ini, sapply(2:n, function(i) coefs %*% c(1, draws[i,]))))
    } else if (type=="fixed"){ # suppose always same wind as initial
        draws<-matrix(rep(wind.ini,n),nrow=n,ncol=length(wind.ini),byrow=T)
        means<-matrix(rep(wind.ini,n),nrow=n,ncol=length(wind.ini),byrow=T)
    } else if (type=="null"){ # no wind
        draws<-matrix(0,nrow=n,ncol=length(wind.ini))
        means<-matrix(0,nrow=n,ncol=length(wind.ini))
    } else if (type=="online_simulated"){ # real wind data affects dynamics, stochastic control
        # Retrieve real wind data
        real_wind<-read.csv("data/CPNY_wind_NYeve.csv",stringsAsFactors =F)
        index<-which(real_wind$date=="2009-12-31 10:00:00")
        real_wind<-as.matrix(real_wind[index:(index+n-1),3:4])
        real_wind<-cbind(real_wind,rep(0,n))
        # Retrieve covariance matrix and AR1 coefficients
        cov <- rbind(cbind(read.csv("data/cov_wind_residuals.csv", row.names = 1), 0), 0) # to build the gaussian shocks
        coefs <- t(rbind(cbind(read.csv("data/coefs_AR1_wind.csv", row.names = 1), 0), 0)) # coefs of the AR1 model
        # Stochastic draws generated each step from real data
        for (i in (1:n)){
            draws[i] <- ar1.noise(n, coefs, cov, real_wind[i])
        }
        # Means generated from expected value considering draws who already consider actual wind    
        means <- t(cbind(wind.ini, sapply(2:n, function(i) coefs %*% c(1, draws[i,]))))
        
    } 
    
    return(list(draws = draws, means = means))
}



#Calculates GPS uncertainity cov matrix and writes it to data/covariance
get.GPS.cov <- function() {
  
  set.seed(1000)
  position <- data.frame(X = rnorm(1000000, mean = 10, sd = 1), 
                         Y = rnorm(1000000, mean = 10, sd = 1), 
                         Z = rnorm(1000000, mean = 20, sd = 2))
  sigma <- cov(as.matrix(position))
  write.table(x = sigma, file = "data/Covariance")
}

get.gps.noise <- function(type) {
  #GPS uncertainity is persistant. Only case to be added is when we dont want it
  #altogether
  
  sigma <- as.matrix(read.table("data/Covariance", header = T))
  coord.noise <- c(0,0,0)
  if(type != "no_GPS"){
    coord.noise <- mvtnorm::rmvnorm(n = 1, mean = c(10,10,20), sigma = sigma)
    
  }
  return(coord.noise)
}