library(MASS)
library("mvtnorm")


ar1.noise <- function(n, coefs, noise.cov,wind.ini=c(0,0,0)) {
    shocks <- mvrnorm(n, rep(0, dim(noise.cov)[1]), noise.cov)
    shocks[1,]<-wind.ini
    for (i in 2:dim(shocks)[1])
        shocks[i,] <- coefs %*% c(1, shocks[i-1,]) + shocks[i,]
    return(shocks)
}


ny.wind.model <- function(n, wind.ini) {
    cov <- rbind(cbind(read.csv("data/cov_wind_residuals.csv", row.names = 1), 0), 0) # to build the gaussian shocks
    coefs <- t(rbind(cbind(read.csv("data/coefs_AR1_wind.csv", row.names = 1), 0), 0)) # coefs of the AR1 model
    draws <- ar1.noise(n, coefs, cov, wind.ini)
    means <- t(cbind(coefs[,1], sapply(2:n, function(i) coefs %*% c(1, draws[i,]))))
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

get.GPS.cov()
#Note: get info on the format of the GPS system to use
get.gps.noise <- function(x, y, z) {
  
  sigma <- as.matrix(read.table("data/Covariance", header = T))
  coord.noise <- mvtnorm::rmvnorm(n = 1, mean = c(10,10,20), sigma = sigma)
  return (c(x + coord.noise[1], y + coord.noise[2], z + coord.noise[3]))
}
