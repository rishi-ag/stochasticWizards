library(MASS)
library("mvtnorm")


ar1.noise <- function(n, coefs, noise.cov) {
    shocks <- mvrnorm(n, rep(0, dim(noise.cov)[1]), noise.cov)
    for (i in 2:dim(shocks)[1])
        shocks[i,] <- coefs %*% c(1, shocks[i-1,]) + shocks[i,]
    return(shocks)
}


ny.wind.model <- function(n) {
    cov <- rbind(cbind(read.csv("data/cov_wind_residuals.csv", row.names = 1), 0), 0) # to build the gaussian shocks
    coefs <- t(rbind(cbind(read.csv("data/coefs_AR1_wind.csv", row.names = 1), 0), 0)) # coefs of the AR1 model
    draws <- ar1.noise(n, coefs, cov)
    means <- t(cbind(coefs[,1], sapply(2:n, function(i) coefs %*% c(1, noise[i,]))))
    return(list(draws = draws, means = means))
}



#Note: get info on the format of the GPS system to use
get.gps.noise <- function(lat, lon, alt) {
  #estimate variance covariance based on a radius of 10m uncertainity on the three corrdinates
  set.seed(1000)
  position <- data.frame(X = runif(1000000, min = -10, max = 10), 
                         Y = runif(1000000, min = -10, max = 10), 
                         Z = runif(1000000, min = -10, max = 10))
  
  sigma <- cov(as.matrix(position))
  #draw a random sample from the estimated cov
  coord.noise <- mvtnorm::rmvnorm(1, mean = c(0,0,0), sigma = sigma)
  
  #convert displacement in meters to displacement in latitude and longitude
  del.lat <- coord.noise[1] * (1/111111)
  del.lon <- coord.noise[2] / (cos(lat * pi/ 180) * 111111)
  
  return (c(lat + del.lat, lon + del.lon, alt + coord.noise[3]))
}