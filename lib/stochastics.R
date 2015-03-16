library(MASS)


ar1 <- function(n, coefs, noise.cov = matrix(0, 3, 3), ini = c(0,0,0)) {
    
    shocks <- mvrnorm(n, rep(0, dim(noise.cov)[1]), noise.cov)
    if (n == 1)
        shocks <- t(shocks)
    
    shocks[1,]<-t(coefs %*% c(1, ini)) + shocks[1,]
    if (n > 1) {
        for (i in 2:n)
            shocks[i,] <- t(coefs %*% c(1, shocks[i-1,])) + shocks[i,]
    }

    means <- t(cbind(coefs %*% c(1, ini), sapply(1:(n-1), function(i) coefs %*% c(1, shocks[i,]))))
    
    return(list(draws = shocks, means = means))
}


#' ny.wind.model
#' 
#' Wind model for New York Central Park. Requires data/cov_wind_residuals.csv data/coefs_AR1_wind.csv and data/CPNY_wind_NYeve.csv to run ar1.noise function
#'
#' @param n integer Number of predicted observations to be generated
#' @param wind.ini Vector Initial wind speed (m/s) in x(east),y(north),z(vertical) directions
#' @param type Factor Type of data to generate. 'null' means all wind results set to 0, 'fixed' means all wind results set to initial value, 'simulated' means simulate an AR1 model, 'simulated_det' means AR1 model without the stochastic part,'online_simulated' mean to consider every step the real wind data and generate a 1-step ahead prediction from that value.
#' @return A list of two matrices. Draws are the stochastic draws from an AR1 model. Means are the expected values for next step, considering only the deterministic part of AR1 model
#' @export
#' @import
#' @examples
#' 
#' #Retrieving real wind CPNY new year's eve 2009-2010
#' wind_ini<-as.vector(read.csv("data/wind_ini_CPNY.csv",stringsAsFactors =F))[,2]
#'
#' # get 10 shocks
#' ny.wind.model(n=10, wind.ini=c(wind_ini,0),type="simulated")
#'
ny.wind.model <- function(n, wind.ini = c(0,0,0), type = "simulated") {
        cov <- rbind(cbind(read.csv("data/cov_wind_residuals.csv", row.names = 1), 0), 0)
        coefs <- t(rbind(cbind(read.csv("data/coefs_AR1_wind.csv", row.names = 1), 0), 0))
    
    if (type == "simulated") {
        draw <- ar1(n, coefs, noise.cov = cov, ini = wind.ini)
    } else if (type=="simulated_det") {
        draw <- ar1(n, coefs, ini = wind.ini)
    } else if (type=="fixed") { # suppose always same wind as initial
        draw <- list(
            draws = matrix(rep(wind.ini,n),nrow=n,ncol=length(wind.ini),byrow=T),
            means = matrix(rep(wind.ini,n),nrow=n,ncol=length(wind.ini),byrow=T)
            )
    } else if (type=="null") { # no wind
        draw <- list(
            draws = matrix(0,nrow=n,ncol=length(wind.ini)),
            means = matrix(0,nrow=n,ncol=length(wind.ini))
            )
        
    } else if (type=="online_simulated") {
        ### THIS IS DEAD CODE AND NEEDS TO BE ADJUSTED ###

        # real wind data affects dynamics, stochastic control
        # Retrieve real wind data
        real_wind <- read.csv("data/CPNY_wind_NYmacey.csv", stringsAsFactors = F)
        index <- which(real_wind$date=="2009-11-26 12:00:00")
        real_wind <- as.matrix(real_wind[index:(index+n-1),4:5])
        real_wind <- cbind(real_wind, rep(0, n))
        # Stochastic draws generated each step from real data, means without stochastic part
        draws <- matrix(NA, n, length(wind.ini))
        means <- matrix(NA, n, length(wind.ini))
        for (i in (1:n)) {
            draws[i,] <- ar1(1, coefs, noise.cov=cov, ini=real_wind[i,],noise=T)
            means[i,] <- ar1(1, coefs, ini=real_wind[i,])
        }
    }
    
    return(draw)
}


get.gps.noise <- function(noShocks) {
  sigma <- as.matrix(read.table("data/Covariance", header = T))
  mvrnorm(n = noShocks, mu = c(0,0,0), Sigma = sigma)
}


gps.model <- function(n) {
    return(list(draws = get.gps.noise(n)))
}
