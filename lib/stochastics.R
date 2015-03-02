library(MASS)


ar1.noise <- function(n, coefs, noise.cov) {
    shocks <- mvrnorm(n, rep(0, dim(noise.cov)[1]), noise.cov)
    for (i in 2:dim(shocks)[1])
        shocks[i,] <- coefs %*% c(1, shocks[i-1,]) + shocks[i,]
    return(shocks)
}


ny.wind.model <- function(n) {
    cov <- rbind(cbind(read.csv("../data/cov_wind_residuals.csv", row.names = 1), 0), 0) # to build the gaussian shocks
    coefs <- t(rbind(cbind(read.csv("../data/coefs_AR1_wind.csv", row.names = 1), 0), 0)) # coefs of the AR1 model
    draws <- ar1.noise(n, coefs, cov)
    means <- t(cbind(coefs[,1], sapply(2:n, function(i) coefs %*% c(1, noise[i,]))))
    return(list(draws = draws, means = means))
}
