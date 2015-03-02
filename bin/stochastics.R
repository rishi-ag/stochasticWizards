require(MASS)


gaussian.noise <- function(n, mean, var) {
    return(mvrnorm(n, mean, var))
}
