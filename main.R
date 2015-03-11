source("lib/controller.R")
source("lib/stochastics.R")
source("lib/viz.R")


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

#Retrieving real wind CPNY new year's eve 2009-2010
wind_ini<-as.vector(read.csv("data/wind_ini_CPNY.csv",stringsAsFactors =F))[,2]


sim <- perfect.info.lqr(
    target,
    list(Q = diag(1, 3, 3), R = diag(0, 3, 3)),
    ny.wind.model(n=dim(target)[1], wind.ini=c(wind_ini,0),type="stochastic")
)

plot(sim$target,type="l",col="blue")
lines(sim$state,type="l",col="red")
