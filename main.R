source("bin/controller.R")
source("bin/stochastics.R")
source("bin/viz.R")


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

sim <- perfect.info.lqr(
    target,
    list(Q = diag(1, 3, 3), R = diag(0, 3, 3)),
    ny.wind.model
)

plot(sim$target,type="l",col="blue")
lines(sim$state,type="l",col="red")
