require(MASS)


# ----------------------------------------------------------------------
# Perfect State Information Linear Quadratic Controller
# ----------------------------------------------------------------------
#' perfect.info.lqr
#' 
#' Steer the system towards a target state, optimizing for quadratic loss.
#'
#' @param target (numeric matrix) Target state at each iteration (equivalent to 1 min)
#' @param params (list) Deterministic model parameters. Consists of symmetric weighting matrices \code{Q} and \code{R}.
#' @param noise.model (list) Model from which the noise is drawn, stochastic part of AR1 model. Consists of a function \code{draw} that returns draws and the parameters \code{mean} and \code{var} of the model.
#' @param AR_coef (matrix) Coefficients to build the deterministic part of a AR1 model 
#' @param WR (vector) Vector ratio to transform from wind velocity to shift in position during a 1 min interval
#' @param w_ini Initial Wind speed in time 0 (m/s)
#' @return A list with following elements: target, state, controls, noise, loss
#' @export
#' @import MASS
#' @examples
#' # set target trajectory
#' target <- matrix(c(1, 1, 1, -1, -1, -1, -1, 1, 1, 1),
#'     nrow = 5,
#'     ncol = 2,
#'     byrow = TRUE
#'     )
#'
#' # run
#' sim <- perfect.info.lqr(
#'     target,
#'     list(Q = diag(1, 2, 2), R = diag(0, 2, 2)),
#'     list(draw = gaussian.noise, mean = rep(0, 2), var = diag(0.01, 2, 2))
#'     )
#'

perfect.info.lqr <- function(target, params, noise.model,AR_coef,WR=0,w_ini=0) {
    
    # math
    .policy <- function(x, K, Q, R, mean.noise, WR)
        -solve(R + K) %*% K %*% (x + WR*mean.noise)
    .dynamics <- function(x, u, w, WR)
        x + u + WR* w
    .riccati.eqn <- function(K, Q, R)
        K -  K %*% solve(K + R) %*% K + Q
    .loss <- function(X, U, Q, R)
        sum(apply(X, 1, function(x) x %*% Q %*% x)) + sum(apply(U, 1, function(u) u %*% R %*% u))

    # preprocess
    niter <- dim(target)[1]
    dims <- dim(target)[2]
    target <- target
    state <- matrix(nrow = niter, ncol = dims)
    state[1,] <- target[1,]
    controls <- matrix(nrow = niter - 1, ncol = dims)
    noise <- noise.model$draw(niter - 1, noise.model$mean, noise.model$var)
    params$K <- list(params$Q)
    for (i in (2:niter))
        params$K[[i]] <- .riccati.eqn(params$K[[i-1]], params$Q, params$R)
    wind<-matrix(nrow = niter, ncol = dims)
    wind[1,]<-w_ini
        
    # run simulation
    for (i in 1:(niter - 1)) {
        # expected estimate of wind (expected value of stochastic part is 0)
        wind[i+1,]<- AR_coef[1,]+AR_coef[2,]*wind[i,1]+AR_coef[3,]*wind[i,2]
                    
        # compute optimal step
        controls[i,] <- .policy(
            state[i,] - target[i + 1,],
            params$K[[niter - i]],
            params$Q,
            params$R,
            wind[i+1,], #deterministic part of wind AR1
            WR
            )

        # simulate next waypoint
        # here change to real values of wind if we want to see what happened really
        # realization of wind: deterministic + stochastic
        wind[i+1,]<- wind[i+1,]+noise[i,]
        state[i + 1,] <- .dynamics(state[i,], controls[i,], 
                wind[i+1], 
                WR)
        
    }

    # compute loss
    loss <- .loss(target - state, controls, params$Q, params$R)

    return(list(
        target = target,
        state = state,
        controls = controls,
        noise = noise,
        loss = loss
        ))
}


gaussian.noise <- function(n, mean, var) {
    return(mvrnorm(n, mean, var))
}





# TESTING CODE
target <- matrix(
    c(1, 1, 1, -1, -1, -1, -1, 1, 1, 1),
    nrow = 5,
    ncol = 2,
    byrow = TRUE
    )

sim <- perfect.info.lqr(
    target,
    list(Q = diag(1, 2, 2), R = diag(0, 2, 2)),
    list(draw = gaussian.noise, mean = rep(0, 2), var = diag(0.01, 2, 2)),
    matrix(0,3,2)
    )


# TESTING with real NY wind covariance model (still autoregressive AR1 not included)
cov<- as.matrix(read.csv("cov_wind_residuals.csv")[,2:3]) # To build the gaussian shocks
coefs<-as.matrix(read.csv("coefs_AR1_wind.csv")[,2:3]) # coefs of the AR1 model
real_wind<-read.csv("CPNY_wind_NYeve.csv",stringsAsFactors =F)
index<-which(real_wind$date=="2009-12-31 23:30:00")
real_wind<-as.matrix(real_wind[index:(index+60),3:4]) #wind Central Park NYeve
W<-rep(60*0.2,2) # 20% ratio shift, applied during 60 s
# wx column: linear model wind speed prediction axis x in m/s according to wx(t-1) and wy(t-1)
# wy column: same but axis y
# Note the autorregressive parameters are persistant in the same axis basically

# Central Park Rectangle, UTM 18T coordinates, starting at southern point and going clockwise
target <- matrix(
    c(586673.4, 4513101.97, 
      585969.8, 4513512.3,
      587886.85, 4517117.2,
      588637.65, 4516736.65,
      586673.4, 4513101.97),
    nrow = 5,
    ncol = 2,
    byrow = TRUE
    )

sim <- perfect.info.lqr(
    target,
    list(Q = diag(1, 2, 2), R = diag(0, 2, 2)),
    list(draw = gaussian.noise, mean = rep(0,2), var = cov),
    coefs,
    W,
    real_wind[1,]
    )

plot(sim$target,type="l",col="blue")
lines(sim$state,type="l",col="red")


