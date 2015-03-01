require(MASS)


# ----------------------------------------------------------------------
# Perfect State Information Linear Quadratic Controller
# ----------------------------------------------------------------------
#' perfect.info.lqr
#' 
#' Steer the system towards a target state, optimizing for quadratic loss.
#'
#' @param target (numeric matrix) Target state at each iteration.
#' @param params (list) Deterministic model parameters. Consists of symmetric weighting matrices \code{Q} and \code{R}.
#' @param noise.model (list) Model from which the noise is drawn. Consists of a function \code{draw} that returns draws and the parameters \code{mean} and \code{var} of the model.
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

perfect.info.lqr <- function(target, params, noise.model) {

    # math
    .policy <- function(x, K, Q, R, mean.noise)
        -solve(R + K) %*% K %*% (x + mean.noise)
    .dynamics <- function(x, u, w)
        x + u + w
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
    
    # run simulation
    for (i in 1:(niter - 1)) {

        # compute optimal step
        controls[i,] <- .policy(
            state[i,] - target[i + 1,],
            params$K[[niter - i]],
            params$Q,
            params$R,
            noise.model$mean
            )

        # simulate next waypoint
        state[i + 1,] <- .dynamics(state[i,], controls[i,], noise[i,])
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
    list(draw = gaussian.noise, mean = rep(0, 2), var = diag(0.01, 2, 2))
    )


# TESTING with real NY wind covariance model (still autoregressive AR1 not included)
cov<- read.csv(cov_resid,"cov_wind_residuals.csv") # To build the gaussian shocks
coefs<-read.csv(coefs,"coefs_AR1_wind.csv") # coefs of the AR1 model
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
    list(draw = gaussian.noise, mean = rep(0, 2), var = cov)
)

plot(sim$target,type="l",col="blue")
lines(sim$state,type="l",col="red")


