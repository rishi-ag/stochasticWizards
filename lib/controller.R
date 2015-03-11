#' perfect.info.lqr
#' 
#' Steer the system towards a target state, optimizing for quadratic loss.
#'
#' @param target (numeric matrix) Target state at each iteration (starting at 0)
#' @param params (list) Deterministic model parameters. Consists of symmetric weighting matrices \code{Q} and \code{R}.
#' @param noise.model (list) Model which returns mean estimates and noise draws.
#' @return A list with following elements: target, state, controls, noise, loss
#' @export
#' @import
#' @examples
#' # set target trajectory and parameters
#' target <- matrix(c(1, 1, 1, -1, -1, -1, -1, 1, 1, 1),
#'     nrow = 5,
#'     ncol = 2,
#'     byrow = TRUE
#'     )
#' params <- list(Q = diag(1, 2, 2), R = diag(0, 2, 2))
#' noise.model <- list(draw = gaussian.noise, mean = rep(0, 2), var = diag(0.01, 2, 2))
#'
#' # run
#' sim <- perfect.info.lqr(target, params, noise.model)
#'

perfect.info.lqr <- function(target, params, noise.model) {

    # math
    .policy <- function(x, K, Q, R, mean.noise)
        -solve(R + K) %*% K %*% (x + mean.noise)
    .dynamics <- function(x, u, w)
        x + u + (60*0.2)*w # convert m/s to m shift, 20% efect on drone
    .riccati.eqn <- function(K, Q, R)
        K -  K %*% solve(K + R) %*% K + Q
    .loss <- function(X, U, Q, R)
        sum(apply(X, 1, function(x) x %*% Q %*% x)) + sum(apply(U, 1, function(u) u %*% R %*% u))

    # preprocess
    niter <- dim(target)[1]
    dims <- dim(target)[2]
    target <- target
    noise <- noise.model
    state <- matrix(nrow = niter, ncol = dims)
    state[1,] <- target[1,]
    controls <- matrix(nrow = niter - 1, ncol = dims)
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
            noise$means[i]
            )

        # simulate next waypoint
        state[i + 1,] <- .dynamics(state[i,], controls[i,], noise$draws[i,])
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







