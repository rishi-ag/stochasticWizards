require(MASS)


# ----------------------------------------------------------------------
# Perfect State Information Controller
# ----------------------------------------------------------------------
#' perfect.info.lqr
#' 
#' Steer the system towards a target state, optimizing for quadratic loss.
#'
#' @param target (numeric matrix) Target state at each iteration.
#' @param dynamics (function) State transition between iterations.
#' @param policy (function) Optimal policy.
#' @param noise.model (function) Function generating random draws from the noise distribution.
#' @param params (list) Parameters of the cost function, the noise model and the dynamics. Q penalizes deviations form the targets and R penalizes movement.
#' @return A list with following elements: target, state, controls, noise, loss, params
#' @export
#' @import MASS
#' @examples
#' # example to be created
#'

perfect.info.lqr <- function(target, dynamics, policy, noise.model, params) {

    # setup
    target <- target
    state <- matrix(nrow = dim(target)[1], ncol = dim(target)[2])
    state[1,] <- target[1,]
    controls <- matrix(nrow = dim(target)[1] - 1, ncol = dim(target)[2])
    noise <- noise.model(dim(target)[1] - 1, params$mean.noise, params$var.noise)

    # compute K-matrices
    K <- list(params$Q)
    for (i in (2:dim(target)[1])) {
        K[[i]] <- riccati.eqn(K[[i-1]], params$Q, params$R)
    }

    # run simulation
    for (i in 1:(dim(state)[1] - 1)) {

        # compute optimal step
        controls[i,] <- policy(
            state[i,] - target[i + 1,],
            K[[dim(target)[1] - i]],
            params$Q,
            params$R,
            params$mean.noise
            )

        # simulate next ioint
        state[i + 1,] <- dynamics(
            state[i,],
            controls[i,],
            noise[i,]
            )
    }

    # compute losses
    dev.loss <- sum(apply(target - state, 1, function(x) x %*% params$Q %*% x))
    ctrl.loss <- sum(apply(controls, 1, function(u) u %*% params$R %*% u))

    return(list(
        target = target,
        state = state,
        controls = controls,
        noise = noise,
        loss = dev.loss + ctrl.loss,
        params = params
        ))
}


policy <- function(state, K, Q, R, mean.noise) {
    return(-solve(R + K) %*% K %*% (state + mean.noise))
}


dynamics <- function(state, control, noise) {
    return(state + control + noise)
}


gaussian.noise <- function(n, mean, var) {
    return(mvrnorm(n, mean, var))
}


riccati.eqn <- function(K, Q, R) {
    return(K -  K %*% solve(K + R) %*% K + Q)
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
    dynamics,
    policy,
    gaussian.noise,
    list(Q = diag(1, 2, 2), R = diag(0, 2, 2), mean.noise = rep(0, 2), var.noise = diag(0.01, 2, 2))
    )
