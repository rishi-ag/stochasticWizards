open.loop.lqr <- function(target, params, noise.model) {

    # math
    .policy <- function(x, K, Q, R, mean.noise)
        -solve(R + K) %*% K %*% (x + mean.noise)
    .dynamics <- function(x, u, w)
        x + u + w
    .riccati.eqn <- function(K, Q, R)
        K - K %*% solve(K + R) %*% K + Q
    .loss <- function(X, U, Q, R)
        sum(apply(X, 1, function(x) x %*% Q %*% x)) + sum(apply(U, 1, function(u) u %*% R %*% u))

    # preprocess
    niter <- dim(target)[1]
    dims <- dim(target)[2]
    target <- target
    noise <- noise.model(niter - 1)
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
            target[i,] - target[i + 1,],
            params$K[[niter - i + 1]],
            params$Q,
            params$R,
            noise$means[i,]
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


perfect.info.lqr <- function(target, params, noise.model) {

    # math
    .policy <- function(x, K, Q, R, mean.noise)
        -solve(R + K) %*% K %*% (x + mean.noise)
    .dynamics <- function(x, u, w)
        x + u + w
    .riccati.eqn <- function(K, Q, R)
        K - K %*% solve(K + R) %*% K + Q
    .loss <- function(X, U, Q, R)
        sum(apply(X, 1, function(x) x %*% Q %*% x)) + sum(apply(U, 1, function(u) u %*% R %*% u))

    # preprocess
    niter <- dim(target)[1]
    dims <- dim(target)[2]
    target <- target
    noise <- noise.model(niter - 1)
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
            params$K[[niter - i + 1]],
            params$Q,
            params$R,
            noise$means[i,]
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


imperfect.info.lqr <- function(target, params, noise.model, state.noise) {

    # math
    .policy <- function(x, K, Q, R, mean.noise)
        -solve(R + K) %*% K %*% (x + mean.noise)
    .dynamics <- function(x, u, w)
        x + u + w
    .riccati.eqn <- function(K, Q, R)
        K - K %*% solve(K + R) %*% K + Q
    .loss <- function(X, U, Q, R)
        sum(apply(X, 1, function(x) x %*% Q %*% x)) + sum(apply(U, 1, function(u) u %*% R %*% u))

    # preprocess
    niter <- dim(target)[1]
    dims <- dim(target)[2]
    target <- target
    noise <- noise.model(niter - 1)
    state.noise <- state.noise(niter - 1)
    tru.state <- matrix(nrow = niter, ncol = dims)
    tru.state[1,] <- target[1,]
    est.state <- matrix(nrow = niter, ncol = dims)
    est.state[1,] <- target[1,]
    controls <- matrix(nrow = niter - 1, ncol = dims)
    params$K <- list(params$Q)
    params$P <- 100 * diag(dims)
    for (i in (2:niter))
        params$K[[i]] <- .riccati.eqn(params$K[[i-1]], params$Q, params$R)
    
    # run simulation
    for (i in 1:(niter - 1)) {

        # compute optimal step
        controls[i,] <- .policy(
            est.state[i,] - target[i + 1,],
            params$K[[niter - i + 1]],
            params$Q,
            params$R,
            noise$means[i,]
            )

        # simulate next waypoint
        tru.state[i + 1,] <- .dynamics(tru.state[i,], controls[i,], noise$draws[i,])
        noisy.state <- tru.state[i + 1,] + state.noise$draws[i,] # make next waypoint noisy due to GPS uncertainty
        filter <- kalman.filter( #estimate as better as possible the true state with kalman filter
            est.state[i,],
            controls[i,],
            noisy.state,
            params$Q,
            params$R,
            params$P
            )
        est.state[i + 1,] <- filter$est.state
        params$P <- filter$P.new
    }

    # compute loss
    loss <- .loss(target - tru.state, controls, params$Q, params$R)

    return(list(
        target = target,
        est.state = est.state,
        tru.state = tru.state,
        controls = controls,
        noise = noise,
        state.noise = state.noise,
        loss = loss
        ))
}


kalman.filter <- function(est.state.prev, control, noisy.state, Q, R, P.prev) { 
    
    est.state <- est.state.prev + control
    P.new <- P.prev + Q
  
    y <- noisy.state - est.state
    S <- P.new 
    K <- P.new %*% solve(S)
    est.state <- est.state + K %*% y
    P.new <- (diag(3) - K) %*% P.new
  
    return(list(est.state = est.state, P.new = P.new))
}
