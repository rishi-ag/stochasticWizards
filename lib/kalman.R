library(assertthat)

kalman.filter <- function(est.state.prev, control, noisy.state, Q, R, P.prev) {
  assert_that(is.matrix(P.prev), nrow(P.prev) ==3,  ncol(P.prev) == 3)
  assert_that(is.matrix(Q), nrow(Q) ==3,  ncol(Q) == 3)
  assert_that(is.matrix(R), nrow(R) ==3,  ncol(R) == 3)
  
  #Predict Stage
  #Predicted (a priori) state estimate
  est.state <- est.state.prev + control
  #Predicted (a priori) estimate covariance
  P.new <- P.prev + Q
  
  #Update Stage
  #Innovation or measurement residual
  y <- noisy.state - est.state
  #Innovation (or residual) covariance
  S <- P.new 
  #Optimal Kalman gain
  K <- P.new %*% solve(S)
  #Updated (a posteriori) state estimate
  est.state <- est.state + K %*% y
  #Updated (a posteriori) estimate covariance
  P.new <- (diag(3) - K) %*% P.new
  
  return(list(est.state = est.state, P.new = P.new))
}
