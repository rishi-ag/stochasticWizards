#' real.path
#' 
#' Real path from control data, real wind and initial position
#'
#' @param target Matrix Target path
#' @param controls Matrix For each step, controls (x,y,z) sent to drone to determine next movement
#' @param real_wind Matrix For each step, real wind that occurs (m/s) in x(east),y(north),z(vertical) directions
#' @param initial position x,y,x initial coordinates
#' @param Q Matrix Quadratic loss related to position
#' @param R Matrix Quadratic loss related to controls
#' @return A list of the following elements: path (matrix of real path), and loss (numeric quantity of loss)
#' @export
#' @import
#' @examples
#' 
#'# Retrieving real wind CPNY macey's day 2009
#' n<-dim(target)[1]
#' real_wind<-read.csv("data/CPNY_wind_NYeve.csv",stringsAsFactors =F)
#' index<-which(real_wind$date=="2009-12-31 10:00:00")
#' real_wind<-as.matrix(real_wind[index:(index+n),4:5])
#' real_wind<-cbind(real_wind,rep(0,n+1))
#' wind_ini<-real_wind[1,]
#' 
#'# Find control
#' sim <- perfect.info.lqr(
#'     target,
#'     list(Q = diag(1, 3, 3), R = diag(0, 3, 3)),
#'     ny.wind.model(n, wind.ini=wind_ini,type="simulated")
#' )
#' 
#'# Calculate real path with this control
#' real<-real.path(sim$controls,real_wind,target[1,])

#'



real.path<-function(target,controls,real_wind,initial_position,Q,R){
    #Loss function
    .loss <- function(X, U, Q, R)
        sum(apply(X, 1, function(x) x %*% Q %*% x)) + sum(apply(U, 1, function(u) u %*% R %*% u))
    
    # Calculate path
    n<-dim(controls)[1]
    path<-matrix(rep(NA,(n+1)*dim(controls)[2]),n+1,dim(controls)[2])
    path[1,]<-initial_position
    for (i in (1:n)){
        path[i+1,]<-path[i,]+controls[i,]+0.2*60*real_wind[i]
    }
    # Calculate loss
    loss<-.loss(target-path,controls,Q,R)
        
    return (list(path=path,loss=loss))
}