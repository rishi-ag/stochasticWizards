#' real.path
#' 
#' Real path from control data, real wind and initial position
#'
#' @param n integer Number of predicted observations to be generated
#' @param wind.ini Vector Initial wind speed (m/s) in x(east),y(north),z(vertical) directions
#' @param type Factor Type of data to generate. 'null' means all wind results set to 0, 'fixed' means all wind results set to initial value, 'simulated' means simulate an AR1 model, 'online_simulated' mean to consider every step the real wind data and generate a 1-step ahead prediction from that value.
#' @return A list of two matrices. Draws are the stochastic draws from an AR1 model. Means are the expected values for next step, considering only the deterministic part of AR1 model
#' @export
#' @import
#' @examples
#' 
#' #Retrieving real wind CPNY new year's eve 2009-2010
#' wind_ini<-as.vector(read.csv("data/wind_ini_CPNY.csv",stringsAsFactors =F))[,2]

#' # get 10 shocks
#' ny.wind.model(n=10, wind.ini=c(wind_ini,0),type="stochastic")
#'



real.path<-function(controls,real_wind,initial_position){
    n<-dim(controls)[1]
    path<-matrix(rep(NA,(n+1)*dim(controls)[2]),n+1,dim(controls)[2])
    path[1,]<-initial_position
    for (i in (1:n)){
        path[i+1,]<-path[i,]+controls[i,]+0.2*60*real_wind[i]
    }
    
    return (path)
}