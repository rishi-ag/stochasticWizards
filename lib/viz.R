library(ggmap)
library(ggplot2)
library(shiny) 
library(devtools) 
library(rgdal)
library(grid)


graph_wait_pts<- function (wait_points, zoom){
lon1<- -73.972
lat1<- 40.772
mapImageData <- get_googlemap(center = c(lon = lon1, lat = lat1),
                              zoom = 15,
                              # size = c(500, 500),
                              maptype = c("terrain"))
ggmap(mapImageData)



wait_points<- data.frame(lat=wait_points[,2], lon=wait_points[,1])


base_plot<- ggmap(mapImageData) +
        geom_point(data=wait_points,aes(x=lon,y=lat), colour='red', size=2) +
        theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank()) 
      


return(base_plot)

}


gps_to_utm<-function  (target) {

  
  xy <- data.frame(ID = 1:nrow(target), X = target[,2], Y = target[,1])
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  
  res <- spTransform(xy, CRS("+proj=utm +zone=18 ellps=NAD27"))
  res
}


utm_to_gps<-function (sim) {
  # prepare UTM coordinates matrix 
  state<-sim[[2]]
  utmdata <- state[,1:2]
  utmcoor<-SpatialPoints(utmdata, proj4string=CRS("+proj=utm +zone=18"))
  longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))       
  state_LL<-as.data.frame(longlatcoor)

}