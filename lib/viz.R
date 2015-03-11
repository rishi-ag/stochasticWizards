library(ggmap)
library(ggplot2)
library(shiny) 
library(devtools) 
library(rgdal)


graph_wait_pts<- function (){
lon1<- -73.970543
lat1<- 40.7744
mapImageData <- get_googlemap(center = c(lon = lon1, lat = lat1),
                              zoom = 15,
                              # size = c(500, 500),
                              maptype = c("terrain"))
ggmap(mapImageData)


wait_points<-t(matrix(c(40.776986, -73.963652,
                      40.773753, -73.966088,
                      40.770503, -73.968416,
                      40.768049, -73.970272,
                      40.764295, -73.973008,
                      40.765666, -73.976170,
                      40.766852, -73.979088,
                      40.768152, -73.981535,
                      40.771191, -73.979668,
                      40.774246, -73.977458,
                      40.777545, -73.975076,
                      40.780778, -73.972683),
                      nrow=2 ))

wait_points<- data.frame(lat=wait_points[,1], lon=wait_points[,2])


base_plot<- ggmap(mapImageData) + geom_point(data=wait_points,aes(x=lon,y=lat), colour='red', size=3) +
        theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank())


return(base_plot)

}

