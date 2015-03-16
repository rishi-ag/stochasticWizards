if(!require("shiny")) install.packages("shiny")
if (!require("ggmap")) install.packages("ggmap")
if (!require("ggplot2")) install.packages("ggplot2")


source("viz.R")
source("stochastics.R")
source("controller.R")
source("real_path.R")

#standard blick in manthattan is 80 m (st block) × 274 m (avenue part)
#walking speed is assumed to be 80m/min (4km/hr)
source("viz_setup.R")


figure<-graph_wait_pts(wait_points)

shinyServer(
  function(input, output) {
  
    
    output$plot1 <- renderPlot({ 
     
 
      
   if ("1" %in% input$policy) {  
      #plotting wait points
      sim <- perfect.info.lqr(
                              target,
                              list(Q = Q, R = R),
                              ny.wind.model(n, wind.ini=wind_ini,type="null"))
     
      loss <-sim$loss
     
      LonLat<-data.frame(utm_to_gps(sim))
      figure <- figure + 
                geom_path(data=LonLat,aes(x=coords.x1,y=coords.x2), arrow=arrow(), colour="red")
      }
   if ("2" %in% input$policy) {  
     #plotting wait points
     #run simulation
         print("1")
         sim <- perfect.info.lqr(
                                  target,
                                  list(Q = Q, R = R),
                                  ny.wind.model(n, wind.ini=wind_ini,type="simulated"))
         
         loss <-sim$loss
         
         LonLat<-data.frame(utm_to_gps(sim))
          
         figure <- figure + 
            geom_path(data=LonLat,aes(x=coords.x1,y=coords.x2), arrow=arrow(), colour="blue") +
            geom_point(data=LonLat,aes(x=coords.x1,y=coords.x2), colour='blue', size=2) 
    }
      print(figure)
      
  
      output$loss <-renderPrint({print(paste("Simulated Loss:", round(loss), sep=" "))})
      #output$realloss <-renderPrint({print(paste("Real Loss:", round(realloss), sep=" "))})
    })
    
   
    
  }
)


