if(!require("shiny")) install.packages("shiny")
if (!require("ggmap")) install.packages("ggmap")
if (!require("ggplot2")) install.packages("ggplot2")


source("viz.R")

figure<-graph_wait_pts()

shinyServer(
  function(input, output) {
  
    
    output$plot1 <- renderPlot({ 
    
   if (input$policy=="1") {  
      #plotting wait points
      figure <- figure
    } 
   
   if (input$policy=="2") {  
     #plotting wait points
     figure <- figure
   } 
   
   if (input$policy=="3") {  
     #plotting wait points
     figure <- figure
   } 
      
      print(figure)
      
    })
    
    
    
  }
)