library(shiny)

shinyUI(fluidPage(
  
  titlePanel( h2("Drone Flights in Central Park", align="center")),
  sidebarLayout(
  sidebarPanel(
  fluidRow(h4("Dynamic Programming Options", align="center"),
  fluidRow(column(width=12, offset=1,
                   fluidRow( checkboxGroupInput("policy", label = "Policy Type",
                                choices = list("Open" = 1, "Closed" = 2))),
                  fluidRow( checkboxGroupInput("uncertainty", label ="State (Position) Uncertainty",
                                    choices = list("Perfect State: Wind" , "Imperfect State: GPS + Wind"))),
                  fluidRow(checkboxGroupInput("simulatated", label = "Wind Model",
                                                      choices = list("Fixed" = 1, "Simulated"= 2))),
                  fluidRow(radioButtons("realPath", label = "View Real Path",
                                              choices = list("Yes" = 1, "No"= 2), selected=2)),
                  fluidRow( h5(strong("Initial Wind Speed (meters/sec)"))),
                  column(width=9,fluidRow( sliderInput("x_initialWind", 
                                              label = "X-Coordinate", min = -10, max = 10, value = 0))),
                  column(width=9, fluidRow(sliderInput("y_initialWind", 
                                              label = "Y-Coordinate", min = -10, max = 10, value = 0)))                
                  ) ))
  ),
  mainPanel(
  plotOutput("plot1", width="600", height="600"),
  textOutput("loss")
  )                        
  
  ) #closes sidebar layout
  
))  #closes fluid page and shiny #UI
