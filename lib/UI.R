library(shiny)

shinyUI(fluidPage(
  
  titlePanel( h2("Drone Flights in Central Park", align="center")),
  sidebarLayout(
  sidebarPanel(
  fluidRow(h4("Dynamic Programming Options", align="center"),
  fluidRow(column(width=12, offset=1,
                   fluidRow(radioButtons("policy", label = "Policy Type",
                                choices = list("None"=1, "Open" = 2, "Closed" = 3), selected = 1)),
                   fluidRow( checkboxGroupInput("uncertainty", label ="Uncertainty",
                                    choices = list("Deterministic" , "Perfect State: Wind" , "Imperfect State: GPS+Wind"), 
                                    selected = "Deterministic")),
                  fluidRow( h5(strong("Initial Wind Speed (meters/sec)"))),
                  column(width=9,fluidRow( sliderInput("x_initialWind", 
                                              label = "X-Coordinate", min = -10, max = 10, value = 0))),
                  column(width=9, fluidRow(sliderInput("y_initialWind", 
                                              label = "Y-Coordinate", min = -10, max = 10, value = 0))) ) ))
  ),
  mainPanel(
  plotOutput("plot1", width="600", height="600")
  )                        
  
  ) #closes sidebar layout
  
))  #closes fluid page and shiny #UI
