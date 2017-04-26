# ui.R

# Load Packages -----------------------------------------------------------

library(shiny)
library(leaflet)

# UI ----------------------------------------------------------------------


ui <- fluidPage(
  titlePanel(em("Travel Map")),
  sidebarLayout(
    mainPanel(
      leafletOutput("collegeMap"),
      p(),
      sliderInput("slider", label = "Date", schedule$dates[1],
                  schedule$dates[nrow(schedule)], value = schedule$dates[1],
                  animate = animationOptions(interval = 100))
      
    ),
    sidebarPanel( selectizeInput("team", label = "Team", NCAA_Locations$School, selected = NCAA_Locations[[1]]),
                  br(),
                  h3(strong(em(textOutput("opponentName"))), align = "left"),
                  span(h3(em(textOutput("gameResult"))),
                       style = "color:red",
                       align = "left"),
                  splitLayout(h1(em(textOutput("resultSelected"))),
                              h1(em(textOutput("resultOpponent")))),
                  br(),
                  h3(strong(em("Season Statistics")), align = "left"),
                  textOutput("record"),
                  br(),
                  flowLayout(
                    verticalLayout(span("Miles Traveled",
                                        style = "color:blue", align = "left"),
                                   textOutput("distTraveled")),
                    verticalLayout(span("Average Points Per Game",
                                        style = "color:blue", align = "left"),
                                   textOutput("avgPoints"))
                  )
                  
    )
  )
)


