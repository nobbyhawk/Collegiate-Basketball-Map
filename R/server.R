# server.R

# Must use helpers to download the espn data first

## Load Packages ------------------------------------------------------
if(!require(devtools)) install.packages("devtools")
if(!require(geosphere)) install.packages("geosphere")
if(!require(lubridate)) install.packages("lubridate")
if(!require(htmltools)) devtools::install_github("rstudio/htmltools")
if(!require(htmlwidgets)) devtools::install_github("ramnathv/htmlwidgets")
if(!require(leaflet)) devtools::install_github("rstudio/leaflet")

## Load Data -------------------------------------------------------------
#load("espnTbls", verbose = 2)

if(!exists("NCAA_Locations")) NCAA_Locations <- read.csv("NCAA_Locations.csv",
                                                         stringsAsFactors=FALSE)
schedule <- espnTbls[[3]]



source("helpers.R")



## Server ---------------------------------------------------------

server <- function(input, output, session) {
 
  #Make the basketball icon
  basketBallIcon <- makeIcon(iconUrl = "images/basketball_map_icon.png", 18, 18)
  
  #Create the initial map, markers, and lines
  output$collegeMap <- renderLeaflet({
    leaflet(schedule()) %>% addTiles() 
  })
  
  #Outputs the name of the opponent team 
  output$opponentName <- renderText({
    symbol <- ""
    rank <- ""
    if(!is.null(schedule()$oppRank[currentGame()]))
      rank <- ifelse(is.na(schedule()$oppRank[currentGame()]), "", schedule()$oppRank[currentGame()])
    if (schedule()$location[currentGame()] == "away") symbol <- "@"
    else symbol <- "vs"
    
    paste( symbol, rank, schedule()$opponent[currentGame()])
  })
  
  
  #Outputs Total Distance Travelled by team
  output$distTraveled <- renderText({
    total <- 0 #Start at 0 and sum line-by-line
    
    if (currentGame() <= 1 ) {
      return(paste("0"))
    }
    
    for(i in 2:currentGame()) {
      if(schedule()$place[i] != schedule()$place[i-1]) {
        total <- total + distCosine(p1 = c(schedule()$lon[i-1],schedule()$lat[i-1]),
                                    p2 = c(schedule()$lon[i],schedule()$lat[i]))
      }
    }
    paste(round(total/1609.34,digits = 0))
  })
  
  #Displays selected teams score
  output$resultSelected <- renderText(({
    paste(schedule()[currentGame(), "scoreTeam"])
  }))
  
  output$resultOpponent <- renderText(({
    paste(schedule()[currentGame(), "scoreOpp"])
  }))
  
  #Display score of last game
  output$gameResult <- renderText({
    gameNum <- currentGame()
    paste(schedule()$result[currentGame()], attr(schedule(), "team"))
  })
  
  #Outputs the team's record for the season so far
  output$record <- renderText({
    if(is.na(schedule()$wins[currentGame()])) paste0("0-0")
    else paste(schedule()$wins[currentGame()],schedule()$losses[currentGame()], sep = "-")
  })
  
  #Outputs average points per game
  output$avgPoints <- renderText ({
    paste(round(mean(schedule()$scoreTeam[1:currentGame()], na.rm= T), digits = 2))
  })
  
  # currentGame() - Variable that has current game number based on slider position
  currentGame <- reactive({
    max(which(schedule()$dates <= input$slider))
  })

  # currentTeam() - Variable that holds the selected team in drop down menu
  currentTeam <- reactive({
    input$team
  })
  
  # homeMarker() is the lon/lat of the selected team's home stadium
  homeMarker <- reactive({
    ind <- which(schedule()$location == 'home')[1]
    c(schedule()$lon[ind], schedule()$lat[ind])
  })
  
  # schedule() is an updated data.frame of the selected team's schedule at the
  # specified date
  schedule <- reactive({
    ind <- 1 #Keeps track of index of selected team in espnTbls
    if (currentTeam() == '') {
      return(espnTbls[[1]])
    } else {
      for (i in 1:length(espnTbls)) {
        #find index of team - they are not sorted alphabetically
        if (attr(espnTbls[[i]], 'team') == currentTeam())
          ind <- i
      }
      
      schedule <- espnTbls[[ind]]
      
      #merge schedule with NCAA_Locaitons to get lon/lat fields
      schedule <- merge(schedule, NCAA_Locations, by.x = "place", by.y = "School" )
      schedule <- schedule[order(schedule$dates),]
      
      schedule
    }
  })
  
  #Observe when team is changed in the drop down menu
  observe( {
    #Update slider for the team's schedule
    
    updateSliderInput(session, "slider", label = "Team",
                      value = schedule()$dates[1], min = schedule()$dates[1],
                      max = schedule()$dates[nrow(schedule())])
    
    
    output$collegeMap <- renderLeaflet({
      leaflet(schedule()) %>% addTiles() %>%
        fitBounds(min(schedule()$lon), min(schedule()$lat),
                  max(schedule()$lon), max(schedule()$lat)) %>%
        addMarkers(lng = homeMarker()[1], lat = homeMarker()[2])
    })
  })
  
  
  #Changes lines when the date slider is moved
  observe( {
  
      #Need to access games - checking newLines for game location is stupid - it
      #doesnt change when a game is in the same place
    
      #TravelLines() creates a data frame of points for all the travel lines
      linesNew <- travelLines(schedule(), currentGame())
      
      #If the data frame is null/empty, remove all lines from the map
      if(is.null(linesNew) | length(linesNew) == 0 ) {
        
        leafletProxy("collegeMap", data = NULL) %>%
                      clearGroup("lines")
      } 
     
      # #Draw basketball icon for the first game
      # else if (nrow(linesNew) == 30) {
      #   leafletProxy("collegeMap", data = schedule()) %>%
      #     clearGroup("lines") %>%
      #     clearGroup("basketball") %>%
      #     addPolylines(lng = linesNew$lon, lat = linesNew$lat, group = "lines") %>%
      #     addMarkers(lng = linesNew$lon[length(linesNew)] + 50,
      #                lat = linesNew$lat[length(linesNew)] + 50,
      #                icon = basketBallIcon,
      #                group = "basketball")
      #   
      #   print(nrow(linesNew))
      # }
      
      # V THis was previous conditions V
      # Redraw lines as game progresses
      else {
        leafletProxy("collegeMap", data = schedule()) %>%
          clearGroup("lines") %>%
          clearGroup(group = "basketball") %>%
          addPolylines(lng = linesNew$lon, lat = linesNew$lat, group = "lines", 
                       opacity = 0.9, weight = 4, color = "red") %>%
          addMarkers(lng = linesNew$lon[nrow(linesNew)],
                     lat = linesNew$lat[nrow(linesNew)],
                     icon = basketBallIcon, group = "basketball")
      }
      
      #Draw lines and basketball icon
      # else {
      #   leafletProxy("collegeMap", data = schedule()) %>%
      #     clearShapes() %>%
      #     clearMarkers() %>%
      #     addPolylines(lng = linesNew$lon, lat = linesNew$lat) %>%
      #     addMarkers(lng = linesNew$lon[length(linesNew)],
      #                lat = linesNew$lat[length(linesNew)],
      #                icon = basketBallIcon)
      #   
      #   print("Else 2")
      #   print(nrow(linesNew))
      # }
    })
}


