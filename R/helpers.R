# helpers.R

#################################################################################
#
#
#   This code contains the functions that assist in the basketball schdeule
#   visualizer ShinyApp. Including:
#
#   loc_wiki() -- Scrapes all Div I NCAA School lon/lat
#   travelLines(schedule) -- create lines given a schedule
#   espn_scrape -- scrapes and cleans all schools from this year (change to get other years)
#         -attribute for year?
#
#   TODO!!!!
#     espn_scrape: 1. multiple OT bug 2. attr for 'year' param, not lubridate function
#     travelLines: 1. Make csv(or R obj?) for each team/year
#################################################################################

#Load required packages
if (!require("ggmap"))
  install.packages("ggmap")
if (!require("rvest"))
  install.packages("rvest")
if (!require("stringr"))
  install.packages("stringr")
if (!require("dplyr"))
  install.packages("dplyr")
if (!require(XML))
  install.packages("XML")
if (!require(lubridate))
  install.packages("lubridate")


travelLines <- function(schedule, gameNum) {
  
  
  # Get pre-scraped locations of universities
#   if(!exists("NCAA_Locations")) NCAA_Locations <- read.csv("data/NCAA_Locations.csv",
#                              stringsAsFactors=FALSE)
# #   
#   NCAA_Locations[27, "School"] <- "Boston University"
#   NCAA_Locations[315, "School"] <- "South Florida"
#   
  #We must determine when schools do not travel for subsequent games
  #only new locations will be graphed with a line
  
  
  
  #Ammend the schedule to include the correct long/lat information
#   schedule <- merge(schedule, NCAA_Locations, by.x = "place", by.y = "School" )
#   schedule <- schedule[order(schedule$dates),]
#   
  
  #Don't return any lines if only on the first game
  if (gameNum <= 1) return(NULL)
  

  repeatLocation <- 0 #First game cannot be in a repeated location...
  for(i in 2:gameNum){ #look for multiple home games or an away series at one school
    repeatLocation[i] <- (schedule$place[i] == schedule$place[i - 1])
  }
  
  # "lines" will hold the travel line information that will be returned
  lines <- data.frame("lon" = c(), "lat" = c())

    for (i in 2:gameNum) {
    if (!repeatLocation[i]) {
      #Create lines between new locations in the schedule
      tempLines <- geosphere::gcIntermediate(
        p1 = c(schedule$lon[i - 1], schedule$lat[i - 1]),
        p2 = c(schedule$lon[i], schedule$lat[i]),
        n = 30
      )
      names(tempLines) <- names(lines)
      lines <- rbind(lines, tempLines)
    }
  }
  
  return(lines)
}


loc_wiki <- function() {
  ## Scrapes Div I schools (inc city/state) from Wikipedia
  ## td , th
  
  # Get table of Div I Schools from wikipedia - cities of the university are listed
  team_cities <-
    read_html("http://en.wikipedia.org/wiki/List_of_NCAA_Division_I_institutions") %>%
    html_table(header = T, fill = T)
  
  
  # Their are two returned tables from wikipedia - we need to merge them
  # 1: Current teams
  # 2: Teams transitioning from DII -> DI this year
  
  uni_table <- rbind(team_cities[[1]], team_cities[[2]][,1:6] )
  remove(team_cities) #Dont need this list anymore
  
  
  # They appear to have the same length (351), I am *assuming* ESPN and Wiki have same teams
  # Geocode(get longitude and latitude) of each school
  uni_table <- c(uni_table,
                 geocode(paste(uni_table$City, uni_table$State, sep = ", "),
                       )
  )
  uni_table <- as.data.frame(uni_table)
  uni_table <- uni_table[order(uni_table$School),]
  
  #TODO: Get correct school names ***********************
  # Sort by team name...
  wiki_name_order <- read.csv("data/wiki_name_order.csv", sep="", stringsAsFactors=FALSE)
  uni_table <- uni_table[wiki_name_order$x,]
  
  uni_names <- read.csv("data/uni_names.csv") #must run espn_scrape first
  uni_table$School <- as.character(uni_names$x)
  uni_table <- uni_table[,-1]
  
  #Save locations in local directory
  write.csv(x = uni_table, file = "data/NCAA_Locations.csv")
}

espn_scrape <-
  function(year = lubridate::year(lubridate::today())) {
    
   
    #Get all team URLS
    team_htmls <- as.character(
      read_html("http://www.espn.com/mens-college-basketball/teams") %>%
        html_nodes(".bi")
    )
    
    #Get all the team names located in the url
    team_names <- as.character(str_sub(
      team_htmls,
      start = str_locate(team_htmls, "_/id/[0-9]*/")[,2] + 1,
      end = str_locate(team_htmls, '\" class=')[,1] - 1
    ))
    
    ######
    #Get official team names
    ######
    uni_names <- as.character(str_sub(
      team_htmls,
      start = str_locate(team_htmls, 'bi\\">')[,2] + 1,
      end = str_locate(team_htmls, '</a')[,1] - 1
    ))
    
    
    #get all the team ids in the url
    team_ids <- as.character(str_sub(
      team_htmls,
      start = str_locate(team_htmls, pattern = "_/id/")[, 2] + 1,
      end = str_locate(team_htmls, pattern = "_/id/[0-9]*")[, 2]
    ))
    
    #we want a file that contains names/id of the schools...
    uni_names <- str_replace(uni_names, pattern = "&amp;", replacement = "&")
    
    #This file contains the espn names, in order
    temp_frame <- data.frame(uni_names = uni_names[order(uni_names)], id = team_ids[order(uni_names)])
    write.csv(temp_frame,file = "data/uni_names.csv")
    
    teamURLS <-
      paste0(
        "http://www.espn.com/mens-college-basketball/team/schedule/_/id/",
        team_ids,
        "/year/",
        ifelse(length(as.character(year)) == 4, as.character(year), "2016")
      )
    
    #Return table with urls/names/ids
    espnTable <- data.frame(teamURLS, team_names, team_ids)
    espnTable$teamURLS <- as.character(espnTable$teamURLS)
    
    
    
    #Scrape the raw tables from the list of team urls
    numLoops <- length(espnTable$teamURLS)
    espnTbls <- list()
    for (i in 1:numLoops) {
      html <- read_html(espnTable$teamURLS[i])
      cast <- html_node(html, "#showschedule > div > table")
      espnTbls[[i]] <- rvest::html_table(cast)
    }
    
    
    #Clean each table...
    for ( i in 1:numLoops) {
      tbl <- espnTbls[[i]]
      # Extract information from the header and remove header rows from table (will only run once)
      if (tbl[1,1] == tbl[1,2]) {
        # Get context of the data from the first row...
        header <- tbl[1,1] #First row contains the table header
        year <-
          as.integer(str_sub(header, 1, 4)) #The first year of the schedule
        team <-
          str_sub(header, 9, str_locate(header, " Schedule")[1] - 1) #The team name
        
        #Delete first and second rows
        tbl <- tbl[-c(1,2),]
      }
      
      
      # Convert the dates into date objects
      # %a Abbrviated weekday
      # %b/%h abbreviated month
      # %d day of month
      dates <-
        as.Date(x = tbl[,1], format = "%a, %b %d") #Temp holder for our dates
      
      # Record index of the last game in January, we know each game after that
      # must occur in the following year!
      lastGameIdx <- which(lubridate::month(dates) == 1)[1] - 1
      
      lubridate::year(dates[1:lastGameIdx]) <-
        year #First half of season is in the first year
      lubridate::year(dates[(lastGameIdx + 1):length(dates)]) <-
        (year + 1)
      tbl[,1] <- dates #Place cleaned data in data frame
      names(tbl)[1] <- "dates"
      
      
      # Create column for Home/Away/Nuetral
      loc <-
        str_sub(tbl[,2], start = 1, end = 1) #Temp holder for location
      loc <- str_replace(loc, "@", "away")
      loc <- str_replace(loc, "v", "home")
      loc[!is.na(str_locate(tbl[,2], pattern = "[*]"))[,1]] <-
        "nuetral"
      tbl$location <- as.factor(loc) #insert into data frame
      
      
      # Create Column for Opponent Ranking... if it is in there
      tbl$oppRank[str_detect(tbl[,2], pattern = "#")] <-
        str_sub(
          tbl[,2][str_detect(tbl[,2], pattern = "#")],
          start = str_locate(tbl[,2][str_detect(tbl[,2], pattern = "#")], pattern = "#")[,1] + 1,
          end = str_locate(tbl[,2][str_detect(tbl[,2], pattern = "#")], pattern = "#[0-9]*")[,2]
        )
      
      # Create column for Team name
      tbl$opponent <-
        str_sub(tbl[,2], str_locate(tbl[,2], "[A-Z]")[,1], length(tbl[,2]))
      tbl$opponent <-
        str_replace(tbl$opponent, pattern = "[*]", replacement = "") #Remove *'s
      
      # Create Column for Game Result
      tbl$result <- as.factor(str_sub(tbl[,3], start = 1, end = 1))
      
      # Create Column for OT flag
      tbl$ot <- str_detect(tbl[,3], "OT")
      
      #     # Create Column for number of OT periods
      #     tbl$otPeriods[tbl$ot] <- 
      #       as.numeric(
      #         str_sub(tbl[tbl$ot,3],
      #                 start = str_detect(tbl[,3], pattern = "[0-9]OT")[,1],
      #                 end = str_detect(tbl[,3], pattern = "[0-9]OT")[,1]
      #                 )
      #         )
      #     
      # Create Columns for Opponent/Team of interest Scores
      lowScore <- str_sub(tbl[,3], str_locate(tbl[,3], "[-]")[,1] + 1)
      lowScore[tbl$ot] <-
        str_sub(lowScore[tbl$ot], 1, str_locate(lowScore[tbl$ot], " OT")[,1] - 1)
      tbl$scoreTeam[tbl$result == "L"] <-
        lowScore[tbl$result == "L"]
      tbl$scoreOpp[tbl$result == "W"] <- lowScore[tbl$result == "W"]
      
      highScore <-
        str_sub(tbl[,3], 2, str_locate(tbl[,3], "-")[,1] - 1)
      tbl$scoreTeam[tbl$result == "W"] <-
        highScore[tbl$result == "W"]
      tbl$scoreOpp[tbl$result == "L"] <- highScore[tbl$result == "L"]
      
      tbl$scoreOpp <- as.integer(tbl$scoreOpp)
      tbl$scoreTeam <- as.integer(tbl$scoreTeam)
      
      # Create Postponed flag
      tbl$postponed <- tbl[,3] == "Postponed"
      
      # Create Total Record columns
      tbl$wins <-
        as.integer(str_sub(tbl[,4], 1, str_locate(tbl[,4], "-")[,1] - 1))
      tbl$losses <-
        as.integer(str_sub(tbl[,4], str_locate(tbl[,4], "-")[,1] + 1 , str_locate(tbl[,4], "[(]")[,1] - 2))
      
      # Create Conf wins/losses columns
      tbl$winsConf <-
        as.integer(str_sub(
          tbl[,4],
          start = str_locate(tbl[,4], "[(]")[,1] + 1 ,
          end = str_locate(tbl[,4], "-[0-9]*[)]")[,1] - 1
        ))
      tbl$lossesConf <-
        as.integer(str_sub(tbl[,4], str_locate(tbl[,4], "-")[,1] + 1 , str_locate(tbl[,4], "[(]")[,1] - 2))
      
      #If location is at Home, use home team arena as arena... otherwise use opponents!
      tbl$place[tbl$location == "home"] <- uni_names[i]
      tbl$place[tbl$location == "away"] <-
        tbl$opponent[tbl$location == "away"]
      
      #Remove unnecessary columns from table
      tbl <- tbl[,-c(2,3,4)]
      
      # We don't need these variables anymore
      remove(dates, lastGameIdx, year, loc, lowScore, highScore)
    
      
      espnTbls[[i]] <- tbl
      
      #make attributes for easy info gathering
      attr(espnTbls[[i]], 'year') <- year
      attr(espnTbls[[i]], 'team') <- uni_names[i]
    }
    save(espnTbls, file = "data/espnTbls")
  }


# Get opponent names listed in the schedules... ---------------------------

get_opponent_names <- function() {
  opponent_names <- vector(mode = "character")
  opponent_names <- sapply(espnTbls, function(x) {c(opponent_names, x$opponent)} )
  opponents <- unique(unlist(opponent_names))
  opponents <- opponents[!(str_detect(opponents,
                                        pattern = "CBI"))]
  opponents <- opponents[!(str_detect(opponents,
                                      pattern = "CIT"))]
  opponents <- opponents[!(str_detect(opponents,
                                      pattern = "NIT"))]
  opponents <- opponents[!(str_detect(opponents,
                                      pattern = "VEGAS"))]
  opponents <- opponents[order(opponents)]
  
  
  nms1 <- NCAA_Locations$School
  university_names <- c(opponents, NCAA_Locations$School)
  write.csv(university_names, file="university_names.csv")
  
}

