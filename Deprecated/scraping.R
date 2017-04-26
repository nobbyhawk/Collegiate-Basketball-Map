# scraping.R
# Double overtime bug***

#NOT COUNTED HOURS (in book)
#One hour Monday, 9/26
#One hour Tuesday, 9/27
#One Hour Meeting on Wednesday, 9/28


## Scrape Data -------------------------------------------------------------

espn_scrape <-
  function(year = lubridate::year(lubridate::today())) {
    
    #required packages
    if (!require(rvest))
      install.packages("rvest")
    if (!require(XML))
      install.packages("XML")
    if (!require(dplyr))
      install.packages("dplyr")
    if (!require(stringr))
      install.packages("stringr")
    if (!require(lubridate))
      install.packages("lubridate")
    
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
    
    #get all the team ids in the url
    team_ids <- as.character(str_sub(
      team_htmls,
      start = str_locate(team_htmls, pattern = "_/id/")[, 2] + 1,
      end = str_locate(team_htmls, pattern = "_/id/[0-9]*")[, 2]
    ))
    
    
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
    
    
    # Read in arenas data
    arenas <-
      read.csv(
        "C:/Users/Christopher/Desktop/School/Research 2/PracticeMap/PracticeMap/arenas.csv"
      )
    names(arenas) <- c("lng", "lat", "place")
    
    
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
    tbl$place[tbl$location == "home"] <- team
    tbl$place[tbl$location == "away"] <-
      tbl$opponent[tbl$location == "away"]
    
    #Remove unnecessary columns from table
    tbl <- tbl[,-c(2,3,4)]
    
    # We don't need these variables anymore
    remove(dates, lastGameIdx, year, loc, lowScore, highScore)
    
    espnTbls[[i]] <- tbl
    }
    espnTbls
  }
