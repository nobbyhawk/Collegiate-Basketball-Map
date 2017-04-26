# travelLines.r


####################################################################
####################################################################
#                                                                  #
# This program creates the travel lines for a given team's schedule#
# Goals:                                                           #
#         1. Create lines                                          #
#         2. Make it accessible by date/game                       #
#         3. Make into function of some sort, just to wrap it nicely
#         4. Fix university names in table                         #
####################################################################
####################################################################

# Note: Used Wikipedia to get college locations...
# https://en.wikipedia.org/wiki/List_of_NCAA_Division_I_institutions


## HOURS: 1 hour 10/10 creating functions
##        1 hour 10/11 scraping wikipedia
##        1 hour 10/12 object storage/travel lines
##        1.5 hour 10/19 travel lines
##        1 hour 10/19 meeting



# # Create Travel Lines -----------------------------------------------------
# 
# # Packages
# if(!require(devtools)) install.packages("devtools")
# if(!require(geosphere)) install.packages("geosphere")
# if(!require(lubridate)) install.packages("lubridate")
# if(!require(htmltools)) devtools::install_github("rstudio/htmltools")
# if(!require(htmlwidgets)) devtools::install_github("ramnathv/htmlwidgets")
# if(!require(leaflet)) devtools::install_github("rstudio/leaflet")
# 
# # Scrape schedules...takes a while to get all of them
# if(!file.exists("schedules")) {
#   source("scraping.R")
#   schedules <- espn_scrape(2015)
#   save(schedules, file = "schedules")
# }
# load("schedules")
# 
# # Load college locations.. takes a while because queries google maps
# if(!file.exists("NCAA_Locations")) loc_wiki()
# locations <- read.csv("C:/Users/Christopher/Desktop/School/Research 2/PracticeMap/PracticeMap/Map Practice/NCAA_Locations",
#                       stringsAsFactors=FALSE)
# 
# 
# 
# 
# 
# # New Location Vector  -------------------------------------------------------------------
# 
# numGames <- nrow(schedules[[1]])
# 
# repeatLocation <- 0 #First game cannot be in a repeated location...
# for(i in 2:numGames){ #look for multiple home games or an away series at one school
#   repeatLocation[i] <- (schedules[[1]]$location[i] == "home" && schedules[[1]]$location[i - 1] == "home") |
#     schedules[[1]]$opponent[i] == schedules[[1]]$opponent[i - 1]
# }
# 
# 
# # Create lines ------------------------------------------------------------
# 
# lines <- data.frame("lon" = c(), "lat" = c())
# 
# schedules[[1]] <- merge(schedules, )
# 
# for (i in 2:numGames) {
#   if (!repeatLocation[i]) {
#     lines <- rbind(lines, geosphere::gcIntermediate(
#       p1 = c(full$lng[i - 1], full$lat[i - 1]),
#       p2 = c(full$lng[i], full$lat[i]),
#       n = 50
#     ))
#   }
#   
#   loc <- full$place[i]
# }
# 
# 
# 
# 
# #  ------------------------------------------------------------------------
# # This function will return a data structure that contain the travel lines
# # of a given teams schedule. 
# # TODO: What to do about Nuetral locations
# #  ------------------------------------------------------------------------
# 
# ## TODO: Need collegiate arena locations - use city locations as default?







# Line Generating Function ------------------------------------------------

#Takes ONE schedule and locations as parameters
#TODO - Take location as town or long/lat?

travelLines <- function(schedule, gameDate) {
  
  if(!require(lubridate)) install.packages("lubridate")
  
  # Get pre-scraped locations of universities
  NCAA_Locations <- read.csv("C:/Users/Christopher/Desktop/School/Research 2/Map/NCAA_Locations",
                             stringsAsFactors=FALSE)
  
  
  ##FOR NOW - ASSUME WE HAVE CORRECT LON/LAT
  ##TEMPORARILY USING GOLDEN SUN WARRIOR's DATA ----- REPLACE!!!!
  # TEMPORARY #
  ############################################################
  warriors_games <- read.csv("C:/Users/Christopher/Desktop/School/Research 2/PracticeMap/PracticeMap/warriors_games.csv")
  arenas <- read.csv("C:/Users/Christopher/Desktop/School/Research 2/PracticeMap/PracticeMap/arenas.csv")
  names(warriors_games)[3] <- "place"
  schedule <- warriors_games
  NCAA_Locations <- arenas
  #fix dates
  dates <- as.Date(schedule$DATE, format = "%a, %b %d") #Temp holder for our dates
  
  # Record index of the last game in January, we know each game after that
  # must occur in the following year!
  lastGameIdx <- which(lubridate::month(dates) == 1)[1] - 1
  
  lubridate::year(dates[1:lastGameIdx]) <-  2015L #First half of season is in the first year
  lubridate::year(dates[(lastGameIdx + 1):length(dates)]) <- 2016L
  schedule$DATE <- dates #Place cleaned data in data frame
  names(schedule)[2] <- "dates" #change to name used in normal table
  ############################################################
  # TEMPORARY #
  
  
  #We must determine when schools do not travel for subsequent games
  #only new locations will be graphed with a line
  numGames <- nrow(schedule)
  
  
  #Ammend the schedule to include the correct long/lat information
  #TEMPORARY!! WILL NEED TO CHANGE#
  schedule <- merge(schedule, NCAA_Locations, by = c("place") )
  schedule <- schedule[order(schedule$dates),]
  #TEMPORARY!! WILL NEED TO CHANGE#
  
  repeatLocation <- 0 #First game cannot be in a repeated location...
  for(i in 2:numGames){ #look for multiple home games or an away series at one school
    repeatLocation[i] <- (schedule$place[i] == schedule$place[i - 1])
  }
  
  # "lines" will hold the travel line information that will be returned
  lines <- data.frame("lon" = c(), "lat" = c(), "date" = c())
  rows <- 0
  
  for (i in 2:numGames) {
    if (!repeatLocation[i]) {
      #Create lines between new locations in the scedule
      lines <- rbind(lines, geosphere::gcIntermediate(
        p1 = c(schedule$target_lon[i - 1], schedule$target_lat[i - 1]),
        p2 = c(schedule$target_lon[i], schedule$target_lat[i]),
        n = 30
      ))
    }
  }
  
  #Add date in so we know when the team is traveling...
  allGameDates <- rep(schedule$DATE[which(!repeatLocation)], each = 30)
  allGameDates <- allGameDates[-c(1:30)]
  lines$date <- allGameDates
  
  write.csv(schedule, file = "warriors_temp")
  lines
}






# Collegiate Locations ----------------------------------------------------
# generates the longitude and latitude for each team's city
college_loc <- function() {
  if(!require("ggmap")) install.packages("ggmap")
  if(!require("rvest")) install.packages("rvest")
  if(!require("stringr")) install.packages("stringr")
  
  #Get University names, in order to geocode their location
  team_htmls <- as.character(
    read_html("http://www.espn.com/mens-college-basketball/teams") %>%
      html_nodes(".bi")
  )
  
  #Extract the university names from the urls
  uni_names <- str_sub(team_htmls,
                       start = str_locate(team_htmls, ">[A-z a-z &;'() [-]]*</a>")[,1] + 1,
                       end = str_locate(team_htmls, "</a>")[,1] - 1
                       )
  
  # "&" is replaced as "&amp;" -- this code fixes that
  uni_names <- str_replace( string = uni_names,
                            pattern = "&amp;",
                            replacement = "&")
  
  #Sort the universities by name
  uni_names <- sort(uni_names)
  
  
  
  # Generate long/lat of colleges
  coords <- ggmap::geocode(uni_names,messaging = F)
  
  
  data.frame("university" = uni_names,
             "lon" = locations[,1],
             "lat" = locations[,2]
             )
}


####################################################################################
# This function scrapes/cleans the NCAA Div I teams on the appropriate Wiki page   #
#                                                                                  #
# Output:    "NCAA_Locations.csv"                                                  #
# Variables:                                                                       #
#             School                                                               #
#             Team (basically mascot?)                                             #
#             City                                                                 #
#             State                                                                #
#             Primary.Conference                                                   #
#             City                                                                 #
#             lon                                                                  #
#             lat                                                                  #
####################################################################################
loc_wiki <- function() {
  ## Trying wikipedia table
  ## td , th
  
  #Load required packages
  if (!require("ggmap"))
    install.packages("ggmap")
  if (!require("rvest"))
    install.packages("rvest")
  if (!require("stringr"))
    install.packages("stringr")
  if (!require("dplyr"))
    install.packages("dplyr")
  
  # Get table of Div I Schools from wikipedia - cities of the university are listed
  team_cities <-
    read_html("http://en.wikipedia.org/wiki/List_of_NCAA_Division_I_institutions") %>%
    html_table(header = T, fill = T)
  
  
  # Their are two returned tables from wikipedia - we need to merge them
  # 1: Current teams
  # 2: Teams transitioning from DII -> DI this year
  
  uni_table <- rbind(team_cities[[1]], team_cities[[2]][,1:5] )
  remove(team_cities) #Dont need this list anymore
  
  
  # They appear to have the same length (351), I am *assuming* ESPN and Wiki have same teams
  # Geocode(get longitude and latitude) of each school
  uni_table <- c(uni_table,
                 geocode(paste(uni_table$City, uni_table$State, sep = ", "))
  )
  uni_table <- as.data.frame(uni_table)
  
  
  #TODO: Get correct school names ***********************
  # Sort by team name...
  wiki_name_order <- read.csv("wiki_name_order.csv", sep="", stringsAsFactors=FALSE)
  uni_table <- uni_table[wiki_name_order,]
  
  
  #Save locations in local directory
  write.csv(uni_table, file = "NCAA_Locations")
}







