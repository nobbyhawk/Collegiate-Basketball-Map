

#showschedule > div > table > tbody > tr.oddrow.team-41-213 > td:nth-child(2) > ul > li.team-name > a

library(rvest)
html <- read_html("http://www.espn.com/mens-college-basketball/team/schedule/_/id/399/albany-great-danes")
nodes <- html_node(html, "#showschedule > div > table > tbody > tr.oddrow.team-41-213 > td:nth-child(2) > ul > li.team-name > a
")
nodes


head(html_attr(html_nodes(html, "a"), "href"))

html_attr(html_nodes(html, "a"), "href")



# Read in different types of data -----------------------------------------



require(rvest)
require(stringr)

#Just read in text
plainText <- read_html("https://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/") %>%
  html_nodes(".fn") %>%
  html_text()

#Read in the links that the names point to
links <- read_html("https://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/") %>%
  html_nodes(".fn") %>%
  sapply(FUN = as.character)

#Read in English Premier Club names
url <- "https://www.premierleague.com/clubs"
footballClubs <- read_html(url) %>%
  html_nodes(".clubName") %>%
  html_text()

#Read in Club Page URLS
url <- "https://www.premierleague.com/clubs"
clubPages <- read_html(url) %>%
  html_nodes(xpath = "//li")

str(clubPages[1])
as.character(clubPages[1])



# ESPN Example for writeup ------------------------------------------------
require("rvest")
url <- "http://www.espn.com/mens-college-basketball/team/schedule/_/id/152/nc-state-wolfpack"


#Store the webpage’s html in espnHtml
espnHtml <- read_html(url)


print(espnHtml)

#Example for rvest
espnNodes <- html_nodes(espnHtml, "#showschedule > div > table")
print(espnNodes)

espnTable <- html_table(espnNodes,header = TRUE)
str(espnTable[[1]])

str(html_text(espnNodes))
