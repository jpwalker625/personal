library(rvest)
library(tidyverse)
library(stringr)
library(purrr)
library(rebus)
library(lubridate)

chart <- as.tibble()
seasons <- 1:6

imdbScrape <- function(x){
  page <- paste0("https://www.imdb.com/title/tt2149175/episodes?season=", x)
  name <- page %>% read_html() %>% html_nodes('#episodes_content strong a') %>% html_text() %>% as.data.frame()
  rating <- page %>% read_html() %>% html_nodes('.ipl-rating-widget') %>% html_text() %>% as.data.frame()
  details <- page %>% read_html() %>% html_nodes('.zero-z-index div') %>% html_text() %>% as.data.frame()
  date <- page %>% read_html() %>% html_nodes('.airdate') %>% html_text() %>% as.data.frame()
  
  temp <- cbind(name, rating, details, date)
  names(temp) <- c("Name", "Rating", "Details", "Date")
  chart <- rbind(chart, temp)
  return(chart)
  Sys.sleep(5)
}

#iterate over all 6 seasons
americans <- map_df(.x = seasons, .f = imdbScrape)

#Parse out columns
americans <- americans %>%
  #separate the details into season and episode
  separate(col = Details, into = c("Season", "Episode"), sep = ", ") %>%
  #convert Date to date format, extract the rating & votes, drop characters from seasons and votes
  mutate(Date =  dmy(str_extract(americans$Date, pattern = "\\b.*")),
         Rating = as.numeric(str_extract(string = americans$Rating, pattern = "\\w.*")),
         Votes = str_extract(string = americans$Rating, pattern = "(?<=\\().*(?=\\))"),
         Votes = as.integer(str_remove(temp$Votes, pattern = ",")))

americans_clean <- americans %>%
  mutate(Episode = as.factor(str_extract(string = americans$Episode, pattern = "\\d")),
         Season = as.factor(str_extract(string = americans$Season, pattern = "\\d")))



