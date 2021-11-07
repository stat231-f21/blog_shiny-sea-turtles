library(tidyverse)
library(kableExtra)
library(robotstxt) 
library(rvest) 
library(purrr)
library(janitor)
library(tidytext)
library(wordcloud)
library(textdata)
library(sf)
library(leaflet)
library(kableExtra)
library(viridis)
library(plotly)

# scraping

url <- "https://www.whats-on-netflix.com/news/every-viewing-statistic-netflix-has-released-so-far-october-2021/"

paths_allowed(url)

url_html <- read_html(url)

shows <- url_html %>%
  html_elements("h4 em") %>%
  html_text()

shows_labels <- url_html %>%
  html_elements("p strong") %>%
  html_text()

shows_info <- url_html %>%
  html_elements("div p") %>%
  html_text()

shows_data <- tibble(show = shows)
shows_label_data <- tibble(show_labels = shows_labels)
shows_info_data <- tibble(show_info = shows_info)

# reading in csv

netflix_shows <- read_csv("data/netflix_titles.csv")
netflix_subs <- read_csv("data/revenue_subscriber_data.csv")

########
# Maps #
########

## Netflix shows dataset ##
netflix_map <- netflix_shows %>%
  # filter out all variables except country, show title, type, and ID 
  select(show_id, type, title, country) %>%
  # If movies have multiple countries, make a new row for each country
  unnest_tokens(output = country, input = country, token = "regex", 
                pattern = c(", ")) %>%
  # Drop NAs
  drop_na() %>%
  # Rename country to ID to match world map data
  rename(ID = country)

# change country names to match country names in world map data
netflix_map$ID[netflix_map$ID == "united kingdom"] <- "uk"
netflix_map$ID[netflix_map$ID == "united states"] <- "usa"
netflix_map$ID[netflix_map$ID == "east germany"] <- "germany"
netflix_map$ID[netflix_map$ID == "west germany"] <- "germany"
netflix_map$ID[netflix_map$ID == "soviet union"] <- "russia"
netflix_map$ID[netflix_map$ID == "hong kong"] <- "china"
netflix_map$ID[netflix_map$ID == "vatican city"] <- "vatican"

# Get number of shows/movies by country
netflix_map_by_country <- netflix_map %>%
  group_by(ID) %>%
  summarize(number_of_films = n())

# load in world map data
world_map <- maps::map("world", plot = FALSE, fill = TRUE) %>% 
  st_as_sf()

# Make the IDs lowercase to match other dataset
world_map$ID <- tolower(world_map$ID)

# join world map and movies/shows by country datasets
netflix_map_shows <- world_map %>%
  inner_join(netflix_map_by_country, by = "ID")

# rename ID and number_of_viewers
netflix_map_shows <-
  rename(Country = ID, 'Amount_of_Content' =  number_of_films)

# dataset for Netflix shows by country with coordinates: netflix_map_shows

# write netflix_map_by_country to csv (cant write geom to csv?)
write.csv(netflix_map_by_country, file = 'netflix_map_by_country.csv')

## Netflix subs/revenue dataset ##

# Rename country to ID to match world map data
netflix_subs <- netflix_subs %>%
  rename(ID = Country)

# change country names to match country names in world map data
netflix_subs$ID[netflix_subs$ID == "UAE"] <- "united arab emirates"
netflix_subs$ID[netflix_subs$ID == "United States"] <- "usa"
netflix_subs$ID[netflix_subs$ID == "United Kingdom"] <- "uk"
netflix_subs$ID[netflix_subs$ID == "Slovak Republic"] <- "slovakia"

# Make the IDs lowercase to match other dataset
netflix_subs$ID <- tolower(netflix_subs$ID)

# Make all variable names have _ instead of spaces
names(netflix_subs)<- str_replace_all(names(netflix_subs), c(" " = "_", 
                                                             "#" = "Number")) 

netflix_subs <- netflix_subs %>%
  mutate(Number_of_Subscribers = Number_of_Subscribers_Q2_2021/1000)

# Join world map and subs/revenue datasets
netflix_subs_map <- world_map %>%
  inner_join(netflix_subs, by = "ID")

# rename ID
netflix_subs_map <- netflix_subs_map %>%
  rename(Country = ID)

# write netflix_subs to csv (cant write geom to csv?)
write.csv(netflix_subs, file = 'netflix_subs_map.csv')


