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
bridgerton <- read.csv("data/bridgerton.csv", sep=",", header = FALSE)
squidgame <- read.csv("data/squidgame.csv", sep=",", header = FALSE)
moneyheist <- read.csv("data/moneyheist.csv", sep=",", header = FALSE)
strangerthings <- read.csv("data/strangerthings.csv", sep=",", header = FALSE)
thirteenreasons <- read.csv("data/thirteenreasons.csv", sep=",", header = FALSE)

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
# Make Number of Subscribers be in thousands of people
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

## Netflix show popularity datasets ##

# Rename variables
bridgerton <- bridgerton %>%
  rename(ID = V1, Bridgerton = V2)
squidgame <- squidgame %>%
  rename(ID = V1, SquidGame = V2)
moneyheist <- moneyheist %>%
  rename(ID = V1, MoneyHeist = V2)
strangerthings <- strangerthings %>%
  rename(ID = V1, StrangerThings = V2)
thirteenreasons <- thirteenreasons %>%
  rename(ID = V1, ThirteenReasonsWhy = V2)

# Delete first two rows of each set
bridgerton <- bridgerton[-c(1, 2), ]
squidgame <- squidgame[-c(1, 2), ]
moneyheist <- moneyheist[-c(1, 2), ]
strangerthings <- strangerthings[-c(1, 2), ]
thirteenreasons <- thirteenreasons[-c(1, 2), ]

# Join datasets of show popularity
shows_popularity_master <- bridgerton %>% 
  left_join(squidgame) %>%
  left_join(., moneyheist) %>%
  left_join(., strangerthings) %>%
  left_join(., thirteenreasons)

shows_popularity_master <- shows_popularity_master %>%
  # Convert variables to be treated as numbers instead of characters
  mutate(Bridgerton = as.numeric(Bridgerton), SquidGame = as.numeric(SquidGame), 
         MoneyHeist = as.numeric(MoneyHeist), 
         StrangerThings = as.numeric(StrangerThings), 
         ThirteenReasonsWhy = as.numeric(ThirteenReasonsWhy))

# Set NAs to be 0
shows_popularity_master[is.na(shows_popularity_master)] <- 0

# Convert data to long format
shows_popularity_master_long <- shows_popularity_master %>%
  pivot_longer(
    cols = "Bridgerton" : "ThirteenReasonsWhy",
    names_to = "Show",
    values_to = "PopularityScore")

# Get most popular show for each country
most_popular_show <- shows_popularity_master_long %>% 
  group_by(ID) %>% 
  top_n(1, PopularityScore) %>%
  slice(which.max(PopularityScore))

# Delete countries with a most popular show with a score of 0
most_popular_show <- most_popular_show[most_popular_show$PopularityScore != 0, ]

# change country names to match country names in world map data
most_popular_show$ID[most_popular_show$ID == "Myanmar (Burma)"] <- "myanmar"
most_popular_show$ID[most_popular_show$ID == "United States"] <- "usa"
most_popular_show$ID[most_popular_show$ID == "United Kingdom"] <- "uk"
most_popular_show$ID[most_popular_show$ID 
                     == "Bosnia & Herzegovina"] <- "bosnia and herzegovina"
most_popular_show$ID[most_popular_show$ID == "Trinidad & Tobago"] <- "trinidad"
most_popular_show$ID[most_popular_show$ID == "Czechia"] <- "czech republic"

# Duplicate Trinidad row to make a separate row for Tobago
most_popular_show <- rbind(most_popular_show, most_popular_show[rep(81, 1), ])

# Rename ID of Trinidad to Tobago to have a row for each of them
most_popular_show$ID[94] <- "tobago"

# Make the IDs lowercase to match other dataset
most_popular_show$ID <- tolower(most_popular_show$ID)

# Join world map and popular show datasets
netflix_popular_show_map <- world_map %>%
  inner_join(most_popular_show, by = "ID")

# rename ID
netflix_popular_show_map <- netflix_popular_show_map %>%
  rename(Country = ID)

# write most_popular_show to csv (cant write geom to csv?)
write.csv(most_popular_show, file = 'most_popular_show_map.csv')


