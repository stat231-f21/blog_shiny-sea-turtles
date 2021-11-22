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

shows_info <- url_html %>%
  html_elements("div p ") %>%
  html_text() %>%
  tolower()

shows_data <- tibble(show = shows)

# Missing some show names

shows_info_data <- tibble(show_info = shows_info)

shows_info_data1 <- data.frame(show_info = unlist(strsplit(as.character(shows_info_data$show_info), "number of viewers: ")))
shows_info_data1 <- data.frame(show_info = unlist(strsplit(as.character(shows_info_data1$show_info), "number of viewers:")))

shows_info_data1$show_info <- gsub("~", "", as.character(shows_info_data1$show_info))
shows_info_data1$show_info <- gsub(" million", ",000,000", as.character(shows_info_data1$show_info))
shows_info_data1$show_info <- gsub("\\,000,00\\b", ",000,000", as.character(shows_info_data1$show_info))
shows_info_data1 <- shows_info_data1[!grepl("type", shows_info_data1$show_info),]

shows_info_data1 <- tibble(show_info = shows_info_data1)

## get weird numbers too

shows_info_data1 <- gsub("(600,000).*","\\1",shows_info_data1$show_info)

shows_info_data1 <- tibble(show_info = shows_info_data1)
shows_info_data1 <- gsub("(000,000).*","\\1",shows_info_data1$show_info)

shows_info_data1 <- tibble(show_info = shows_info_data1)

shows_info_data_new <- str_extract(shows_info_data1$show_info, 
                                   ".*[:digit:]$")

shows_info_data_final <- tibble(shows_info_data_new = shows_info_data_new)

shows_info_data_final <- shows_info_data_final%>%
  drop_na()

shows_info_data_final  <- shows_info_data_final [-c(151:161, 37, 39, 43, 46, 51, 
                                                    53, 55, 57, 59, 61, 63, 64, 
                                                    66, 68, 70, 75, 77, 82, 84, 
                                                    85, 98, 99, 100, 102, 103, 
                                                    105, 107, 113, 115, 117, 
                                                    118, 122, 124, 126, 128, 
                                                    130, 136, 142), ]

# reading in csv

netflix_shows <- read_csv("data/netflix_titles.csv")
netflix_subs <- read_csv("data/revenue_subscriber_data.csv")
bridgerton <- read.csv("data/bridgerton.csv", sep=",", header = FALSE)
squidgame <- read.csv("data/squidgame.csv", sep=",", header = FALSE)
moneyheist <- read.csv("data/moneyheist.csv", sep=",", header = FALSE)
strangerthings <- read.csv("data/strangerthings.csv", sep=",", header = FALSE)
thirteenreasons <- read.csv("data/thirteenreasons.csv", sep=",", header = FALSE)
all_stream_shows <- read_csv("data/MoviesOnStreamingPlatforms_updated.csv")

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

# change some country names to match country names in world map data
shows_popularity_master$ID[shows_popularity_master$ID 
                           == "Myanmar (Burma)"] <- "myanmar"
shows_popularity_master$ID[shows_popularity_master$ID 
                           == "United States"] <- "usa"
shows_popularity_master$ID[shows_popularity_master$ID 
                           == "United Kingdom"] <- "uk"
shows_popularity_master$ID[shows_popularity_master$ID 
                     == "Bosnia & Herzegovina"] <- "bosnia and herzegovina"
shows_popularity_master$ID[shows_popularity_master$ID 
                           == "Trinidad & Tobago"] <- "trinidad"
shows_popularity_master$ID[shows_popularity_master$ID 
                           == "Czechia"] <- "czech republic"

# Duplicate Trinidad row to make a separate row for Tobago
shows_popularity_master <- rbind(shows_popularity_master, 
                                 shows_popularity_master[rep(8, 1), ])

# Rename ID of Trinidad to Tobago to have a row for each of them
shows_popularity_master$ID[251] <- "tobago"

# Make the IDs lowercase to match other dataset
shows_popularity_master$ID <- tolower(shows_popularity_master$ID)

# Set NAs to be 0
shows_popularity_master[is.na(shows_popularity_master)] <- 0

# Convert data to long format
shows_popularity_master_long <- shows_popularity_master %>%
  pivot_longer(
    cols = "Bridgerton" : "ThirteenReasonsWhy",
    names_to = "Most_Popular_Show",
    values_to = "PopularityScore")

# Get most popular show for each country
most_popular_show <- shows_popularity_master_long %>% 
  group_by(ID) %>% 
  top_n(1, PopularityScore) %>%
  slice(which.max(PopularityScore))

# Delete countries with a most popular show with a score of 0
most_popular_show <- most_popular_show[most_popular_show$PopularityScore != 0, ]

# Join most popular show and all shows datasets
shows_popularity_full <- most_popular_show %>% 
  left_join(shows_popularity_master)

# Join world map and popular show datasets
netflix_popular_show_map <- world_map %>%
  inner_join(shows_popularity_full, by = "ID")

# rename ID
netflix_popular_show_map <- netflix_popular_show_map %>%
  rename(Country = ID) 

# write most_popular_show to csv (cant write geom to csv?)
write.csv(shows_popularity_full, file = 'most_popular_show_map.csv')

## Shows/Films on all streaming platforms dataset ##

all_stream_shows <- all_stream_shows %>%
  # Rename some variables
  rename(RottenTomatoes = 'Rotten Tomatoes', 
         PrimeVideo = 'Prime Video', DisneyPlus = 'Disney+') %>%
  # Kepp select variables
  select(Year, Title, IMDb, RottenTomatoes, Netflix, Hulu, PrimeVideo, 
         DisneyPlus, Genres)

# Remove /10 and /100 from IMDb and RottenTomatoes ratings variables
all_stream_shows$IMDb <- 
  str_sub(all_stream_shows$IMDb, 1, nchar(all_stream_shows$IMDb) - 3)
all_stream_shows$RottenTomatoes <- 
  str_sub(all_stream_shows$RottenTomatoes, 1, 
          nchar(all_stream_shows$RottenTomatoes) - 4)

all_stream_shows <- all_stream_shows %>%
  # Make RottenTomatoes and IMDb ratings seen as numeric
  mutate(IMDb = as.numeric(IMDb), RottenTomatoes = as.numeric(RottenTomatoes),
         # Create new variable that says which streaming service each show is on 
         Service = case_when((Netflix == 1 & Hulu == 0 & PrimeVideo == 0 
                              & DisneyPlus == 0 ~ "Netflix"),
                          (Hulu == 1 & Netflix == 0 & PrimeVideo == 0 
                           & DisneyPlus == 0 ~ "Hulu"),
                          (PrimeVideo == 1 & Hulu == 0 & Netflix == 0 
                           & DisneyPlus == 0 ~ "Prime Video"),
                          (DisneyPlus == 1 & Hulu == 0 & PrimeVideo == 0 
                           & Netflix == 0 ~ "Disney Plus"),
                          (DisneyPlus == 1 & Hulu == 1 & PrimeVideo == 0 
                           & Netflix == 0 ~ "Disney Plus, Hulu"),
                          (DisneyPlus == 1 & Hulu == 0 & PrimeVideo == 1 
                           & Netflix == 0 ~ "Disney Plus, Prime Video"),
                          (DisneyPlus == 1 & Hulu == 0 & PrimeVideo == 0 
                           & Netflix == 1 ~ "Disney Plus, Netflix"),
                          (DisneyPlus == 0 & Hulu == 1 & PrimeVideo == 1 
                           & Netflix == 0 ~ "Hulu, Prime Video"),
                          (DisneyPlus == 0 & Hulu == 1 & PrimeVideo == 0 
                           & Netflix == 1 ~ "Hulu, Netflix"),
                          (DisneyPlus == 0 & Hulu == 0 & PrimeVideo == 1 
                           & Netflix == 1 ~ "Prime Video, Netflix"),
                          (DisneyPlus == 1 & Hulu == 1 & PrimeVideo == 1 
                           & Netflix == 0 ~ "Disney Plus, Hulu, Prime Video"),
                          (DisneyPlus == 1 & Hulu == 1 & PrimeVideo == 0 
                           & Netflix == 1 ~ "Disney Plus, Hulu, Netflix"),
                          (DisneyPlus == 1 & Hulu == 0 & PrimeVideo == 1 
                           & Netflix == 1 ~ "Disney Plus, Prime Video, Netflix"),
                          (DisneyPlus == 0 & Hulu == 1 & PrimeVideo == 1
                           & Netflix == 1 ~ "Hulu, Prime Video, Netflix"),
                          (DisneyPlus == 1 & Hulu == 1 & PrimeVideo == 1 
                           & Netflix == 1 
                           ~ "Disney Plus, Hulu, Prime Video, Netflix")))

# Make a duplicate row for each show that is on multiple platforms
# One platform per row
platforms_all <- all_stream_shows %>%
  mutate(Service = strsplit(as.character(Service), ", ")) %>% 
  unnest(Service)

# Make a duplicate row for each show that has multiple genres
# One genre per row
platforms_all_genre <- platforms_all %>%
  mutate(Genres = strsplit(as.character(Genres), ",")) %>% 
  unnest(Genres)

# Get average IMDb rating for each platform by genre
platforms_all_genre_rating <- platforms_all_genre %>%
  na.omit(platforms_all_genre$IMDb, platforms_all_genre$Genres) %>%
  group_by(Service, Genres) %>%
  summarise_at(vars(IMDb), list(IMDb = mean))

# Make each average rating have only two decimal points
platforms_all_genre_rating$IMDb <- 
  format(round(platforms_all_genre_rating$IMDb, 2), nsmall = 2)

# Make IMDb a numeric variable
platforms_all_genre_rating <- platforms_all_genre_rating %>%
  mutate(IMDb = as.numeric(IMDb))

# write platforms_all_genre_rating to csv
write.csv(platforms_all_genre_rating, file = 'platforms_all_genre_rating.csv')

# Get average IMDb rating for each platform
platforms_all_avg_IMDb_rating <- platforms_all %>%
  na.omit(platforms_all$IMDb) %>%
  group_by(Service) %>%
  summarise_at(vars(IMDb), list(IMDb = mean))

# Make each average rating have only two decimal points
platforms_all_avg_IMDb_rating$IMDb <- 
  format(round(platforms_all_avg_IMDb_rating$IMDb, 2), nsmall = 2)

# Make IMDb a numeric variable
platforms_all_avg_IMDb_rating <- platforms_all_avg_IMDb_rating %>%
  mutate(IMDb = as.numeric(IMDb))

# write platforms_all_genre_rating to csv
write.csv(platforms_all_avg_IMDb_rating, 
          file = 'platforms_all_avg_IMDb_rating.csv')
  
# Get average Rotten Tomatoes rating for each platform
platforms_all_avg_Rotten_rating <- platforms_all %>%
  na.omit(platforms_all$RottenTomatoes) %>%
  group_by(Service) %>%
  summarise_at(vars(RottenTomatoes), list(RottenTomatoes = mean))

# Make each average rating have only two decimal points
platforms_all_avg_Rotten_rating$RottenTomatoes <- 
  format(round(platforms_all_avg_Rotten_rating$RottenTomatoes, 2), nsmall = 2)

# Make RottenTomatoes a numeric variable
platforms_all_avg_Rotten_rating <- platforms_all_avg_Rotten_rating %>%
  mutate(RottenTomatoes = as.numeric(RottenTomatoes))

# write platforms_all_avg_Rotten_rating to csv
write.csv(platforms_all_avg_Rotten_rating, 
          file = 'platforms_all_avg_Rotten_rating.csv')

ggplotly(ggplot(data = platforms_all_avg_IMDb_rating, 
                aes(x = Service, y = IMDb)) +
           geom_bar(stat='identity', fill = "#FF6666") +
           labs(title = "Average IMDb Ratings of Shows By Platform",
                x = "Platform",
                y = "Average IMDb Rating (Out of 10)"))

ggplotly(ggplot(data = platforms_all_avg_Rotten_rating, 
                aes(x = Service, y = RottenTomatoes)) +
           geom_bar(stat='identity', fill = "#FF6666") +
           labs(title = "Average Rotten Tomatoes Ratings of Shows By Platform",
                x = "Platform",
                y = "Average IMDb Rating (Out of 10)"))

ggplotly(ggplot(data = platforms_all_genre_rating, 
                aes(x = Genres, y = IMDb, fill = Service)) +
           geom_bar(stat='identity') +
           labs(title = "Average IMDb Ratings of Shows By Genre",
                x = "Genre",
                y = "Average IMDb Rating (Out of 10)"))

