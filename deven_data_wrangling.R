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
                pattern = c(", ",",")) %>%
  # Drop NAs
  drop_na() %>%
  # Rename country to ID to match world map data
  rename(ID = country)

netflix_map$ID <- str_trim(netflix_map$ID, side = c("left"))

# change country names to match country names in world map data
netflix_map$ID[netflix_map$ID == "united kingdom"] <- "uk"
netflix_map$ID[netflix_map$ID == "united kingdom,"] <- "uk"
netflix_map$ID[netflix_map$ID == "united states"] <- "usa"
netflix_map$ID[netflix_map$ID == "east germany"] <- "germany"
netflix_map$ID[netflix_map$ID == "west germany"] <- "germany"
netflix_map$ID[netflix_map$ID == "soviet union"] <- "russia"
netflix_map$ID[netflix_map$ID == "hong kong"] <- "china"
netflix_map$ID[netflix_map$ID == "vatican city"] <- "vatican"
netflix_map$ID[netflix_map$ID == "cambodia,"] <- "cambodia"

# Get number of shows/movies by country
netflix_map_by_country <- netflix_map %>%
  group_by(ID) %>%
  summarize(number_of_films = n())

# write netflix_map_by_country to csv
write.csv(netflix_map_by_country, file = 'main_data/netflix_map_by_country.csv')

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

# write netflix_subs to csv
write.csv(netflix_subs, file = 'main_data/netflix_subs_map.csv')

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
bridgerton <- bridgerton %>% 
  slice(-c(1, 2))
squidgame <- squidgame %>% 
  slice(-c(1, 2))
moneyheist <- moneyheist %>% 
  slice(-c(1, 2))
strangerthings <- strangerthings %>% 
  slice(-c(1, 2))
thirteenreasons <- thirteenreasons %>% 
  slice(-c(1, 2))

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

# Rename most popular shows to be more user readable

shows_popularity_full$Most_Popular_Show[shows_popularity_full$Most_Popular_Show 
                             == "ThirteenReasonsWhy"] <- "Thirteen Reasons Why"
shows_popularity_full$Most_Popular_Show[shows_popularity_full$Most_Popular_Show 
                                      == "StrangerThings"] <- "Stranger Things"
shows_popularity_full$Most_Popular_Show[shows_popularity_full$Most_Popular_Show 
                                        == "MoneyHeist"] <- "Money Heist"
shows_popularity_full$Most_Popular_Show[shows_popularity_full$Most_Popular_Show 
                                        == "SquidGame"] <- "Squid Game"

# write most_popular_show to csv
write.csv(shows_popularity_full, file = 'main_data/most_popular_show_map.csv')

#############
# Bar Chart #
#############

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
write.csv(platforms_all_genre_rating, 
          file = 'main_data/platforms_all_genre_rating.csv')

# Get average Rotten Tomatoes rating for each platform by genre
platforms_all_genre_Rotten <- platforms_all_genre %>%
  na.omit(platforms_all_genre$RottenTomatoes, platforms_all_genre$Genres) %>%
  group_by(Service, Genres) %>%
  summarise_at(vars(RottenTomatoes), list(RottenTomatoes = mean))

# Make each average rating have only two decimal points
platforms_all_genre_Rotten$RottenTomatoes <- 
  format(round(platforms_all_genre_Rotten$RottenTomatoes, 2), nsmall = 2)

# Make RottenTomatoes a numeric variable
platforms_all_genre_Rotten <- platforms_all_genre_Rotten %>%
  mutate(RottenTomatoes = as.numeric(RottenTomatoes))

# write platforms_all_genre_Rotten to csv
write.csv(platforms_all_genre_Rotten, 
          file = 'main_data/platforms_all_genre_Rotten.csv')

