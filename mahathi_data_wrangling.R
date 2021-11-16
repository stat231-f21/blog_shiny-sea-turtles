# wrangling for network

library(tidyverse)
library(kableExtra)
library(tidyr)
library(dplyr)
library(stringr)

# read in csv
netflix_titles <- read_csv("data/netflix_titles.csv")

# filtering out observations that are NA for both cast and director, and then for cast
netflix_titles_1 <- netflix_titles %>%
  filter(!(is.na(director) & is.na(cast))) %>%
  filter(!is.na(cast))

# create separate rows for each director / cast member
netflix_titles_1 <- netflix_titles_1 %>%
  mutate(director = strsplit(as.character(director), ",")) %>%
  unnest(director)

netflix_titles_1 <- netflix_titles_1 %>%
  mutate(cast = strsplit(as.character(cast), ",")) %>%
  unnest(cast)

# create dataframe for directors
directors <- netflix_titles_1 %>%
  # select(-cast) %>%
  rename(people = "director") %>%
  mutate(role = "director")

# create dataframe for actors

actors <- netflix_titles_1 %>%
  # select(-director)
  rename(people = "cast") %>%
  mutate(role = "cast")

final <- bind_rows(directors, actors)

final2 <- final %>% arrange(title) %>%
  filter(!is.na(people)) %>%
  select(-c(cast,director)) %>%
  distinct()
  
# remove white space before each name
final2$people <- trimws(final2$people, which = c("left"))

# create a new dataset that only includes the actors/directors with the top 20 number of connections
final3 <- final2 %>%
  count(people, sort = TRUE) %>%
  slice(1:20)

final4 <- final2 %>%
  right_join(final3, by = "people")

write_csv(final3, "data/final3.csv")
write_csv(final4, "data/final4.csv")
  

  
