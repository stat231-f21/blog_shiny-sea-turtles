# wrangling for network

library(tidyverse)
library(kableExtra)
library(tidyr)
library(dplyr)

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