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

# Some preliminary data wrangling that builds off of what has already been done, but formats specific to use for making network

# read in csv files
#final3 <- read_csv("data/final3.csv")
#final4 <- read_csv("data/final4.csv")

# create a table of crossproducts between actors by movie
final5 <- final4 %>%
  select(title, people) %>%
  table() %>%
  crossprod() 

# set diagonal crossproducts (each actor working with themself) equal to 0, and make a new dataframe. 
diag(final5) <- 0
df <- as.data.frame(final5)

# left join to put the information in final3 along with all other variables in final2
final3 <- final3 %>%
  left_join(final2, by = "people") 
final3 <- final3 %>% 
  select(people, role, n) %>% 
  unique()

# get rid of duplicate names
# final3 <- final3[-c(15, 17, 30, 35, 40, 42, 44, 48, 78, 89, 94, 99, 109, 119, 141, 153, 157, 160, 168, 173, 178, 209, 210, 214, 217), ]

# create dataframes for igraph
ve <- final3
ed <- df %>% 
  mutate(from = rownames(.)) %>%
  tidyr::gather(to, weight, 1:nrow(final5)) %>%
  mutate(weight = ifelse(weight == 0, NA, weight)) %>% 
  filter(from %in% final3$people) %>% 
  filter(to %in% final3$people)

# create igraph object for data frame
people_igraph <- graph_from_data_frame(d = ed, 
                                       vertices = ve,
                                       directed = FALSE) %>%
  simplify()

  

  
