# data wrangling for shiny app

library(tidyverse)
library(kableExtra)

movies <- read_csv("data/MoviesOnStreamingPlatforms_updated.csv")

mtables <- movies %>%
  select(Title, Year, Age, IMDb, `Rotten Tomatoes`, Netflix, Hulu, `Prime Video`, `Disney+`) %>%
  arrange(Year)