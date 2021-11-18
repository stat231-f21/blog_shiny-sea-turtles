# data wrangling for shiny app

library(tidyverse)
library(kableExtra)

mtables <- movies %>%
  select(Title, Year, Age, IMDb, `Rotten Tomatoes`, Netflix, Hulu, `Prime Video`, `Disney+`) %>%
  arrange(Year)