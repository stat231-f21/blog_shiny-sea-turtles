# data wrangling for shiny app

library(tidyverse)
library(kableExtra)

movies <- read_csv("update_all_platforms.csv")

mtables <- movies %>%
  select(Year, Title, IMDb, RottenTomatoes, Service) 