library(tidyverse)
library(kableExtra)
library(ggnetwork)
library(igraph)

network_df <- final2 %>%
  select(people, role, title)

people_igraph <- graph_from_data_frame(network_df, 
                                       directed = FALSE)

people_network <- ggnetwork(people_igraph)

ggplot(data = people_network, aes(x = x, y = y, 
                                     xend = xend, yend = yend)) +
  geom_edges(color = "lightgrey") +
  geom_nodes() +
  geom_nodelabel(aes(label = name)) +
  theme_blank()