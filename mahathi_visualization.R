library(tidyverse)
library(kableExtra)
library(ggnetwork)
library(igraph)

# read in csv files

final3 <- read_csv("data/final3.csv")
final4 <- read_csv("data/final4.csv")

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
final3 <- final3[-c(15, 17, 30, 35, 40, 42, 44, 48, 78, 89, 94, 99, 109, 119, 141, 153, 157, 160, 168, 173, 178, 209, 210, 214, 217), ]

# create dataframes for igraph
ve <- final3
ed <- df %>%
  mutate(from = rownames(.)) %>%
  tidyr::gather(to, weight, 1:nrow(final5)) %>%
  mutate(weight = ifelse(weight == 0, NA, weight)) %>% 
  filter(from %in% final3$people) %>% 
  filter(to %in% final3$people)

# start creating igraph
people_igraph <- graph_from_data_frame(d = ed, 
                                       vertices = ve,
                                       directed = FALSE) %>%
  simplify()

people_network <- ggnetwork(people_igraph)

# write into csv so the entire code doesnt have to be run every time.
write_csv(people_network, file = 'people_network.csv')

# visualization
ggplot(data = people_network, aes(x = x, y = y, 
                                  xend = xend, yend = yend)) +
  geom_edges(aes(size = weight), color = "lightgrey", curvature = 0.1, ) +
  geom_nodes(aes(size = n, color = role)) +
  geom_nodetext_repel(aes(label = name)) +
  theme_blank() +
  labs(color = "role", caption = "Actor - Director Relationships")