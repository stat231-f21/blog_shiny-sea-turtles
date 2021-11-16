library(tidyverse)
library(kableExtra)
library(ggnetwork)
library(igraph)

final5 <- final4 %>%
  select(title, people) %>%
  table() %>%
  crossprod() 

diag(final5) <- 0
df <- as.data.frame(final5)

final3 <- final3 %>%
  left_join(final2, by = "people") 
final3 <- final3 %>% 
  select(people, role, n) %>% 
  unique()

final3 <- final3[-c(15, 17, 30, 35, 40, 42, 44, 48, 78, 89, 94, 99, 109, 119, 141, 153, 157, 160, 168, 173, 178, 209, 210, 214, 217), ]


ve <- final3
ed <- df %>%
  mutate(from = rownames(.)) %>%
  tidyr::gather(to, weight, 1:nrow(final5)) %>%
  mutate(weight = ifelse(weight == 0, NA, weight)) %>% 
  filter(from %in% final3$people) %>% 
  filter(to %in% final3$people)


people_igraph <- graph_from_data_frame(d = ed, 
                                       vertices = ve,
                                       directed = FALSE) %>%
  simplify()

people_network <- ggnetwork(people_igraph)


ggplot(data = people_network, aes(x = x, y = y, 
                                  xend = xend, yend = yend)) +
  geom_edges(aes(size = weight), color = "lightgrey", curvature = 0.1, ) +
  geom_nodes(aes(size = n, color = role)) +
  geom_nodetext_repel(aes(label = name)) +
  theme_blank() +
  labs(color = "role", caption = "Actor - Director Relationships")