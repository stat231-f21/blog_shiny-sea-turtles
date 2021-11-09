library(tidyverse)
library(kableExtra)
library(janitor)
library(tidytext)
library(wordcloud)
library(textdata)

# wrangling

shows <- read_csv("data/netflix_titles.csv") %>% 
  select(title, description)


description_all <- shows %>%
  unnest_tokens(output = word, input = description)

# d_ngrams <- description %>%
#   unnest_tokens(output = bigram, input = word,
#                 token = "ngrams", n = 2)

data(stop_words)

# First, take a look at the stop_words dataset
# head(stop_words)
# tail(stop_words)

stop_words %>% 
  count(lexicon)

# Create new dataset since we are removing words
d_words <- description %>%
  anti_join(stop_words, by = "word")

# Explore which stop words were removed
## If you don't want all these words removed, you can modify 
## the stop_words dataframe before `anti_join`ing 
removed2 <- description %>%
  anti_join(d_words, by = "word") %>%
  count(word) %>%
  arrange(word)

d_words %>%
  count(word, sort = TRUE) %>% 
  slice(1:10) %>%
  ggplot(aes(x = reorder(word, n), y = n, 
             color = word, fill = word)) +
  geom_col() +
  coord_flip() +
  guides(color = "none", fill = "none") +
  labs(x = NULL,
       y = "Number of instances",
       title = "The most common words in\nNetflix Descriptions")

desc_frequencies <- shows %>%
  unnest_tokens(output = word, input = description) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) 

# Word cloud will rearrange each time unless seed is set
set.seed(53)

# choose color palette from color brewer
mypal <- brewer.pal(10, "Paired")

wordcloud(words = desc_frequencies$word, 
          freq = desc_frequencies$n,
          min.freq = 20,
          max.words = 75,
          # plot the words in a random order
          random.order = TRUE,
          # specify the range of the size of the words
          scale = c(2, 0.3),
          # specify proportion of words with 90 degree rotation
          rot.per = 0.15,
          # colors words from least to most frequent
          colors = mypal,
          # font family
          family = "sans")
