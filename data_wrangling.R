# scraping

url <- "https://www.whats-on-netflix.com/news/every-viewing-statistic-netflix-has-released-so-far-october-2021/"

paths_allowed(url)

url_html <- read_html(url)

shows <- url_html %>%
  html_elements("h4 em") %>%
  html_text()

shows_labels <- url_html %>%
  html_elements("p strong") %>%
  html_text()

shows_info <- url_html %>%
  html_elements("div p") %>%
  html_text()

shows_data <- tibble(show = shows)
shows_label_data <- tibble(show_labels = shows_labels)
shows_info_data <- tibble(show_info = shows_info)

# reading in csv

netflix_shows <- read_csv("netflix_titles.csv")
netflix_subs <- read_csv("revenue_subscriber_data.csv")

num <- c(netflix_subs$'# of Subscribers Q1 2021')
num <- gsub('K', 'e3', num)
num <- gsub('M', 'e6', num)
netflix_subs$'# of Subscribers Q1 2021' <- format(as.numeric(num), scientific = FALSE)

netflix_subs$'# of Subscribers Q1 2021' <- as.numeric(netflix_subs$'# of Subscribers Q1 2021')

