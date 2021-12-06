library(tidyverse) 
library(rvest) 
library(purrr)
library(janitor)
library(tidytext)
library(wordcloud)
library(textdata)
library(sf)
library(leaflet)
library(kableExtra)
library(viridis)
library(plotly)
library(ggnetwork)
library(igraph)
library(shinythemes)

# Import data
platforms_all_genre_rating <- read_csv("platforms_all_genre_rating.csv")
mtables <- read_csv("data/mtables.csv")

# For TAB 1 Table widgets

## For sliderInput choices for Year
si_choices <- unique(mtables$Year)

# For TAB 2 Bar Graph widgets:

## For selectInput choices for Genre
genre_choices <-  unique(platforms_all_genre_rating$Genres)

ui <- navbarPage(
  title = "Who's Watching Netflix?", 
  theme = shinytheme("sandstone"),
  
  # Visualization 1: Table
  
  tabPanel(
    title = "Ratings, Ratings, Ratings",
    
    sidebarLayout(
      sidebarPanel(
#        checkboxInput(inputId = "streamingservice",
#                      label = "Select Streaming Service:",
#                      choices = cb_choices,
#                      selected = "Netflix"),
        
        sliderInput("yr", "Years:", min = min(si_choices), 
                    max = max(si_choices), 
                    value = c(1914, 2021),sep = "",),
        sliderInput("imdb", "IMDb Rating:",min = 0, max = 10, value = c(0, 10), 
                    sep = "",),
        sliderInput("rottentom", "Rotten Tomatoes Rating",min = 0, max = 100, 
                    value = c(0, 100),sep = "",),
      
      
      ),
      
      mainPanel(DT::dataTableOutput(outputId = "table"))

    )
  ),

  # Visualization 2: Bar Graph

  tabPanel(
    title = "Streaming Platform Competition",
  
    sidebarLayout(
      sidebarPanel(
      
        # Create dropdown to allow user to select genre
        selectInput(inputId = "genre_bar",
                  label = "Select Genre:",
                  choices = genre_choices,
                  selected = "Crime"),
        # Set width of this component
        width = 3
      ),
      
      mainPanel(
        plotlyOutput(outputId = "bar"),
      
        # Set width of this component
        width = 9
      )
    )
  )
)

############
# server   #
############

server <- function(input, output){
  

  # Visualization 1: Table

  output$table <- DT::renderDataTable({
#    addCheckboxButtons <- paste0('<input type="checkbox" name="row', cb_choices, '" Streaming Service="', cb_choices, '">',"")
#    cbind(Pick=addCheckboxButtons, mtables[, input$streamingservice, drop=FALSE])
    dt1 <- mtables[(mtables$Year >= input$yr[1] & mtables$Year <= input$yr[2]) & 
                     (mtables$IMDb >= input$imdb[1] & 
                      mtables$IMDb <= input$imdb[2]) & 
                     (mtables$RottenTomatoes >= input$rottentom[1] & 
                      mtables$RottenTomatoes <= input$rottentom[2]),] 
  })
  
  # Visualization 2: Bar Graph
  data_for_bar <- reactive({
    # Filtering dataset for year and species based off the user input
    data <- filter(platforms_all_genre_rating, Genres %in% input$genre_bar)
  })
  
  output$bar <- renderPlotly({
    # Create bar chart for Shows/Movies Average Rating by Platform
    # For different genres
    ggplotly(ggplot(data = data_for_bar(), 
                    # Set Platform as x var and Average Rating as y var
                    # Fill the bars with color by species
                    aes(x = Service, y = IMDb)) +
               geom_bar(alpha = 0.7, stat = "identity", fill = c("#FF6666")) +
               # Label axes
               labs(title = "Average IMDb Rating of Shows/Movies by Platform",
                    x = "Platform",
                    y = "IMDb Rating (Out of 10)"),
             # Only show amount when hovering
             tooltip = "IMDb") 
  })
  
}

####################
# call to shinyApp #
####################

shinyApp(ui = ui, server = server)
