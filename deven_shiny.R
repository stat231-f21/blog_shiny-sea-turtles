# Read in packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(lubridate)
library(DT)
library(ggrepel)
library(shinyWidgets)
library(plotly)

# Import data
platforms_all_genre_Rotten <- read_csv("platforms_all_genre_Rotten.csv")

########################################################
#  Choice values and labels for widgets (user inputs)  #                       
########################################################

# For TAB 1 Bar Graph widgets:

## For selectInput choices for Genre
genre_choices <-  unique(platforms_all_genre_Rotten$Genres)

############
#    ui    #
############

ui <- navbarPage(theme = shinytheme("sandstone"),
                 
                 title = "Netflix",
                 
                 
                 # Visualization 1: Bar Graph
                 tabPanel(
                   title = "Streaming Platform Rating by Genre Comparison",
                   
                   sidebarLayout(
                     sidebarPanel(
                       
                       # Create dropdown to allow user to select genre
                       selectInput(inputId = "genre_bar",
                                          label = "Select Genre:",
                                          choices = genre_choices,
                                          selected = genre_choices),
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

###########
# server  #
###########

server <- function(input, output){
  
  # Visualization 2: Bar Graph
  data_for_bar <- reactive({
    # Filtering dataset for year and species based off the user input
    data <- filter(platforms_all_genre_Rotten, Genres %in% input$genre_bar)
  })
  
  output$bar <- renderPlotly({
    # Create bar chart for Shows/Movies Average Rating by Platform
    # For different genres
    ggplotly(ggplot(data = data_for_bar(), 
                    # Set Platform as x var and Average Rating as y var
                    # Fill the bars with color by species
                    aes(x = Service, y = RottenTomatoes)) +
               geom_bar(alpha = 0.7, stat = "identity", fill = c("#FF6666")) +
               # Label axes
               labs(title = "Average Rotten Tomatoes Rating of Content by Platform",
                    x = "Platform",
                    y = "Rotten Tomatoes Rating (Out of 100)"),
             # Only show amount when hovering
             tooltip = "RottenTomatoes") 
  })
  
}

####################
# call to shinyApp #
####################

shinyApp(ui = ui, server = server)
