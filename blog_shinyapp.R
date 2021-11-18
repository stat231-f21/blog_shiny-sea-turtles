# shiny app 
library(tidyverse)
library(kableExtra)
library(robotstxt) 
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

# load data

movies <- read_csv("data/MoviesOnStreamingPlatforms_updated.csv")


ui <- navbarPage(
  title = "Who's Watching Netflix?", 
  theme = shinytheme("cyborg"),
  
  # Visualization 1: Time Series
  tabPanel(
    title = "Salmon Abundance Per Year",
    
    sidebarLayout(
      sidebarPanel(
        
        selectizeInput(inputId = "species",
                       label = "Choose one or more species:",
                       choices = ts_choices,
                       selected = ts_choices,
                       multiple = TRUE),
        
        
        checkboxGroupInput(inputId = "age",
                           label = "Select between maturity:",
                           choices = age_choices,
                           selected = age_choices,
                           inline = TRUE)
      ),
      
      mainPanel(plotOutput(outputId = "ts"))
    )
  ),
  
  
  # Visualization 2: Table
  
  tabPanel(
    title = "Percentage of Total Biomass",
    
    sidebarLayout(
      sidebarPanel(
        #radioButtons(inputId = "spcs",
        #             label = "Choose species:",
        #             choices = biomass_long$Species,
        #             selected = "Pink"),
        
        sliderInput("yr", "Years:",min = min(biomass_long$Year), max = max(biomass_long$Year), value = c(1970, 2000),sep = "",)
      ),
      
      
      mainPanel(DT::dataTableOutput(outputId = "table"))
    )
  )
)





############
# server   #
############

server <- function(input, output){
  
  # Visualization 1: Time Series
  data_for_time <- reactive({
    data <- filter(abundance, Age %in% input$age,
                   Species %in% input$species)
  })
  
  output$ts <- renderPlot({
    ggplot(data = data_for_time(), aes(x = Year, y = Amount, color = Species,
                                       linetype = Age)) +
      geom_line(alpha = 0.7, stat = "identity") +
      labs(title = "Salmon Abundance Per Year",
           x = "Year",
           y = "Total Number of Salmon (Millions)") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      theme(plot.title = element_text(size = 20))
    # +
    #   scale_x_datetime(date_breaks = "1 year")
  })
  
  
  # Visualization 3
  
  output$table <- DT::renderDataTable({
    dt <- biomass_long[biomass_long$Year >= input$yr[1] & biomass_long$Year <= input$yr[2],]
  })
  
  
}

####################
# call to shinyApp #
####################

shinyApp(ui = ui, server = server)

# embed code into blog
knitr::include_app("https://your-account.shinyapps.io/your-app/", height = "600px")
