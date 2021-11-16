# Visualization 1 (Kauila), Visualization 2 (Deven), Visualization 3 (Mahathi)

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
abundance <- read_csv("main_data/total_abundance.csv")
nat_origin <- read_csv("main_data/nat_origin_master_long.csv")
biomass_table <- read_csv("main_data/biomass_long.csv")

########################################################
#  Choice values and labels for widgets (user inputs)  #                       
########################################################

# For TAB 1 Time Series widgets:

## For selectizeInput choices for Salmon species
ts_choices <- unique(abundance$Species)

## For checkboxGroupInput choices for Salmon age
age_choices <-  unique(abundance$Age)

# For TAB 2 Bar Graph widgets:

## For sliderInput choices for year
bar_choice_values <- unique(nat_origin$Year)

## For checkboxGroupInput choices for Salmon species
species_choices <-  unique(nat_origin$Species)

############
#    ui    #
############

ui <- navbarPage(theme = shinytheme("sandstone"),
                 
    title = "What's With Salmon?",
                 
    # Visualization 1: Time Series
    tabPanel(
      title = "Salmon Abundance Per Year",
      
      sidebarLayout(
        sidebarPanel(
          # Create input ID
          selectizeInput(inputId = "species",
                         # Prompt
                         label = "Choose one or more species:",
                         # Use choices for species defined above ui chunk
                         choices = ts_choices,
                         selected = ts_choices,
                         multiple = TRUE),
          
          # Create input ID for checkbox             
          checkboxGroupInput(inputId = "age",
                             label = "Select between maturity:",
                             choices = age_choices,
                             selected = age_choices,
                             inline = TRUE)
          ),
        # Output ID for server output   
        mainPanel(plotlyOutput(outputId = "ts"))
     )
   ),
                 
   # Visualization 2: Bar Graph
   tabPanel(
     title = "Salmon Abundance by National Origin",
     
     sidebarLayout(
       sidebarPanel(
         # Create slider to allow user to select year
         sliderInput(inputId = "year_bar",
                     label = "Choose Year:",
                     min = 1952, max = 2015,
                     value = 1952, step = 1,
                     sep ="", animate = TRUE),

         # Create checkboxes to allow user to select species
         checkboxGroupInput(inputId = "species_bar",
                            label = "Select Species:",
                            choices = species_choices,
                            selected = species_choices,
                            inline = TRUE),
         # Set width of this component
         width = 3
         ),
       mainPanel(
         plotlyOutput(outputId = "bar"),
         
         # Set width of this component
         width = 9
       )
     )
   ),
                 
  # Visualization 3: Table
  tabPanel(
    title = "Percentage of Total Biomass",
    sidebarLayout(
      sidebarPanel(
        # Double sided slider input that allows users to select a range of years      
        sliderInput("yr", "Years:",min = min(biomass_table$Year), 
                    max = max(biomass_table$Year), 
                    value = c(1970, 2000), sep = "",)
      ),
      
      mainPanel(DT::dataTableOutput(outputId = "table"))
    )
  )
)

###########
# server  #
###########

server <- function(input, output){
  
  # Visualization 1: Time Series
  data_for_time <- reactive({
    # Filtering dataset for age and species based off the user input
    data <- filter(abundance, Age %in% input$age,
                   Species %in% input$species)
  })
  
  output$ts <- renderPlotly({
    # Creating an interactive time series using plotly
    # Labeling axes approporiately and changing aesthetic to make clear diff.
    # Between the species and ages
    ggplotly(ggplot(data = data_for_time(), aes(x = Year, y = Amount, 
                                                color = Species, 
                                                linetype = Age)) +
      geom_line(alpha = 0.7, stat = "identity") +
      labs(title = "Salmon Abundance Per Year",
           x = "Year",
           y = "Total Number of Salmon (Millions)",
           color = "Species & Age",
           linetype = "") +
      # Add more breaks on the y axis for a better idea of population
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)))
  })
  
  # Visualization 2: Bar Graph
  data_for_bar <- reactive({
    # Filtering dataset for year and species based off the user input
    data <- filter(nat_origin, Year == input$year_bar, 
                   Species %in% input$species_bar)
  })
  
  output$bar <- renderPlotly({
    # Create bar chart for Salmon Abundance by Location
    ggplotly(ggplot(data = data_for_bar(), 
                    # Set Salmon Abundance as x var and location as y var
                    # Fill the bars with color by species
                    aes(x = Location, y = Amount, fill = Species)) +
               geom_bar(alpha = 0.7, stat = "identity") +
               # Flip the axes
               coord_flip() +
               # Label axes
               labs(title = "Salmon Abundance by National Origin",
                    x = "National Origins",
                    y = "Number of Salmon (In Millions)") +
               # Choose bar colors 
               scale_fill_manual(values = c("dark green", "pink", "red")),
             # Only show amount when hovering
             tooltip = "Amount") 
  })
  
  # Visualization 3: Table
  output$table <- DT::renderDataTable({
    # Create new dataframe that extracts Year and specifies that both endpoints 
    # for year will be specified by user
    dt <- biomass_table[biomass_table$Year >= input$yr[1] & 
                         biomass_table$Year <= input$yr[2],]
    # Remove a column from the dataset that lists the number of the observation
    dt <- dt[-1]
  })
  
}

####################
# call to shinyApp #
####################

shinyApp(ui = ui, server = server)