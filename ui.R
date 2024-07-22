#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
install.packages("DT")
install.packages("shinydashboard")
library(shiny)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(ggplot2)
library(plotly)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Airbnb Data Analysis Dashboard"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      selectInput("text_var", "Text Variable:", 
                  choices = names(airbnb_data)),
      actionButton("text_analyze", "Analyze Text"),
      
      selectInput("sentiment_var", "Sentiment Variable:", 
                  choices = names(airbnb_data)),
      actionButton("sentiment_analyze", "Analyze Sentiment"),
      
      actionButton("amenities_analyze", "Analyze Amenities"),
      
      actionButton("price_analyze", "Correlate Price")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Text Analysis", plotlyOutput("text_plot")),
        tabPanel("Amenities", plotOutput("amenities_plot")),
        tabPanel("Sentiment Analysis", plotOutput("sentiment_plot")),
        tabPanel("Pricing Strategy", plotlyOutput("price_plot")),
        tabPanel("Data Table", dataTableOutput("data_table"))
        # ... add more tabs for additional analyses as needed ...
      )
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server)
