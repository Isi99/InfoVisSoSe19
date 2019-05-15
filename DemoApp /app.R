#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

bond_movies <- read_delim("BondData.csv", delim = ";")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("DemoApp_Bond_Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         sliderInput("minx",
                      "Minimum-Wert:",
                     min = 1,
                     max = 50,
                     value = 30 ),
      textInput("name", "Suche"),
      checkboxGroupInput("name", "Checkbox:",
                         choices = c("Film1", "Film2")),
      actionButton("name", "Film ab!"),
      selectInput("name", "Select:",
                  choices = c("Show movies", "Show actors", "Show year", "Show tomatoes"))
      ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   #output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     # x    <- faithful[, 2] 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
    #  hist(x, breaks = bins, col = 'darkgray', border = 'white')
   #})
}

# Run the application 
shinyApp(ui = ui, server = server)

