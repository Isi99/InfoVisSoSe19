#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("DT")

library(shiny)
library(tidyverse)
library(DT)

tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")
write_rds(tidy_anime[1,], "temp.rds")

tidy_anime <- read_rds("temp.rds")

tidy_anime$source %>% as.factor()

anime <- tidy_anime %>% select(animeID, name, title_english, genre, episodes, rating, score, scored_by, rank, popularity, type, source) %>% 
  mutate(rating = factor(source))

anime$source

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("AnimeData Tidy Tuesday"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(3,
            wellPanel(
              checkboxInput("km", "4-koma manga", FALSE),
              checkboxInput("book", "Book", FALSE),
              checkboxInput("cgame", "Card Game", FALSE),
              checkboxInput("digmanga", "Digital manga", FALSE),
              checkboxInput("game", "Game", FALSE),
              checkboxInput("lignovel", "Light Novel", FALSE),
              checkboxInput("manga", "Manga", FALSE),
              checkboxInput("music", "Music", FALSE),
              checkboxInput("novel", "Novel", FALSE),
              checkboxInput("orig", "Original", FALSE),
              checkboxInput("other", "Other", FALSE),
              checkboxInput("pbook", "Picture book", FALSE),
              checkboxInput("radio", "Radio", FALSE),
              checkboxInput("unknown", "Unknwon", FALSE),
              checkboxInput("webmanga", "Web manga", FALSE),
              checkboxInput("visnovel", "Visual novel", FALSE)
            )
     ),
     column(9, 
            # Show a plot of the generated distribution
            plotOutput("sourceplot", brush = "plot_brush"),
            DT::DTOutput("tabledata")
     )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$tabledata <- renderDT(anime)
  
  output$sourceplot <- renderPlot ({
    
    temp <- anime %>% filter(source == "ASÖDLJADÖS")
    if(input$km){
      temp <- anime %>% filter(source == "4-koma manga")
    }
    if(input$book){
      temp <- anime %>% filter(source == "Book") %>% bind_rows(temp)
    }
    if(input$cgame){
      temp <- anime %>% filter(source == "Card Game") %>% bind_rows(temp)
    }
    if(input$digmanga){
      temp <- anime %>% filter(source == "Digital manga") %>% bind_rows(temp)
    }
    if(input$game){
      temp <- anime %>% filter(source == "Game") %>% bind_rows(temp)
    }
    if(input$lignovel){
      temp <- anime %>% filter(source == "Light Novel") %>% bind_rows(temp)
    }
    if(input$manga){
      temp <- anime %>% filter(source == "Manga") %>% bind_rows(temp)
    }
    if(input$music){
      temp <- anime %>% filter(source == "Musik") %>% bind_rows(temp)
    }
    if(input$novel){
      temp <- anime %>% filter(source == "Novel") %>% bind_rows(temp)
    }
    if(input$orig){
      temp <- anime %>% filter(source == "Original") %>% bind_rows(temp)
    }
    if(input$other){
      temp <- anime %>% filter(source == "Other") %>% bind_rows(temp)
    }
    if(input$pbook){
      temp <- anime %>% filter(source == "Picture book") %>% bind_rows(temp)
    }
    if(input$radio){
      temp <- anime %>% filter(source == "Radio") %>% bind_rows(temp)
    }
    if(input$unknown){
      temp <- anime %>% filter(source == "Unknown") %>% bind_rows(temp)
    }
    if(input$webmanga){
      temp <- anime %>% filter(source == "Web manga") %>% bind_rows(temp)
    }
    if(input$visnovel){
      temp <- anime %>% filter(source == "Visual novel") %>% bind_rows(temp)
    }
    
    temp %>% ggplot() + 
      aes (x = source, y = score) +
      geom_boxplot() +
      coord_flip() +
      theme_grey() + 
      labs(title = "Punktzahl für verschiedene Anime-Quellen auf der MyAnimeList (MLA)",
           subtitle = "Boxplot der Auswertung auf der MyAnimeList (MLA) sortiert nach der Quelle des Anime",
           x = "Quelle des Anime",
           y = "Auswertung",
           caption = "Punkte stellen Ausreißer dar/ Quelle: Tam Nguyen/MyAnimeList.net")
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

