
  
  #
  # This is a Shiny web application. You can run the application by clicking
  # the 'Run App' button above.
  #
  # Find out more about building applications with Shiny here:
  #
  #    http://shiny.rstudio.com/
  #
  
  ## app.R ##
  library(shiny)
  library(shinydashboard)
  library(tidyverse)
  library(DT)
  library(scales)
  
  #Daten Laden: Nobel Winners 
  nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
  nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")
  
  # App Nobel
  ui <- dashboardPage(
    # Dashboard Header ----
    dashboardHeader(title = "Nobel Prize Laureates"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Laureates", tabName = "laureates", icon = icon("user-graduate")),
        menuItem("Publications", tabName = "publications", icon = icon("book"))
      )
    ),
    # Dashboard Body ____
    dashboardBody( 
      tabItems(
        tabItem(
          tabName = "laureates",
          # Menü Seite 1 Nobel Laureates
          fluidRow(
            column(width = 3,
                   box(width = 12,
                       # Kategorienauswahl ----
                       checkboxGroupInput("prize_cat",
                                          label = "Please select Nobel Prize:", 
                                          choices = nobel_winners$category %>% unique(),
                                          selected = nobel_winners$category %>% unique()
                       )
                   ),
                   box(width = 12,
                       #Infoboxen ----
                       infoBoxOutput ("approvalBox", width = 12)
                       # Zweite Box, erste Spalte
                      )
                   ),
                   box(width = 12,
                   title = "Prize-Year",
                   sliderInput("select_year",label =  "Select the Prize Year:",
                            min = min(nobel_winners$prize_year),
                            max = max(nobel_winners$prize_year),
                            value = "2015")
                     )
                  ),
                  box(width = 12,
                       checkboxGroupInput("orga_sel", 
                                          label = "Select the Organization Counry:", 
                                          choices = nobel_winners$organization_country %>% unique(),
                                          selected = nobel_winners$organization_country %>% unique()
                       )              
                   ),
                  
          column(width = 9,
                 box(width = 12,
                     dataTableOutput("nobel_laureates")
              ),
                 box(width = 12, 
                    plotOutput("mainplot")
                      )
                    )
             )
          # Menü 1 
        ),
        tabItem(
          tabName = "publications"
          # Menü 2
      )
    )
  )
  # Serverfunktion ----
  server <- function(input, output) {
    
    nobel_selection <- nobel_winners %>% 
      select( full_name, organization_country ,organization_name, prize_year, category, prize, prize_share, laureate_type, gender) %>%
      arrange(desc(prize_year))
    
    
    # Datatable output ----
    output$nobel_laureates <- renderDataTable ({
      nobel_selection %>% 
        filter(category %in% input$prize_cat) %>%
        filter(prize_year == input$select_year)
      
      
    })
    
    output$mainplot <- renderPlot ({
      nobel_selection %>%
        filter(organization_country %in% input$orga_sel) %>%
        ggplot() +
        aes(x = organization_country, fill = gender) +
        geom_bar() +
        coord_flip()+
        labs(
          title = "Most Nobel Laureates come from the USA",
          subtitle = "Bar chart of the organization' countries by gender",
          x = "organization country",
          y = "Number of award winners")
        
  
    })
    
    # Approvalbox ouput ----  
    output$approvalBox <- renderInfoBox({
      
      gender_table <- nobel_selection %>% filter(category %in% input$prize_cat) %>% select(gender) %>% table()
      if(dim(gender_table) == 0){
        percent_val <- 0.5
      } else {
        percent_val <- gender_table[1] / gender_table[2]
      }
      
      male_icon <- icon("male")
      female_icon <- icon("female")
      
      
      if(percent_val > 0.1) {
        selected_icon <- male_icon
      } else {
        selected_icon <- female_icon
      }
      
      percent_output <- percent(percent_val, accuracy = .01)
      
      valueBox(
        "Gender", percent_output, icon = selected_icon,
        color = "blue"
      )
      
    })
    
  }
  shinyApp(ui, server)
  


