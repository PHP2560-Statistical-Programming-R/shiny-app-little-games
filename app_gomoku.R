library(shiny)
library(shinyjs)
ui <- navbarPage("GOMOKU",
    tabPanel(title = "BATTLE",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "battle_num", 
                             label = "choose a number", 
                             value = 19, min = 5, max = 99, step = 2),
                 actionButton("click_battle", "Play")
               ), mainPanel(plotOutput("battle", click = "battle_click"),
                            textOutput("battle_result"))
             ) ),
    tabPanel(title = "COMPUTER",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "computer_num", 
                             label = "Choose a number", 
                             value = 19, min = 5, max = 99, step = 2),
                 radioButtons("color", "Choose Youe Color", choices = c("BLACK", "WHITE"), selected = NULL),
                 radioButtons("level","Choose Level", choices = c("EASY", "HARD"), selected = NULL),
                 actionButton("click_computer","Play")
               ), mainPanel( plotOutput("computer", click = "computer_click"),
                             textOutput("computer_result"))
             )
            
             
             
    ),
    tabPanel(title = "RECORD",
             sidebarLayout(
               sidebarPanel(
                 actionButton("refresh", "Refresh")
               ),mainPanel(tableOutput("record"))
             )
             )
  )


server <- function(input, output) {
  source("gomoku_packages.R")
  source("gomoku_backstage_functions.R")
  source("app_battle.R")
  source("app_black.R")
  source("gomoku_main_function.R")
  source("gomoku_plot.R")
  source("gomoku_harder.R")
  source("app_white.R")
  
  data = reactive(input$color)
  
  gomoku_battle(points, input, output)
  

  gomoku_black(points, input, output)
  
  gomoku_white(points, input, output)


}

shinyApp(ui, server)

