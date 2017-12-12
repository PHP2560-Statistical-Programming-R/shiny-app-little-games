library(shiny)
setwd("gomoku")
ui <- navbarPage("GOMOKU",
    tabPanel(title = "BATTLE",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "battle_num", 
                             label = "choose a number", 
                             value = 19, min = 5, max = 99, step = 2),
                 actionButton("click_battle", "Play")
               ), mainPanel(plotOutput("battle"))
             ) ),
    tabPanel(title = "COMPUTER",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "computer_num", 
                             label = "Choose a number", 
                             value = 19, min = 5, max = 99, step = 2),
                 radioButtons("color", "Choose Youe Color", choices = c("BLACK", "WHITE"), selected = "BLACK"),
                 radioButtons("level","Choose Level", choices = c("EASY", "HARD"), selected = "EASY"),
                 actionButton("click_computer","Play")
               ), mainPanel( plotOutput("computer"))
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
  source("gomoku_battle.R")
  source("gomoku_easy.R")
  source("gomoku_main_function.R")
  source("gomoku_plot.R")
  source("gomoku_harder.R")
  source("gomoku_hard.R")
  
  observeEvent(input$click_battle, {
    output$battle = renderPlot({
      gomoku_battle(n = input$battle_num)
    })
  })
  
  observeEvent(input$click_computer, { output$computer = renderPlot({
    if(input$color == "BLACK"){
      if(input$level == "EASY"){gomoku_easy(n = input$computer_num, choose = 1)}
      if(input$level == "HARD"){gomoku_hard(n = input$computer_num, choose = 1)}
    }
    if(input$color == "WHITE"){
      if(input$level == "EASY"){gomoku_easy(n = input$computer_num, choose = 2)}
      if(input$level == "HARD"){gomoku_hard(n = input$computer_num, choose = 2)}
    }
  })
  })
  
 
  
}

shinyApp(ui, server)