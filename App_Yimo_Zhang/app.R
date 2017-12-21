library(shiny)
library(shinyjs)
library(dplyr)

#Add environment variables; in order to conveniently use define and arrange variables
g <<- new.env()

b <<- new.env()

w <<- new.env()

#The result table
g$r_table_battle = tibble("Date"= as.character(Sys.Date()), "Time" = as.character(format(Sys.time(), "%X")), "Type" = "None", "Result" = "None")

b$r_table_computer = tibble("Date"= as.character(Sys.Date()), "Time" = as.character(format(Sys.time(), "%X")), "Type" = "None", "Result" = "None")

#Click "Reset" button to play another game
g$battle_start = 0

b$computer_start = 0

#Web part
ui <- navbarPage("GOMOKU",
                 #Add instrunctions
                 tabPanel(title = "INSTRUCTION", 
                          includeHTML("App_Yimo_Zhang/R/gomoku_instruction.html")),
                          
                 #Battle
    tabPanel(title = "BATTLE",
             sidebarLayout(
               sidebarPanel(useShinyjs(),
                            div(id = "battle_page",
                 sliderInput(inputId = "battle_num", 
                             label = "choose a number", 
                             value = 19, min = 5, max = 35, step = 2),
                 actionButton("click_battle", "Play")
               ),
               tags$hr(),
               actionButton("reset_battle", "Reset")
               ),
               
               mainPanel(plotOutput("battle", click = "battle_click"))
                            
             ) ),
    #Play with computer
    tabPanel(title = "COMPUTER",
             sidebarLayout(
               sidebarPanel(useShinyjs(),
                            div(id = "computer_page",
                 sliderInput(inputId = "computer_num", 
                             label = "Choose a number", 
                             value = 19, min = 5, max = 35, step = 2),
                 radioButtons("color", "Choose Your Color", choices = c("BLACK", "WHITE"), selected = NULL),
                 radioButtons("level","Choose Level", choices = c("EASY", "HARD"), selected = NULL),
                 actionButton("click_computer","Play")
               ), 
               tags$hr(),
               actionButton("reset_computer", "Reset")
               
               ),
               mainPanel( plotOutput("computer", click = "computer_click"))
                             
             )
            
             
             
    ),
    #Record of playing the game
    tabPanel(title = "RECORD",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("game", "Game", choices = c("BATTLE", "COMPUTER"), selected = NULL),
                 actionButton("refresh", "Refresh")
               ),mainPanel(tableOutput("record"))
             )
    )
  )

#Server part
server <- function(input, output) {
  #Source all the necessary file
  source("App_Yimo_Zhang/R/gomoku_packages.R")
  source("App_Yimo_Zhang/R/gomoku_backstage_functions.R")
  source("App_Yimo_Zhang/R/app_battle.R")
  source("App_Yimo_Zhang/R/app_black.R")
  source("App_Yimo_Zhang/R/gomoku_main_function.R")
  source("App_Yimo_Zhang/R/gomoku_plot.R")
  source("App_Yimo_Zhang/R/gomoku_harder.R")
  source("App_Yimo_Zhang/R/app_white.R")
  
                               
  #Game start
  gomoku_battle(input, output)
  
  gomoku_black(input, output)
  
  gomoku_white(input, output)
  
  #Reset
  observeEvent(input$reset_battle,{
    reset("battle_page")
    g$battle_start = 0
    output$battle = renderPlot({plot_cover()})
  })
  
  observeEvent(input$reset_computer,{
    reset("computer_page")
    b$computer_start = 0
    output$computer = renderPlot({plot_cover()})
  })
  
  #Display the record table
  observeEvent(input$refresh, {
    if(input$game == "BATTLE"){
      output$record = renderTable({
        g$r_table_battle %>%
          filter(Type != "None")
      })}
    if(input$game == "COMPUTER"){
      output$record = renderTable({
        b$r_table_computer %>%
          filter(Type != "None")
      })
    }
  })
            


}

shinyApp(ui, server)

