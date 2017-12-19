library(shiny)
library(shinyjs)
library(dplyr)

g <<- new.env()

b <<- new.env()

w <<- new.env()

g$r_table_battle = tibble("Date"= as.character(Sys.Date()), "Time" = as.character(format(Sys.time(), "%X")), "Type" = "None", "Result" = "None")

b$r_table_computer = tibble("Date"= as.character(Sys.Date()), "Time" = as.character(format(Sys.time(), "%X")), "Type" = "None", "Result" = "None")

g$battle_start = 0

b$computer_start = 0

ui <- navbarPage("GOMOKU",
                 tabPanel(title = "INSTRUCTION",
                          mainPanel(h2("Rules", align = "center"),
                                    br(),
                                    h4(textOutput("grule1")),
                                    br(),
                                    h4(textOutput("grule2")),
                                    br(),
                                    h4(textOutput("grule2.5")),
                                    br(),
                                    h4(textOutput("grule3")),
                                    br(),
                                    plotOutput("gwin1"),
                                    br(),
                                    plotOutput("gwin2"),
                                    br(),
                                    plotOutput("gwin3"),
                                    hr(),
                                    h2("Level", align = "center"),
                                    br(),
                                    h4(textOutput("glevel1")),
                                    br(),
                                    h4(textOutput("glevel2")),
                                    br(),
                                    h4(textOutput("glevel3")),
                                    br(),
                                    plotOutput("g1"),
                                    br(),
                                    h4(textOutput("glevel4")),
                                    br(),
                                    plotOutput("g2")
                         
                          )),
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
               
               mainPanel(plotOutput("battle", click = "battle_click"),
                            textOutput("battle_result"))
             ) ),
    tabPanel(title = "COMPUTER",
             sidebarLayout(
               sidebarPanel(useShinyjs(),
                            div(id = "computer_page",
                 sliderInput(inputId = "computer_num", 
                             label = "Choose a number", 
                             value = 19, min = 5, max = 35, step = 2),
                 radioButtons("color", "Choose Youe Color", choices = c("BLACK", "WHITE"), selected = NULL),
                 radioButtons("level","Choose Level", choices = c("EASY", "HARD"), selected = NULL),
                 actionButton("click_computer","Play")
               ), 
               tags$hr(),
               actionButton("reset_computer", "Reset")
               
               ),
               mainPanel( plotOutput("computer", click = "computer_click"),
                             textOutput("computer_result"))
             )
            
             
             
    ),
    tabPanel(title = "RECORD",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("game", "Game", choices = c("BATTLE", "COMPUTER"), selected = NULL),
                 actionButton("refresh", "Refresh")
               ),mainPanel(tableOutput("record"))
             )
    )
  )


server <- function(input, output) {
  source("App_Yimo_Zhang/R/gomoku_packages.R")
  source("App_Yimo_Zhang/R/gomoku_backstage_functions.R")
  source("App_Yimo_Zhang/R/app_battle.R")
  source("App_Yimo_Zhang/R/app_black.R")
  source("App_Yimo_Zhang/R/gomoku_main_function.R")
  source("App_Yimo_Zhang/R/gomoku_plot.R")
  source("App_Yimo_Zhang/R/gomoku_harder.R")
  source("App_Yimo_Zhang/R/app_white.R")
  
  #instruction for gomoku
  output$grule1 = renderText("1. Choose a color (black or white) whether you play with 
                             computer or play with another person (battle). Black color
                             goes first.")
  output$grule2 = renderText("2. Players should place their own stones on the chessboard alternatively,
                             with one stone at a time. The stones that have been placed on the board cannot 
                             be moved, removed or replaced by other stones.")
  output$grule2.5 = renderText('3. Note that you have to press "Reset" button before you start another game by clickling "Play".')
  output$grule3 = renderText("4. Whoever has five stones in row wins the game. The stones can 
                             lie horizontally, vertically or diagonally, as shown below:")
  output$gwin1 = renderPlot({
    n = 100
    img = readPNG("App_Yimo_Zhang/R/horizontal.png")
    par(mar = rep(0, 4)) 
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(img,0,0,1+n,1+n)
  })
  output$gwin2 = renderPlot({
    n = 100
    img = readPNG("App_Yimo_Zhang/R/vertical.png")
    par(mar = rep(0, 4)) 
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(img,0,0,1+n,1+n)
  })
  output$gwin3 = renderPlot({
    n = 100
    img = readPNG("App_Yimo_Zhang/R/diagonal.png")
    par(mar = rep(0, 4)) 
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(img,0,0,1+n,1+n)
  })
  output$glevel1 = renderText("1. If you choose to play with computer, you can choose either
                              the easy version or hard version.")
  output$glevel2 = renderText("2. The strategy of computer is basically prevent you from winning the game.
                              In other words, the computer was designed to block you so you won't have stones in row. Note that the 
                              computer block you on two sides of a row of stones.")
  output$glevel3 = renderText("3. The difference between easy and hard is how to block you. In easy version, 
                              the computer will start to block you if you have at least one stone. But the position 
                              where it blocks you is random. As shown below, computer (white) will block you (black) in either of the five availabel spots, 
                              because you have two stones in row horizontally, vertically and diagonally.")
  output$g1 = renderPlot({
    n = 100
    img = readPNG("App_Yimo_Zhang/R/easy.png")
    par(mar = rep(0, 4)) 
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(img,0,0,1+n,1+n)
  })
  
  output$glevel4 = renderText("4. The hard version won't block unless you have at least two stones in row. It will choose depend on a series of checking. 
                              For example, in the picture below, there are two stone in row horizontally, vertically and diagonally. But the horizontal 
                              row has already been blocked by one white stone, so the computer will block either the vertical row or diagonal row.")
  output$g2 = renderPlot({
    n = 100
    img = readPNG("App_Yimo_Zhang/R/hard.png")
    par(mar = rep(0, 4)) 
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(img,0,0,1+n,1+n)
  })
  gomoku_battle(input, output)
  
  gomoku_black(input, output)
  
  gomoku_white(input, output)
  
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

