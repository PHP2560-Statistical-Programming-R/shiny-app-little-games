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
                                    plotOutput("g2"),
                                    hr(),
                                    h2("Guide", align = "center"),
                                    br(),
                                    h4(textOutput("gguide1")),
                                    br(),
                                    plotOutput("gguidep1"),
                                    br(),
                                    h4(textOutput("gguide2")),
                                    br(),
                                    plotOutput("gguidep2"),
                                    br(),
                                    h4(textOutput("gguide3")),
                                    br(),
                                    plotOutput("gguidep3")
                         
                          )),
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
  
  #instruction for gomoku
  output$grule1 = renderText("1. Choose a color when you play with computer; decide your color choice in advance if you play battle with another person.
                              Black color goes first.")
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
  
  output$gguide1 = renderText('1. Click the tab to choose a mode of this game. "BATTLE" means 
                              playing with another person; "Computer" means playing with computer; 
                              "RECORD" means the record of your playing this game. In "BATTLE" page, 
                              you can select a size of the chessboard, note you have to decide which color to 
                              use by yourselves. Note that you have to click "Reset" and then "Play" to start another game.')
  output$gguidep1 = renderPlot({
    n = 100
    img = readPNG("App_Yimo_Zhang/R/choose_mode.png")
    par(mar = rep(0, 4)) 
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(img,0,0,1+n,1+n)
  })
  output$gguide2 = renderText('2. In "COMPUTER" page, you can choose size, color as well as the level of the game. Note that 
                              you have to click "Reset" and then "play" to start another game.')
  output$gguidep2 = renderPlot({
    n = 100
    img = readPNG("App_Yimo_Zhang/R/computer.png")
    par(mar = rep(0, 4)) 
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(img,0,0,1+n,1+n)
  })
  output$gguide3 = renderText('3. In "REOCRD" page, you can click select the type of game and then click "Refresh" to see the latest records of the game.')
  output$gguidep3 = renderPlot({
    n = 100
    img = readPNG("App_Yimo_Zhang/R/record.png")
    par(mar = rep(0, 4)) 
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(img,0,0,1+n,1+n)
  })
                               
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

