
library(shiny)
library(shinyjs)


ui <- navbarPage("LITTLE GAMES",
                 tabPanel("GOMOKU",
                 navlistPanel(
    tabPanel(title = "BATTLE",
             sidebarLayout(
               sidebarPanel(useShinyjs(),
                            div(id = "battle_page",
                 sliderInput(inputId = "battle_num", 
                             label = "choose a number", 
                             value = 19, min = 5, max = 99, step = 2),
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
                             value = 19, min = 5, max = 99, step = 2),
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
                 actionButton("refresh", "Refresh")
               ),mainPanel(tableOutput("record"))
             )
             )
  )),
  
  tabPanel(title = "R snake",     #R snake
           sidebarLayout(
             sidebarPanel(actionButton("Start", "Get Ready And Click Here to Start!")
             ),
                          mainPanel(plotOutput("Snake")
             )
             
           )),
  
  
  tabPanel(title = "Mine Sweeper", #mine sweeper
           sidebarLayout(
             sidebarPanel(
               sliderInput("WidthInput", "Width", 5, 50, 10),  #slider for width input
               sliderInput("LengthInput", "Length", 5, 50, 10), #slider for length input
               sliderInput("MinesInput", "Mines", 1, 100, 5), #slider for length input
               actionButton("start", "Start")
               #      selectInput("restartOption", "Restart", c("SELECT CHOICE","YES","NO"))
             ),
             mainPanel(uiOutput("mine")) #show the main game panel
           )),
  
  tabPanel(title = "R flag",   #R flag
           sidebarLayout(
             sidebarPanel(
               selectInput("Countries", "Please Select A Country",
                           choices = c("Norway", 
                                       "Denmark",
                                       "Finland",
                                       "Japan",
                                       "Iceland",
                                       "Sweden")),
               sliderInput("Points", "Please Select A Number",
                           min = 1000, max = 100000, value = 20000, step = 10000)
               
             ),
             mainPanel(
               plotOutput("flagPlot")
             ))

))


server <- function(input, output) {
  source("App_Yimo_Zhang/R/gomoku_packages.R")
  source("App_Yimo_Zhang/R/gomoku_backstage_functions.R")
  source("App_Yimo_Zhang/R/app_battle.R")
  source("App_Yimo_Zhang/R/app_black.R")
  source("App_Yimo_Zhang/R/gomoku_main_function.R")
  source("App_Yimo_Zhang/R/gomoku_plot.R")
  source("App_Yimo_Zhang/R/gomoku_harder.R")
  source("App_Yimo_Zhang/R/app_white.R")
  source("App_Bowei_Wei/R/mine_sweeper.R")
  
  data = reactive(input$color)
  
  gomoku_battle(points, input, output)
  
  gomoku_black(points, input, output)
  
  gomoku_white(points, input, output)
  
  observeEvent(input$reset_battle,{
    reset("battle_page")
    output$battle = renderPlot({plot_cover()})
  })
  
  observeEvent(input$reset_computer,{
    reset("computer_page")
    output$computer = renderPlot({plot_cover()})
  })
  
  output$Snake <- renderUI(  #start the game
    Snake_actionstart()
  )
  
  #trigger of starting button
  Snake_actionstart <- eventReactive(input$Start, { 
    source("data/TSnake.R")
  })
  
  output$Snake <- renderUI(
    Snake_actionstart()
  )
  
  
  actionstart <- eventReactive(input$start, { #trigger of starting button for mine sweeper
    mine_sweeper(input$WidthInput, input$LengthInput, input$MinesInput)
  }
  )
  output$mine <- renderUI(actionstart())#start the mine sweeper

  
  output$flagPlot <- renderPlot({
    Type <- input$Countries
    a <- input$Points
    install_packages <- function(names)
    {
      for(name in names)
      {
        if (!(name %in% installed.packages()))
          install.packages(name, repos="http://cran.us.r-project.org")
        
        library(name, character.only=TRUE)
      }
    }
    install_packages(c("ggplot2","dplyr","rpart","caret","e1071"))
    
    if (Type == "Japan"){
      # Let's create 50k points on a 3x2 grid
      x <- runif(a, min = 0, max = 3)
      y <- runif(a, min = 0, max = 2)
      # Flag colour palette
      japanPalette <- c("red", "white")
      # Flag dataframe
      japan_flag <- as.data.frame(x = x)
      japan_flag$y <- y
      # Now we add the colour
      japan_flag <-mutate(japan_flag, flag_colour = ifelse( (x - 1.5)^2 + (y-1)^2 > 3/10, "white", "red"))
      ggplot(japan_flag) +
        geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) +
        coord_fixed(ratio = 1) +
        scale_colour_manual(values = japanPalette)
    }else if (Type == "Norway"){
      # Let's create 200k points on a 21x16 grid
      x <- runif(a, min = 0, max = 21)
      y <- runif(a, min = 0, max = 16)
      
      flag <- as.data.frame(x = x)
      flag$y <- y
      
      # Now we add the colour, however this flags contain two crosses
      flag <-mutate(flag, flag_colour = ifelse(((x > 6) & (x<=10)) | ((y > 6) & (y<=10)), "cross", "bckgd"))
      crossed_flag <- flag[which(flag$flag_colour == "cross"),]
      flag[which(flag$flag_colour == "cross"),] <- 
        mutate(crossed_flag, flag_colour = ifelse(((x > 7) & (x<=9)) | ((y > 7) & (y<=9)), "inner_cross", "cross"))
      
      NorwayPalette <- c("red", "white", "blue")
      ggplot(flag) +
        geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) +
        coord_fixed(ratio = 1) +
        scale_colour_manual(values = NorwayPalette)
    }else if (Type == "Denmark"){
      # Let's create 200k points on a 16x10 grid
      x <- runif(a, min = 0, max = 16)
      y <- runif(a, min = 0, max = 10)
      
      # We create the dataframe
      flag <- as.data.frame(x = x)
      flag$y <- y
      
      # Now we add the colour
      flag <-mutate(flag, flag_colour = ifelse(((x > 5) & (x<=7)) | ((y > 4) & (y<=6)), "cross", "bckgd"))
      
      DenmarkPalette <- c("red", "white")
      
      ggplot(flag) +
        geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) +
        coord_fixed(ratio = 1) +
        scale_colour_manual(values = DenmarkPalette)
    }else if (Type == "Finland"){
      # Let's create 200k points on a 16x10 grid
      x <- runif(a, min = 0, max = 16)
      y <- runif(a, min = 0, max = 10)
      
      # We create the dataframe
      flag <- as.data.frame(x = x)
      flag$y <- y
      
      # Now we add the colour
      flag <-mutate(flag, flag_colour = ifelse(((x > 5) & (x<=7)) | ((y > 4) & (y<=6)), "cross", "bckgd"))
      
      FinlandPalette <- c("white", "blue")
      
      ggplot(flag) +
        geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) +
        coord_fixed(ratio = 1) +
        scale_colour_manual(values = FinlandPalette)
      
    }else if (Type == "Iceland"){
      # Let's create 200k points on a 21x16 grid
      x <- runif(a, min = 0, max = 21)
      y <- runif(a, min = 0, max = 16)
      
      flag <- as.data.frame(x = x)
      flag$y <- y
      
      # Now we add the colour, however this flags contain two crosses
      flag <-mutate(flag, flag_colour = ifelse(((x > 6) & (x<=10)) | ((y > 6) & (y<=10)), "cross", "bckgd"))
      crossed_flag <- flag[which(flag$flag_colour == "cross"),]
      flag[which(flag$flag_colour == "cross"),] <- 
        mutate(crossed_flag, flag_colour = ifelse(((x > 7) & (x<=9)) | ((y > 7) & (y<=9)), "inner_cross", "cross"))
      
      IcelandPalette <- c("blue", "white", "red")
      ggplot(flag) +
        geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) +
        coord_fixed(ratio = 1) +
        scale_colour_manual(values = IcelandPalette)
    }else if (Type == "Sweden"){
      # Let's create 200k points on a 16x10 grid
      x <- runif(a, min = 0, max = 16)
      y <- runif(a, min = 0, max = 10)
      
      # We create the dataframe
      flag <- as.data.frame(x = x)
      flag$y <- y
      
      # Now we add the colour
      flag <-mutate(flag, flag_colour = ifelse(((x > 5) & (x<=7)) | ((y > 4) & (y<=6)), "cross", "bckgd"))
      
      SwedenPalette <- c("blue", "yellow")
      
      ggplot(flag) +
        geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) +
        coord_fixed(ratio = 1) +
        scale_colour_manual(values = SwedenPalette)
    }
    
  })
             

}

shinyApp(ui, server)

