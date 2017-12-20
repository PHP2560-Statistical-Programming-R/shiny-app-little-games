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

ui <- navbarPage("LITTLE GAMES",
                 tabPanel("Instruction",
                      navbarPage("Instruction",
                     tabPanel(title = "Gomoku",
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
                     
                     #R snake
                     tabPanel(title = "R Snake"),
                     
                     #mine sweeper
                     tabPanel(title = "Mine Sweeper",
                              mainPanel(h2("Rules", align = "center"),
                                        br(),
                                        h4(textOutput("mrule1")),
                                        br(),
                                        h4(textOutput("mrule2")),
                                        br(),
                                        h4(textOutput("mrule3")),
                                        br(),
                                        h4(textOutput("mrule4")))),
                     
                     #R flags
                     tabPanel(title = "R Flags")
                                 
                                 )    
                          ),
                 tabPanel("Gomoku",
    navbarPage("Play Gomoku",
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
               mainPanel( plotOutput("computer", click = "computer_click"))
                             
             )),
    tabPanel(title = "RECORD",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("game", "Game", choices = c("BATTLE", "COMPUTER"), selected = NULL),
                 actionButton("refresh", "Refresh")
               ),mainPanel(tableOutput("record"))
             )
             )
  )),
  
  tabPanel(title = "R Snake",     #R snake
           sidebarLayout(
             sidebarPanel( 
               sliderInput("Speed", "Funny Snake", min = 1, max = 5, value = 1, step = 1),  #slider for speed input
               actionButton("Start", "Get Ready And Click Here to Start!")
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
  
  tabPanel(title = "R Flags",   #R flag
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
    img = readPNG("data/horizontal.png")
    par(mar = rep(0, 4)) 
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(img,0,0,1+n,1+n)
  })
  output$gwin2 = renderPlot({
    n = 100
    img = readPNG("data/vertical.png")
    par(mar = rep(0, 4)) 
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(img,0,0,1+n,1+n)
  })
  output$gwin3 = renderPlot({
    n = 100
    img = readPNG("data/diagonal.png")
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
    img = readPNG("data/easy.png")
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
    img = readPNG("data/hard.png")
    par(mar = rep(0, 4)) 
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(img,0,0,1+n,1+n)
  })
  
  #instruction for mine sweeper
  output$mrule1 = renderText("1. You are presented with a board of squares. Some squares contain mines (bombs), others don't. If you click on a square containing a bomb, you lose. If you manage to click all the squares (without clicking on any bombs) you win.")
  output$mrule2 = renderText("2. Clicking a square which doesn't have a bomb reveals the number of neighbouring squares containing bombs. Use this information plus some guess work to avoid the bombs.")
  output$mrule3 = renderText("3. To open a square, point at the square and click on it. To mark a square you think is a bomb, point and right-click (or hover with the mouse and press Space).")
  output$mrule4 = renderText("4. You can change the difficulty by adjust the width, height of the playground and the mines in the playground on the left control panel.")
  
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
  
  
  
  output$Snake <- renderUI({
    #trigger of starting button
    Snk_actionstart <- eventReactive(input$Start, { 
      
      
      Snake<-function(s){
        
        install_packages <- function(names)
        {
          for(name in names)
          {
            if (!(name %in% installed.packages()))
              install.packages(name, repos="http://cran.us.r-project.org")
            
            library(name, character.only=TRUE)
          }
        }
        
        install_packages(c("ggplot2","Cairo","ggmap","grid","scales","png","jpeg")) 
        if (.Platform$OS.type == "windows") x11() 
        else x11(type = "Xlib")
        init<-function( ){ ## Initialize the environment variable
          e<<-new.env()
          e$stage<-0 ## Scene
          e$width<-e$height<-20  ## Grid segmentation
          e$step<-1/e$width ## Distance moved per unit time
          e$m<-matrix(rep(0,e$width*e$height),nrow=e$width)  ## Dot-matrix
          e$dir<-e$lastd<-'up' ## Direction of movement
          e$head<-c(2,2) ## Start position
          e$lastx<-e$lasty<-2 ## Initialize the head of the snake
          e$tail<-data.frame(x=c(),y=c()) ## Initialize the tail of the snake
          
          e$col_furit<- runif(1,min=1,max=length(colors())) ## Fruit color
          e$col_head<- runif(1,min=1,max=length(colors()))  ## Snake head color
          e$col_tail<- runif(1,min=1,max=length(colors()))  ## Snake tail color
          e$col_path<- 0 ## path color
          
        }
        
        
        index<-function(col) which(e$m==col)  ## Get the index value of the matrix
        
        
        
        
        stage1<-function( ){  ## Being in the game
          e$stage<-1
          
          
          furit<-function( ){ ## Randomized fruit position
            if(length(index(e$col_furit))<=0){ ## Nonexistent position
              idx<-sample(index(e$col_path),1)
              
              fx<-ifelse(idx%%e$width==0,10,idx%%e$width)
              fy<-ceiling(idx/e$height)
              e$m[fx,fy]<-e$col_furit
              
              print(paste("furit idx",idx))
              print(paste("furit axis:",fx,fy))
            }
          }
          
          
          fail<-function( ){    ## Check the failure
            ## Head out of ledge
            if(length(which(e$head<1))>0 | length(which(e$head>e$width))>0){
              print("game over: Out of ledge.")
              keydown('q')
              return(TRUE)
            }
            
            ## Head eats tail
            if(e$m[e$head[1],e$head[2]]==e$col_tail){
              print("game over: head eats tail")
              keydown('q')
              return(TRUE)
            }
            
            return(FALSE)
          }
          
          
          head<-function( ){    ## Snake head
            e$lastx<-e$head[1]
            e$lasty<-e$head[2]
            
            ## Direction operator
            if(e$dir=='up') e$head[2]<-e$head[2]+1
            if(e$dir=='down') e$head[2]<-e$head[2]-s
            if(e$dir=='left') e$head[1]<-e$head[1]-s
            if(e$dir=='right') e$head[1]<-e$head[1]+1
            
          }
          
          
          body<-function( ){    ## Snake body
            e$m[e$lastx,e$lasty]<-0
            e$m[e$head[1],e$head[2]]<-e$col_head ## Snake
            if(length(index(e$col_furit))<=0){ ## Nonexistent position
              e$tail<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty))
            }
            
            if(nrow(e$tail)>0) { ## If snake has tail
              e$tail<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty))
              e$m[e$tail[1,]$x,e$tail[1,]$y]<-e$col_path
              e$tail<-e$tail[-1,]
              e$m[e$lastx,e$lasty]<-e$col_tail
            }
            
            print(paste("snake idx",index(e$col_head)))
            print(paste("snake axis:",e$head[1],e$head[2]))
          }
          
          drawTable<-function( ){ ## The canvas background
            plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
            rect(-3, -3, 3, 3, col="cornsilk") ## Background color
            
          }
          
          
          drawMatrix<-function( ){     ## Fill in the data according to the matrix
            idx<-which(e$m>0)
            px<- (ifelse(idx%%e$width==0,e$width,idx%%e$width)-1)/e$width+e$step/2
            py<- (ceiling(idx/e$height)-1)/e$height+e$step/2
            pxy<-data.frame(x=px,y=py,col=e$m[idx])
            points(pxy$x,pxy$y,col=pxy$col,pch=15,cex=4.4)
          }
          
          furit()
          head()
          if(!fail()){
            body()
            drawTable()
            drawMatrix()  
          }
        }
        
        
        stage0<-function( ){ ## Startup screen
          e$stage<-0
          
          plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
          bg = readJPEG("data/BGB1.jpg")
          rasterImage(bg,0,0,1,1)
          text(0.5,0.7,label="Snake Game",cex=5,col="yellow")
          text(0.5,0.4,label="Any keyboard to start",cex=2,col="pink")
          text(0.5,0.3,label="Up,Down,Left,Right to control direction",cex=2,col="pink")
        }
        
        
        stage2<-function( ){  ## Get result
          e$stage<-2
          plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
          bgg = readJPEG("data/grass1.jpg")
          rasterImage(bgg,0,0,1,1)
          text(0.5,0.7,label="Congratulations!",cex=4,col="pink")
          text(0.5,0.6,label=paste("You got",nrow(e$tail),"fruits!"),cex=3,col="pink")
          text(0.5,0.4,label="'Space'' to restart, 'Q' to quit.",cex=2,col="yellow")
        }
        
        
        
        keydown<-function(K){ ## Keyboards events
          print(paste("keydown:",K,",stage:",e$stage));
          
          if(e$stage==0){ ## Startup screen
            init()
            stage1()
            return(NULL)
          }  
          
          if(e$stage==2){ ## Get result
            if(K=="q") q()
            else if(K==' ') stage0()  
            return(NULL)
          } 
          
          if(e$stage==1){ ## Being in the game
            if(K == "q") {
              stage2()
            } else {
              if(tolower(K) %in% c("up","down","left","right")){
                e$lastd<-e$dir
                e$dir<-tolower(K)
                stage1()  
              }
            }
          }
          return(NULL)
        }
        
        
        
        par(mai=rep(0,4),oma=rep(0,4)) ## Set the global canvas without edge
        e<<-new.env() ## Encapsulate variables
        stage0() ## Startup screen
        
        getGraphicsEvent(prompt="Snake Game",onKeybd=keydown) ## Keyboards events registration
      }
      
      Snake(input$Speed)
      
      
    })
    Snk_actionstart()
  })
  
  
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
