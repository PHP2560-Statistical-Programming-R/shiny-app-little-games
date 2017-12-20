#play with computer(player is white)
gomoku_white = function(input, output) {
  
  
    
    #click play button
    observeEvent(input$click_computer,{
      
    if(input$color == "WHITE"){

    #initilize variables
    w$points_white = matrix(rep(0, input$computer_num^2), nrow = input$computer_num, ncol = input$computer_num)
    
    output$computer = renderPlot({chessboard(input$computer_num, w$points_white)})

    w$k = 1
    w$player_white = list() 
    w$computer_white = list()
    w$playedlist_white = list()
    
    #computer goes first
    if(input$level == "HARD"){new = computer_play_hard(w$player_white, e$computer_white, w$playedlist_white, input$computer_num)}
    if(input$level == "EASY"){new = computer_play(w$player_white, w$computer_white, w$playedlist_white, input$computer_num)}
    point = unlist(new)
    
    w$points_white[point[1], point[2]] = 1
    xy= paste(point, collapse = ":")
    w$playedlist_white = c(w$playedlist_white, xy)
    w$computer_white[[w$k]]= point
    
    output$computer = renderPlot({chessboard(input$computer_num, w$points_white)})
    
    #start to play
    observeEvent(input$computer_click, {
      
      if(b$computer_start == 0){
      if(input$color == "WHITE"){
      
   
      
      point = adjust(input$computer_click, input$computer_num)

     
      #player goes second
      if (!if_in(point = point, points = w$points_white)) #break when the point had chessman on it
      {
        
        
        w$points_white[point[1], point[2]] = 2
        
        
        xy = paste(point, collapse = ":")
        w$playedlist_white = c(w$playedlist_white, xy)
        
        output$computer = renderPlot({chessboard(input$computer_num, w$points_white)})
        #add the spot to player record
        w$player_white[[w$k]] = point
        
        #next round
        w$k = w$k+1
    
        if(if_win(w$player_white)==1){
          #stop the game
          b$computer_start = 1
          output$computer = renderPlot({plot_computer_result("You Win!", "white")})
          #update record table
          b$r_table_computer = b$r_table_computer %>%
            rbind(c(as.character(Sys.Date()), as.character(format(Sys.time(), "%X")), "Computer", "Player Wins!"))
        }
        
        #computer playes after player
        if(input$level == "HARD"){new = computer_play_hard(w$player_white, w$computer_white, w$playedlist_white, input$computer_num)}
        if(input$level == "EASY"){new = computer_play(w$player_white, w$computer_white, w$playedlist_white, input$computer_num)}
        point = unlist(new)
        w$points_white[point[1], point[2]] = 1
        xy = paste(point, collapse = ":")
        w$playedlist_white = c(w$playedlist_white, xy)
        #add the spot to computer record
        w$computer_white[[w$k]] = point
   
        if(if_win(w$computer_white)==1){
          #stop the game
          b$computer_start = 1
          output$computer = renderPlot({plot_computer_result("You Lose!", "white")})
          #update record tabe
          b$r_table_computer = b$r_table_computer %>%
            rbind(c(as.character(Sys.Date()), as.character(format(Sys.time(), "%X")), "Computer", "Computer Wins!"))
        }
      }
        
      }
      }
    })
    }
  })

  }
#plot player wins/player wins
plot_computer_result = function(result, stone_color){
  if(result == "You Win!"){img = readPNG("App_Yimo_Zhang/R/happy_face.png")}
  if(result == "You Lose!"){img = readPNG("App_Yimo_Zhang/R/sad_face.png")}
  colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
  colfunc1 = colorRampPalette(c("black","gray90"))
  windowsFonts(JP1 = windowsFont("Pristina"))
  bg = readJPEG("App_Yimo_Zhang/R/wood.jpg")
  n = 100
  x=c(1:n)
  y=c(1:n)
  par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))#add points to the plot where the lines should be located
  rasterImage(bg,0,0,1+n,1+n)
  text(x = 50, y = 8*n/9, label = toupper(result), cex = 3.5, col = stone_color, family = "JP1")
  rasterImage(img, 30, 15, 70, 75)
}
