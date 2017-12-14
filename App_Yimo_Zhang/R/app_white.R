#play with computer(player is white)
gomoku_white = function(points, input, output, fun) {
  
    observeEvent(input$click_computer,{
      
    if(input$color == "WHITE"){

    #initilize variables
    points_white <<- matrix(rep(0, input$computer_num^2), nrow = input$computer_num, ncol = input$computer_num)
    
    output$computer = renderPlot({chessboard(input$computer_num, points_white)})
    
    
    k = 1
    player_white = list() 
    computer_white = list()
    playedlist_white = list()
    
    #computer goes first
    if(input$level == "HARD"){new = computer_play_hard(player_white, computer_white, playedlist_white, input$computer_num)}
    if(input$level == "EASY"){new = computer_play(player_white,computer_white, playedlist_white, input$computer_num)}
    point = unlist(new)
    
    points_white[point[1], point[2]] = 1
    xy= paste(point, collapse = ":")
    playedlist_white = c(playedlist_white, xy)
    computer_white[[k]]= point
    
    output$computer = renderPlot({chessboard(input$computer_num, points_white)})
    
    #start to play
    observeEvent(input$computer_click, {
      if(input$color == "WHITE"){
      
  
      
      point = adjust(input$computer_click, input$computer_num)

     
      #player goes second
      if (!if_in(point = point, points = points_white)) #break when the point had chessman on it
      {
        
       
        points_white[point[1], point[2]] <<- 2
        
        
        xy = paste(point, collapse = ":")
        playedlist_white <<- c(playedlist_white, xy)
        
        output$computer = renderPlot({chessboard(input$computer_num, points_white)})
        
        player_white[[k]] <<- point
        
        k <<- k+1
    
        if(if_win(player_white)==1){
          output$computer_result = renderText("You Win!")
        }
        
        #computer playes after player
        if(input$level == "HARD"){new = computer_play_hard(player_white, computer_white, playedlist_white, input$computer_num)}
        if(input$level == "EASY"){new = computer_play(player_white,computer_white, playedlist_white, input$computer_num)}
        point = unlist(new)
        points_white[point[1], point[2]] <<- 1
        xy <- paste(point, collapse = ":")
        playedlist_white <<- c(playedlist_white, xy)
        computer_white[[k]]<<- point
   
        if(if_win(computer_white)==1){
          output$computer_result = renderText("You Lose!")
        }
      }
        
        
      }
    })
    }
  })

}

chessboard = function(n , points){
  img<-readJPEG("R/wood.jpg")
  par(mar = rep(0, 4)) 
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  rasterImage(img,0,0,1+n,1+n)
  segments(1, 1:n, n, 1:n)
  segments(1:n, 1, 1:n, n)
  temp = c(round((n+1)/5),(n+1)/2, round(4*(n+1)/5),round(4*(n+1)/5))
  points(rep(temp, 3), rep(temp, each = 3),
         pch = 19, cex = 6/sqrt(n))
  box()
  for(i in 1:n){
    for(k in 1:n){
      l = list()
      l$x = i
      l$y = k
      shape = points[i,k]
      if(shape > 0)
        points(l, cex = 3*19/n, pch = c(19, 21)[shape], bg = c("black", "white")[shape])
    }
  }
  
}


