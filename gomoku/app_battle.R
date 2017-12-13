gomoku_battle = function(points, input, output) {
  
  observeEvent(input$click_battle, {
    
    
    points = matrix(rep(0, input$battle_num^2), nrow = input$battle_num, ncol = input$battle_num)
    
    output$battle = renderPlot({chessboard(input$battle_num, points)})
    
    i <<- 1
    j <<- 1
    black <<- list() 
    white <<- list() 
    
    
    observeEvent(input$battle_click, {
      
      
      point = adjust(input$battle_click, input$battle_num)
      
      
      if (!if_in(point = point, points = points)) #break when the point had chessman on it
      {
        points[point[1], point[2]] <<- j
        print(points)
        
        
        
        output$battle = renderPlot({chessboard(input$battle_num, points)})
        
        
        if(j == 1){
          black[[i]] <<- point
          print(black)
          if(if_win(black)==1){
            output$battle_result = renderText("Black Wins!")
          }
        }
        
        #check the white chessman
        if(j == 2){
          white[[i]] <<- point
          if(if_win(white)==1){
            output$battle_result = renderText("White Wins!")
          }
        }
      
        if(j == 1){j <<- 2}
        else{
          j <<- 1
          i <<- i + 1}
     
        
      }
    })
  })
}

chessboard = function(n , points){
  img<-readJPEG("wood.jpg")
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


adjust = function(adjust_point, n){
  l = adjust_point
  x = min(n, max(1, round(l$x)))
  y = min(n, max(1, round(l$y)))
  return(c(x,y))
}

if_in = function(point, points){
  x = point[1]
  y = point[2]
  if(points[x,y] < 1)
    return(0)
  else{return(1)}
}
