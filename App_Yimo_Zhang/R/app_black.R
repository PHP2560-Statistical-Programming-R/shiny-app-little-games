#play with computer(player is black)
gomoku_black = function(input, output) {
  
    #click play 
  observeEvent(input$click_computer,{

    
  if(input$color == "BLACK" ){
    
    #create/update variable
    b$points = matrix(rep(0, input$computer_num^2), nrow = input$computer_num, ncol = input$computer_num)

    output$computer = renderPlot({chessboard(input$computer_num, b$points)})

    
    
    #initialize variables
    b$i = 1
    b$player = list() 
    b$computer = list()
    b$playedlist = list()
    
    #play the game
    observeEvent(input$computer_click, {
      if(b$computer_start == 0){
      if(input$color == "BLACK"){
      
      
      point = adjust(input$computer_click, input$computer_num)

      #player plays
      if (!if_in(point = point, points = b$points)) #break when the point had chessman on it
      {
        b$points[point[1], point[2]] = 1
     
        
        xy = paste(point, collapse = ":")
        b$playedlist = c(b$playedlist, xy)
        
        #plot the click
        output$computer = renderPlot({chessboard(input$computer_num, b$points)})
         #add the spot to player record
         b$player[[b$i]] = point
        
          if(if_win(b$player)==1){
            #stop the game
            b$computer_start = 1
            output$computer = renderPlot({plot_computer_result("You Win!", "black")})
            #update record table
            b$r_table_computer = b$r_table_computer %>%
              rbind(c(as.character(Sys.Date()), as.character(format(Sys.time(), "%X")), "Computer", "Player Wins!"))
          }
        
         
        #Computer plays
        if(input$level == "HARD"){new = computer_play_hard(b$player, b$computer, b$playedlist, input$computer_num)}
        if(input$level == "EASY"){new = computer_play(b$player, b$computer, b$playedlist, input$computer_num)}
        point = unlist(new)
        b$points[point[1], point[2]] = 2
        xy = paste(point, collapse = ":")
        b$playedlist = c(b$playedlist, xy)
        #add the spot to computer record
        b$computer[[b$i]] = point
    
          if(if_win(b$computer)==1){
            #stop the game
            b$computer_start = 1
            output$computer = renderPlot({plot_computer_result("You Lose!","black")})
            #update record table
            b$r_table_computer = b$r_table_computer %>%
              rbind(c(as.character(Sys.Date()), as.character(format(Sys.time(), "%X")), "Computer", "Computer Wins!"))
          }
        
      #next round
       b$i = b$i+1
        
        
      }
      }
      }
    })
  }
  })
  }


#adjust the click position
adjust = function(adjust_point, n){
  l = adjust_point
  x = min(n, max(1, round(l$x)))
  y = min(n, max(1, round(l$y)))
  return(c(x,y))
}

#check if a spot is already filled
if_in = function(point, points){
  x = point[1]
  y = point[2]
  if(points[x,y] < 1)
    return(0)
  else{return(1)}
}
