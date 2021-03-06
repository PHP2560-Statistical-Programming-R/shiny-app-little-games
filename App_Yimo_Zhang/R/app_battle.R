#play with another person
gomoku_battle = function(input, output) {
 
  #Click play button
  observeEvent(input$click_battle, {
    
    
    g$points = matrix(rep(0, input$battle_num^2), nrow = input$battle_num, ncol = input$battle_num)
    
    output$battle = renderPlot({chessboard(input$battle_num, g$points)})
    
    #define/reset variables
    g$i = 1
    g$j = 1
    g$black = list() 
    g$white = list() 
    
    #click on chessboard
    observeEvent(input$battle_click, {
      
      if(g$battle_start == 0)
      {
        #get the location of the click
      point = adjust(input$battle_click, input$battle_num)
      
      
      if (!if_in(point = point, points = g$points)) #break when the point had chessman on it
      {
        #Record this click
        g$points[point[1], point[2]] = g$j
  
        
        
        #plot the click
        output$battle = renderPlot({chessboard(input$battle_num, g$points)})
        
        #black part
        if(g$j == 1){
          #add the spot to black record
          g$black[[g$i]] = point
          #judge if black wins
          if(if_win(g$black)==1){
            #stop the game
            g$battle_start = 1 
            #plot black wins
            output$battle = renderPlot({plot_battle_result("Black Wins!")})
            #update record table
            g$r_table_battle = g$r_table_battle %>%
              rbind(c(as.character(Sys.Date()), as.character(format(Sys.time(), "%X")), "Battle", "Black Wins!"))
          }
        }
        
        #white part
        if(g$j == 2){
          #add the spot to white record
          g$white[[g$i]] = point
          if(if_win(g$white)==1){
            #stop the game
            g$battle_start = 1
            #plot white wins
            output$battle = renderPlot({plot_battle_result("White Wins!")})
            #update record table
            g$r_table_battle = g$r_table_battle %>%
              rbind(c(as.character(Sys.Date()), as.character(format(Sys.time(), "%X")), "Battle", "White Wins!"))
          }
        }
        #next player
        if(g$j == 1){g$j = 2}
        else{
          g$j = 1
          #next round
          g$i = g$i + 1}
     
        
      }
      }
    })
  })
  }

#plot chessboard
chessboard = function(n , points){
  img<-readJPEG("App_Yimo_Zhang/R/wood.jpg")
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

#plot the cover after clicking reset button
plot_cover = function(){
  n = 100
  x=c(1:n)
  y=c(1:n)
  taiji = readPNG("App_Yimo_Zhang/R/taiji.png")
  windowsFonts(JP1 = windowsFont("Pristina"))
  bg = readJPEG("App_Yimo_Zhang/R/wood.jpg")
  colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
  par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  rasterImage(bg,0,0,1+n,1+n)
  text(x = seq(32,68, length.out = 6), y=rep(8*n/9,6), col = colfunc(6),
       cex = 3.5, label = unlist(strsplit("GOMOKU", NULL)), family = "JP1", lwd = 2.5)
  
  rasterImage(taiji,30, 15, 70, 75)
}


#Plot white wins/black wins
plot_battle_result = function(result)
{
  if(result == "White Wins!"){
    img = readPNG("App_Yimo_Zhang/R/white_wins.png")
    color = "white"
    }
  if(result == "Black Wins!"){
    img = readPNG("App_Yimo_Zhang/R/black_wins.png")
    color = "black"
    }
  windowsFonts(JP1 = windowsFont("Pristina"))
  bg = readJPEG("App_Yimo_Zhang/R/wood.jpg")
  n = 100
  x=c(1:n)
  y=c(1:n)
  par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))#add points to the plot where the lines should be located
  rasterImage(bg,0,0,1+n,1+n)
  text(x = 50, y = 8*n/9, label = toupper(result), cex = 3.5, col = color, family = "JP1")
  rasterImage(img, 30, 15, 70, 75)
}
