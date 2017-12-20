# Gomoku app

## Source

I make this game based on the gomoku function made by Yihui in the [fun package](https://github.com/yihui/fun/tree/master/R). The original function was put in a R file called "original_gomoku.R".

## What the Original Function does

1. Draw a simple chessboard to play the game.

2. Write codes that place stones on the chessboard.

## What I Do

The original codes are quite short, so I basically added elements to the game.

1. Put constraints on the input (size of the chessboard must be an integer and odd number).

2. Modify the chessboard so that the chessboard will also look beautiful if the size is changed.

3. Set rules to judge if someone wins (and return something after someone wins).

4. Add play with computer part, where a player can choose level (easy or hard) and color (black or white).

5. Make a taiji logo use ggplot and add it to the plot.

6. Make differnt plot for different stages of the game. (Maybe the changes are more obvious if you play the R package version, because certain changes were made to accomodate this game to a shiny app environment)

7. Return tables that show the records of playing the game.

## About My Codes and Files

1. All codes were placed in the folder "App_Yimo_Zhang/R".

2. Files including "app_battle.R", "app_black.R" and "app_white.R" are functions written exclusively for the shiny app. Other R files were pretty much the same as that in R package.

3. "gomoku_battle.R", "gomoku_easy.R", "gomoku_hard.R" are functions written for battle, computer (easy) and computer (hard). They were not directed used in this app, but converted to files "app_battle.R", "app_black.R" and "app_white.R" to support this app.

4. "gomoku_backage_functions.R" and "gomoku_harder.R" are backstage functions that judge whether there is a winner. They are made of many tiny functions, each one has a specific purporse.

5. "gomoku_main_function.R" are the main function that runs the whole game. It was used in R package but not in shiny app. It serves as a reference to help me write codes in app.

6. "gomoku_scene_design.R" consists of codes that make logo like taiji.png.

7. "gomoku_plot.R" consists of functions that plot every stage of the game. Not all functions were used in shiny app.

8. "gomoku_packages.R" and "check_packages.R" check if the necessary package for this game is installed.

9. PNG and JPG files were used for plotting the game or instruction part.

## What I Can Improve

1. The strategy of computer can be very complicated and make this game more challenging. But due to the limit of time, I cannot make very complicated strategy.

2. The chessboard is supposed to be a square other than a rectangle when it appears in app. But also due to the limit of time, I didn't explore more.

## More

If you want to explore this game more, you can visit [gomoku](http://gomokuworld.com/articles/16_useful_tips_to_become_a_better_gomoku_player).
