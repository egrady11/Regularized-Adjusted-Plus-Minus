# Regularized-Adjusted-Plus-Minus

Summary: In this project, I created a plus-minus-based metric to facilitate comparisons of NBA players designed to approximate each player's isolated impact on scoring. I began with play-by-play data from every game of the 2018-19 NBA regular season and manipulated it to create a design matrix with a column for each player who appeared in the league that season and a row for each unique stint (10-man combination of players on the floor). For each player column, if the player was in the stint and on the home team, he recived a 1; if he was in the stint and on the away team, he recieved a -1; and if he was not in the stint he recived a 0. A corresponding response vector of raw plus-minus in terms of the  home team for each stint was also created. From here, I ran ridge regression with the raw plus-minus vector as the response and each player as a predictor, yielding a coefficient for each player. Arranging theese coefficients in descending order gave me me rank-ordered list of NBA players to use for player comparisons.

Data: The data was sourced from NBAstuffer and a zip file is attached as "NBA 2018-19.csv.zip."

Code: I worked in R and relied heavily on the dplyr and tidyr packages to transform the play-by-play data into the design matrix needed. The code is attached as "NBA RAPM 1819.R."

Results: The rankings are attached as "RAPM1819 Rankings.xlsx."
