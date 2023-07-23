# 1. Preparation

# Loading required packages
library(readxl)
library(tidyr)
library(dplyr)
library(tibble)
library(MASS)
library(ggplot2)
library(glmnet)

# Reading the data into R
NBA1819 <- read.csv("~/Desktop/Sports Analytics/NBA PM/NBA Seasons/NBA 2018-19.csv")

# Creating a copy of the data to work with
NBA1819NEW <- NBA1819

# Replacing blanks in the player columns with "Ghost"
NBA1819NEW <- NBA1819NEW %>%
  mutate(a1 = replace(a1, a1 == "", "Ghost"),
         a2 = replace(a2, a2 == "", "Ghost"),
         a3 = replace(a3, a3 == "", "Ghost"),
         a4 = replace(a4, a4 == "", "Ghost"),
         a5 = replace(a5, a5 == "", "Ghost"),
         h1 = replace(h1, h1 == "", "Ghost"),
         h2 = replace(h2, h2 == "", "Ghost"),
         h3 = replace(h3, h3 == "", "Ghost"),
         h4 = replace(h4, h4 == "", "Ghost"),
         h5 = replace(h5, h5 == "", "Ghost"))

# Changing data type of key variables
NBA1819NEW$team <- as.factor(NBA1819NEW$team)
NBA1819NEW$a1 <- as.factor(NBA1819NEW$a1)
NBA1819NEW$a2 <- as.factor(NBA1819NEW$a2)
NBA1819NEW$a3 <- as.factor(NBA1819NEW$a3)
NBA1819NEW$a4 <- as.factor(NBA1819NEW$a4)
NBA1819NEW$a5 <- as.factor(NBA1819NEW$a5)
NBA1819NEW$h1 <- as.factor(NBA1819NEW$h1)
NBA1819NEW$h2 <- as.factor(NBA1819NEW$h2)
NBA1819NEW$h3 <- as.factor(NBA1819NEW$h3)
NBA1819NEW$h4 <- as.factor(NBA1819NEW$h4)
NBA1819NEW$h5 <- as.factor(NBA1819NEW$h5)


# Removing unnecessary columns
NBA1819NEW <- NBA1819NEW[, c(1, # game ID
                             4:13, # 10 players on the floor
                             14, # period of the game
                             15:16, # aggregate scores
                             18, # elapsed time
                             22, # event type
                             33) # points scored on play
]

# Renaming the columns
names(NBA1819NEW) <- c("game", paste0("a", 1:5), paste0("h", 1:5), 
                       "period", "away", "home", "elapsed", "event", "change")

# Creating an elapsed seconds variable
NBA1819NEW <- separate(NBA1819NEW, elapsed, c("hours", "minutes", "seconds"), sep = ":")
NBA1819NEW$elapsed_seconds_period <- (as.numeric(NBA1819NEW$hours) * 3600) + 
  (as.numeric(NBA1819NEW$minutes) * 60) + as.numeric(NBA1819NEW$seconds)
NBA1819NEW$period <- as.numeric(NBA1819NEW$period)
elapsed_seconds <- vector("numeric")
for (i in 1:nrow(NBA1819NEW)) {
  if (NBA1819NEW[i, 12] < 6) { # Games that went to one OT or less
    elapsed_seconds <- c(elapsed_seconds, 
                         NBA1819NEW[i, 20] + (720 * (NBA1819NEW[i, 12] - 1)))
    print(i)
  }
  else if (NBA1819NEW[i, 12] >= 6) { # Games that went to more than one OT
    elapsed_seconds <- c(elapsed_seconds, 
                         NBA1819NEW[i, 20] + (2880) # 2880 is the length of a full game
                         + (300 * (NBA1819NEW[i, 12] - 5))) # Length of fully completed previous OTs
    print(i)
  }
}
NBA1819NEW$elapsed_seconds <- elapsed_seconds

# Creating a time change variable, using the elapsed seconds variable to do so
time_change <- vector("numeric")
for (i in 1:length(elapsed_seconds)) {
  if (elapsed_seconds[i] == 0){
    time_change[i] = 0 # Resetting elapsed seconds at 0 to start each game
  } else {
    time_change[i] = elapsed_seconds[i] - elapsed_seconds[i-1]
  }
}
NBA1819NEW$time_change <- time_change

# Filtering the data to remove more unnecessary columns
NBA1819NEW <- NBA1819NEW[, - c(15:17, 20)]



# 2. Building the design matrix and response vector

# Creating the X, Y, and Time data frames
X <- data.frame()
Y <- data.frame()
Time <- data.frame()
game <- c()

# Filling the X, Y, and Time data frames
for(i in unique(NBA1819NEW$game)) {
  # Counting the games that are run through
  game <- c(game, i)
  
  # Subsetting the data to only use the game of interest
  game_data <- filter(NBA1819NEW, game == i)
  
  # Creating new columns to store the event plus/minus values for each team
  game_data$home_l <- c(0, game_data$home[-length(game_data$home)])
  game_data$away_l <- c(0, game_data$away[-length(game_data$away)])
  game_data$home_change <- ifelse(game_data$home == game_data$home_l, 
                                  -game_data$change, 
                                  game_data$change)
  game_data$away_change <- ifelse(game_data$away == game_data$away_l, 
                                  -game_data$change, 
                                  game_data$change)
  
  # Subsetting the dataset to focus on the lineups, plus/minus, and the time elapsed for each event
  game_data <- game_data %>%
    dplyr::select(h1:h5, a1:a5, home_change, away_change, time_change)
  
  # Alphabetizing the lineups for each team 
  game_data[,1:5] <- t(apply(game_data[,1:5], 1, sort))
  game_data[,6:10] <- t(apply(game_data[,6:10], 1, sort)) 
  
  # Condensing the data down into one row for each unique lineup with its associated plus/minus and time spent on the court 
  game_data <- game_data %>%
    group_by(h1, h2, h3, h4, h5, a1, a2, a3, a4, a5) %>%
    dplyr::summarise(points = sum(home_change, na.rm = TRUE), 
                     time = sum(time_change, na.rm = TRUE)) %>%
    rownames_to_column("lineup")
  
  # Removing lineups with time = 0
  game_data <- filter(game_data, time != 0)
  
  # Reorganizing game_data such that each player from each stint has its own row
  des <- game_data %>%
    gather(key = "position", value = "name", h1:a5)
  
  # Beginning of method to code -1s into design matrix for away team
  home_positions <- c("h1", "h2", "h3", "h4", "h5")
  home <- vector("numeric")
  for (i in 1:nrow(des)) {
    if (des[i, 4] %in% home_positions == TRUE) {
      home = c(home, 1)
    } else if (des[i, 4] %in% home_positions == FALSE) {
      home = c(home, -1)
    }
  }
  des$home <- home
  
  # Creating Y_game
  Y_game <- des %>% 
    dplyr::select(lineup, points) %>% 
    unique() %>%
    mutate(lineup = as.numeric(lineup)) %>%
    arrange(lineup)
  
  # Creating Time_game
  Time_game <- des %>% 
    dplyr::select(lineup, time) %>% 
    unique() %>%
    mutate(lineup = as.numeric(lineup)) %>%
    arrange(lineup)
  
  # Creating X_game
  X_game <- table(des$lineup, des$name, des$home) 
  X_game <- as.data.frame(X_game)
  X_game$Var3 <- as.numeric(levels(X_game$Var3))[X_game$Var3]
  X_game$in_game <- X_game$Var3 * X_game$Freq
  X_game <- X_game[,c(1,2,5)]
  X_game <- filter(X_game, in_game != 0)
  X_game <- pivot_wider(X_game, names_from = Var2, values_from = in_game)
  names(X_game)[1] <- "lineup"
  X_game$lineup <- as.numeric(levels(X_game$lineup))[X_game$lineup]
  X_game[is.na(X_game)] = 0
  
  # Attaching X_game, Y_game and Time_game together to properly match up the lineup order
  final <- left_join(Y_game, X_game)
  final <- left_join(Time_game, final)
  
  # Subsetting final to get each component individually
  X_game <- final[,-c(1:3)]
  Y_game <- final[,3]
  Time_game <- final[,2]
  
  # Appending the output from each game to the overall output
  if (i == 1) {
    X <- X_game
    Y <- Y_game
    Time <- Time_game
  } 
  else if (i > 1) {
    X <- bind_rows(X, X_game)
    Y <- bind_rows(Y, Y_game)
    Time <- bind_rows(Time, Time_game)
  }
}

# Converting NAs to zeros
X[is.na(X)] = 0

# Calculating raw PM
points <- as.matrix(Y$points)
X <- as.matrix(X)
X_T <- t(X)
plus_minus <- X_T %*% points

# Recording the amount of seconds played by each player
time <- as.matrix(Time$time)
playing_time <- abs(X_T) %*% time



# 3. Finding coefficients

# Getting rid of the players who played less than 250 minutes (15000 seconds)
playing_time_new <- t(playing_time)
X_new <- as.data.frame(X)
X_new <- rbind(X_new, playing_time_new)
get_rid <- vector("numeric")
for (i in 1:ncol(X_new)) {
  if (X_new[nrow(X_new), i] < 15000) {
    get_rid <- c(get_rid, i)
  }
}
X_new <- X_new[,-get_rid]
playing_time_new <- t(X_new[nrow(X_new),])
X_new <- X_new[-nrow(X_new),]

# Combining design matrix and response vector
Together <- cbind(Y, X_new)

# Finding the optimal lambda value for ridge regression
ridgeseq <- 10^(seq(-3, 5, length = 100))
modridge <- lm.ridge(points ~ ., data = Together, lambda = ridgeseq)
lambda_ridge <- ridgeseq[which.min(modridge$GCV)]

# Finding the regularized adjusted plus-minus (RAPM) coefficients
RAPM <- lm.ridge(points ~ ., data = Together, lambda = lambda_ridge)$coef
RAPM <- as.data.frame(RAPM)
RAPM <- tibble::rownames_to_column(RAPM, "Player")

# Saving the rankings as a csv
write.csv(RAPM, "rapm1819")