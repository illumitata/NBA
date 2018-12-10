# Uncomment and run to install packages used in project.
# install.packages(c("plyr", "dplyr", "e1071", "class"))
library(plyr)
library(dplyr)
library(e1071) # Naive Bayes
library(class) # kNN

# Loading data for project.
# seasons_stats stands for csv file with stats of each player from each season.
# players_stats stands for csv file with players info name, weight, height etc.
# In EU file we can't find any data about position, instead we got converted
# height and weight values to more suitable system for me.
seasons_stats <- read.csv("./data/Seasons_Stats.csv", header = TRUE)
players_stats_eu <- read.csv("./data/Players.csv", header = TRUE)

# Time for making things right. We got to pick some players based on these rules:
# 1) From season 1980 up to max possible 2017. Why tho mane?
#    NBA was poorly documented at the begining since back in the day there were 
#    many leauges and sport was considered as hobby usually for collage students.
#    Moreover, in 1980 NBA introduced 3pt line with 3pt shot (makes sense right?).
#    Why is it important? We will talk about it later.
# 2) Only players from one season are allowed. Why tho bruh?
#    It's like every sport, it evolves with time. Example, in 80's being over
#    200cm and going nuts on offence assisting other players (shout out to Magic Johnson)
#    was something unique, one in a million. Today, players are stronger (more weight) and
#    got extra height over guys from past. Right now hybrid players like Lebron James or
#    Giannis Antetokounmpo can take over the role of any player on the court.
#    Thus taking different players from different years or even "era" will result in
#    wrong prediction of position. (Don't be Flash, don't mess the timelines)
#
# Now it's time to take some data out.
picked_year = 2017
seasons_stats <- seasons_stats[which(seasons_stats$Year >= 1980),]
seasons_stats <- seasons_stats[which(seasons_stats$Year == picked_year),]

# Ok we got them all. Now it's time to clear some data.
# Some of the player names contains asterisk(*) symbol attached.
seasons_stats$Player <- gsub("[*].*$" ,"", seasons_stats$Player)
players_stats_eu$Player <- gsub("[*].*$" ,"", players_stats_eu$Player)

# Some of players have double postions (ex. SF-PF) mostly beacuse they played
# for more than a one team in single season. Dataset is built in a way that the first
# position mentioned in TOT (Total in Team column) is the position the player spent
# the most time during the season. So gsub it again and remove other than TOT mentions
# of the player. (TOT is always above other mentions, so just remove by Player name)
seasons_stats <- seasons_stats[!(duplicated(seasons_stats$Player)), ]
seasons_stats$Pos <- gsub("[-].*$" ,"", seasons_stats$Pos)

# Now it's time to remove unwanted columns.
drop_columns <- c("blanl","blank2")
seasons_stats <- seasons_stats[ , !(names(seasons_stats) %in% drop_columns)]

# Now it's time to add some data with weigth and heigth of player.
seasons_stats$Height <- players_stats_eu$height[match(seasons_stats$Player, players_stats_eu$Player)]
seasons_stats$Weight <- players_stats_eu$weight[match(seasons_stats$Player, players_stats_eu$Player)]

# And replace NA's with 0's.
seasons_stats[is.na(seasons_stats)] <- 0

# Now let's think what we really want from our dataset.
# Total numbers of rebounds or assists are misleading. Some short players can grab more boards
# than centers just because they played more games that season.It's time to make some avg and
# clean up the data to use just the things that matter.
# We taking basic stats: rebounds, assists, steals, blocks, turnovers, points.
# Then we calculating averages based on how many games where played by player.
seasons_stats$TRBavg <- seasons_stats$TRB / seasons_stats$G
seasons_stats$ASTavg <- seasons_stats$AST / seasons_stats$G
seasons_stats$STLavg <- seasons_stats$STL / seasons_stats$G
seasons_stats$BLKavg <- seasons_stats$BLK / seasons_stats$G
seasons_stats$TOVavg <- seasons_stats$TOV / seasons_stats$G
seasons_stats$PTSavg <- seasons_stats$PTS / seasons_stats$G

seasons_stats.cleaned <- select(seasons_stats, 
                                Player, 
                                Height,
                                Weight,
                                PTSavg,
                                TRBavg,
                                ASTavg,
                                STLavg,
                                BLKavg,
                                TOVavg,
                                TS.,
                                FG.,
                                X3P.,
                                X2P.,
                                eFG.,
                                FT.,
                                Pos)

# Now we have clean data from picked season. So.. what are we looking for? It's time to make classes!
# We will be looking for players classified by Position (POS). Let's change column a little bit.
desired_position = "PG"
seasons_stats.cleaned$Pos <- as.character(seasons_stats.cleaned$Pos)
seasons_stats.cleaned$Pos[seasons_stats.cleaned$Pos != desired_position] <- "no"
seasons_stats.cleaned$Pos[seasons_stats.cleaned$Pos == desired_position] <- "yes"

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################

# Set seed for all functions
set.seed(picked_year)

# Probabilities for each sample function
picked_prob <- c(0.67, 0.33)

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
# Naive Bayes norm
ind <- sample(2, nrow(seasons_stats.cleaned), replace = TRUE, prob = picked_prob)
seasons_stats.training <- seasons_stats.cleaned[ind == 1, 1:ncol(seasons_stats.cleaned)]
seasons_stats.test <- seasons_stats.cleaned[ind == 2, 1:ncol(seasons_stats.cleaned)]

seasons_stats.training$Pos <- as.factor(seasons_stats.training$Pos)
seasons_stats.test$Pos <- as.factor(seasons_stats.test$Pos)

seasons_stats.model <-naiveBayes(Pos ~ 
                                   Height + Weight + PTSavg + TRBavg + ASTavg + STLavg +
                                   BLKavg + TOVavg + TS. + FG. + X3P. + X2P. + eFG. + FT.,
                                 data = seasons_stats.training)
# print(seasons_stats.model)

# class(seasons_stats.test$Pos)
# class(predict(seasons_stats.model, seasons_stats.test[, 1:(ncol(seasons_stats.test) - 1)]))

pred_positions.bayes <- predict(seasons_stats.model, seasons_stats.test[, 2:(ncol(seasons_stats.test) - 1)])
real_positions.bayes <- seasons_stats.test$Pos
matrix_error.bayes <- table(pred_positions.bayes, real_positions.bayes)
accuracy.bayes <- sum(diag(matrix_error.bayes)) / sum(matrix_error.bayes)

# Who was picked wrong? Let's see...
which_player.bayes <- seasons_stats.test
which_player.bayes$Pred <- pred_positions.bayes
# Add real positions to look inside problem.
which_player.bayes$ExactPos <- seasons_stats$Pos[match(which_player.bayes$Player, seasons_stats$Player)]
# Match the wrong results and summary them.
which_player.bayes.results <- subset(which_player.bayes, Pos != Pred, select = colnames(which_player.bayes)) 
which_player.bayes.positions_error <- count(which_player.bayes.results, which_player.bayes.results$ExactPos)

accuracy.bayes
matrix_error.bayes
which_player.bayes.positions_error

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################

fnorm <- function(x) {
  return (x-min(x))/(max(x)-min(x))
}

seasons_stats.norm <- fnorm(seasons_stats.cleaned[, 2:(ncol(seasons_stats.cleaned) - 1)])
seasons_stats.norm$Pos <- seasons_stats.cleaned$Pos
seasons_stats.norm$Player <- seasons_stats.cleaned$Player

ind <- sample(2, nrow(seasons_stats.norm), replace = TRUE, prob = picked_prob)
seasons_stats.norm.training <- seasons_stats.norm[ind == 1, 1:ncol(seasons_stats.cleaned)]
seasons_stats.norm.test <- seasons_stats.norm[ind == 2, 1:ncol(seasons_stats.cleaned)]

knn.2 <- knn(seasons_stats.norm.training[, 1:(ncol(seasons_stats.norm.training) - 2)], 
             seasons_stats.norm.test[, 1:(ncol(seasons_stats.norm.test) - 2)], 
             cl = seasons_stats.norm.training$Pos, k = 2, prob = FALSE)

pred_positions.knn <- knn.2
real_positions.knn <- seasons_stats.norm.test$Pos
matrix_error.knn <- table(pred_positions.knn, real_positions.knn)
accuracy.knn <- sum(diag(matrix_error.knn)) / sum(matrix_error.knn)

# Who was picked wrong? Let's see...
which_player.knn <- seasons_stats.norm.test
which_player.knn$Pred <- pred_positions.knn
# Add real positions to look inside problem.
which_player.knn$ExactPos <- seasons_stats$Pos[match(which_player.knn$Player, seasons_stats$Player)]
# Match the wrong results and summary them.
which_player.knn.results <- subset(which_player.knn, Pos != Pred, select = colnames(which_player.knn)) 
which_player.knn.positions_error <- count(which_player.knn.results, which_player.knn.results$ExactPos)

# message(sprintf("%f", accuracy))
accuracy.knn
matrix_error.knn
which_player.knn.positions_error

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################