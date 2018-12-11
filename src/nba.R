# Uncomment and run to install packages used in project.
# install.packages(c("plyr", "dplyr", "e1071", "class", "party",
#                    "ggplot2", "ggpubr", "datasets", "graphics",
#                    "arules", "arulesViz"))
library(plyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(datasets)
require(graphics)
library(e1071) # Naive Bayes
library(class) # kNN
library(party) # C4.5/ID3 (tree)
library(arules) # Association rules
library(arulesViz) # Association rules

# Tweaking values
desired_position = "PG"
picked_year = 2017
# Set seed for all functions
set.seed(picked_year)
# Probabilities for each sample function
picked_prob <- c(0.67, 0.33)
# Used PCs
PC.one = 1
PC.two = 2
# Use PCA data in Hierarchical Clustering
use_PCA = 0

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
seasons_stats.cleaned$Pos <- as.character(seasons_stats.cleaned$Pos)
seasons_stats.cleaned$Pos[seasons_stats.cleaned$Pos != desired_position] <- "no"
seasons_stats.cleaned$Pos[seasons_stats.cleaned$Pos == desired_position] <- "yes"

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
# Samples for Naive Bayes and cTree
ind <- sample(2, nrow(seasons_stats.cleaned), replace = TRUE, prob = picked_prob)
seasons_stats.training <- seasons_stats.cleaned[ind == 1, 1:ncol(seasons_stats.cleaned)]
seasons_stats.test <- seasons_stats.cleaned[ind == 2, 1:ncol(seasons_stats.cleaned)]

seasons_stats.training$Pos <- as.factor(seasons_stats.training$Pos)
seasons_stats.test$Pos <- as.factor(seasons_stats.test$Pos)

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
# C4.5/ID3 (tree calssifier)
seasons_stats.ctree <- ctree(Pos ~ 
                               Height + Weight + PTSavg + TRBavg + ASTavg + STLavg +
                               BLKavg + TOVavg + TS. + FG. + X3P. + X2P. + eFG. + FT.,
                             data = seasons_stats.training)
print(seasons_stats.ctree)

pred_positions.ctree <- predict(seasons_stats.ctree, seasons_stats.test[, 2:(ncol(seasons_stats.test) - 1)])
real_positions.ctree <- seasons_stats.test$Pos
matrix_error.ctree <- table(pred_positions.ctree, real_positions.ctree)
accuracy.ctree <- sum(diag(matrix_error.ctree)) / sum(matrix_error.ctree)

# Who was picked wrong? Let's see...
which_player.ctree <- seasons_stats.test
which_player.ctree$Pred <- pred_positions.ctree
# Add real positions to look inside problem.
which_player.ctree$ExactPos <- seasons_stats$Pos[match(which_player.ctree$Player, seasons_stats$Player)]
# Match the wrong results and summary them.
which_player.ctree.results <- subset(which_player.ctree, Pos != Pred, select = colnames(which_player.ctree)) 
which_player.ctree.positions_error <- count(which_player.ctree.results, which_player.ctree.results$ExactPos)

plot(seasons_stats.ctree)
plot(seasons_stats.ctree, type="simple")

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
# Naive Bayes
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

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
# Samples for kNN
fnorm <- function(x) {
  return (x - min(x)) / (max(x) - min(x))
}

seasons_stats.norm <- fnorm(seasons_stats.cleaned[, 2:(ncol(seasons_stats.cleaned) - 1)])
seasons_stats.norm$Pos <- seasons_stats.cleaned$Pos
seasons_stats.norm$Player <- seasons_stats.cleaned$Player

ind <- sample(2, nrow(seasons_stats.norm), replace = TRUE, prob = picked_prob)
seasons_stats.norm.training <- seasons_stats.norm[ind == 1, 1:ncol(seasons_stats.cleaned)]
seasons_stats.norm.test <- seasons_stats.norm[ind == 2, 1:ncol(seasons_stats.cleaned)]

# kNN - using 2 nearest
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

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
# Association rules


###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
# k-means
seasons_stats.log <- log(seasons_stats.cleaned[, 2:(ncol(seasons_stats.cleaned) - 1)])

# Replace -/+Inf with means
replace_inf <- function(x) {
  x[is.infinite(x)] <- NA
  replace_value <- mean(x, na.rm = TRUE)
  x[is.na(x)] <- replace_value
}

helper_means <- sapply(seasons_stats.log, replace_inf)   # not generally a useful result
helper_means <- as.data.frame(helper_means)

seasons_stats.log$Height[is.infinite(seasons_stats.log$Height)] <- helper_means["Height",]

seasons_stats.log$Weight[is.infinite(seasons_stats.log$Weight)] <- helper_means["Weight",]

seasons_stats.log$PTSavg[is.infinite(seasons_stats.log$PTSavg)] <- helper_means["PTSavg",]

seasons_stats.log$TRBavg[is.infinite(seasons_stats.log$TRBavg)] <- helper_means["TRBavg",]

seasons_stats.log$ASTavg[is.infinite(seasons_stats.log$ASTavg)] <- helper_means["ASTavg",]

seasons_stats.log$STLavg[is.infinite(seasons_stats.log$STLavg)] <- helper_means["STLavg",]

seasons_stats.log$BLKavg[is.infinite(seasons_stats.log$BLKavg)] <- helper_means["BLKavg",]

seasons_stats.log$TOVavg[is.infinite(seasons_stats.log$TOVavg)] <- helper_means["TOVavg",]

seasons_stats.log$TS.[is.infinite(seasons_stats.log$TS.)] <- helper_means["TS.",]

seasons_stats.log$FG.[is.infinite(seasons_stats.log$FG.)] <- helper_means["FG.",]

seasons_stats.log$X3P.[is.infinite(seasons_stats.log$X3P.)] <- helper_means["X3P.",]

seasons_stats.log$X2P.[is.infinite(seasons_stats.log$X2P.)] <- helper_means["X2P.",]

seasons_stats.log$eFG.[is.infinite(seasons_stats.log$eFG.)] <- helper_means["eFG.",]

seasons_stats.log$FT.[is.infinite(seasons_stats.log$FT.)] <- helper_means["FT.",]

# Get back to making data
seasons_stats.stand <- scale(seasons_stats.log, center=TRUE)
seasons_stats.pca <- prcomp(seasons_stats.log)
seasons_stats.final <- predict(seasons_stats.pca)[, 1:ncol(seasons_stats.log)]# [,1:2]

players_num_in_classes <- c(
  length(seasons_stats$Pos[seasons_stats$Pos == "PG"]),
  length(seasons_stats$Pos[seasons_stats$Pos == "SG"]),
  length(seasons_stats$Pos[seasons_stats$Pos == "SF"]),
  length(seasons_stats$Pos[seasons_stats$Pos == "PF"]),
  length(seasons_stats$Pos[seasons_stats$Pos == "C"])
)

players_num_in_classes

# Possible algorithms: "Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"
obj.kmeans <- kmeans(seasons_stats.final, 
                centers = 2, 
                iter.max = 500, 
                nstart = 1, 
                algorithm = c("Lloyd"), 
                trace = FALSE)

results.kmeans <- fitted(obj.kmeans, method = c("centers", "classes"))

# in_each_cluster <- obj.kmeans$size
# in_each_cluster

# if (sum(in_each_cluster) != nrow(iris)) {
#  sprintf("Sum of numbers in clusters IS NOT equal to iris number.")
# } else {
#  sprintf("Sum of numbers in clusters IS equal to iris number.")
# }

#############################################

# Frame for original players database
original_data <- data.frame(PC1 = seasons_stats.final[, PC.one], PC2 = seasons_stats.final[, PC.two], group = seasons_stats$Pos)
original_data[,c(3)] <- sapply(original_data[,c(3)],as.character)

#############################################

# Frame for centers of clusters and cluster themselves
centers_data <- data.frame(PC1 = obj.kmeans$centers[, PC.one], PC2 = obj.kmeans$centers[, PC.two], group = c('c1', 'c2'))

obj.kmeans$cluster <- as.factor(obj.kmeans$cluster)
clusters_data <- data.frame(PC1 = seasons_stats.final[, PC.one], PC2 = seasons_stats.final[, PC.two], group = obj.kmeans$cluster)
clusters_data[,c(3)] <- sapply(clusters_data[,c(3)],as.character) 

# Combine frame with centers and clusters
algorithm_visuals <- rbind(centers_data, clusters_data)

#############################################

# All frames together
all_visuals <- rbind(algorithm_visuals, original_data)

#############################################

# Shows matrix of error
matrix_error.kmeans <- table(obj.kmeans$cluster, seasons_stats.cleaned$Pos)
accuracy.kmeans <- sum(diag(matrix_error.kmeans)) / sum(matrix_error.kmeans)

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
# Hierarchical Clustering
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html

# For year 2017 best results gets with:
# Worse or simillar:
# ward.D
# mcquitty
# complete
# median
# average
# Only gets all "no" right:
# centroid
# single
# However only two methods made two clusters!
# ward.D2
# ward.D with dist(...) ^ 2
# Pretty much the same, only squares, gives more levels.

if (use_PCA == 0) {
  clusters <- hclust(dist(seasons_stats.cleaned[, 2:(ncol(seasons_stats.cleaned) - 1)]), 
                     method = 'ward.D2')
} else {
  clusters <- hclust(dist(seasons_stats.final[, 2:(ncol(seasons_stats.final) - 1)]), 
                     method = 'ward.D2')
}

plot(clusters)

clusterCut <- cutree(clusters, 2)

matrix_error.hclust <- table(clusterCut, seasons_stats.cleaned$Pos)
accuracy.hclust <- sum(diag(matrix_error.hclust)) / sum(matrix_error.hclust)

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
# Save plots:

# All around accuracy:
acc_all <- data.frame("Method" = c("C4.5/ID3", 
                                   "Naive Bayes", 
                                   "kNN", 
                                   "kMeans", 
                                   "Hierarchical Clustering"), 
                      "Accuracy" = c(accuracy.ctree, 
                                     accuracy.bayes, 
                                     accuracy.knn, 
                                     accuracy.kmeans, 
                                     accuracy.hclust) * 100)

acc_plot <-ggplot(acc_all, aes(x = Method, y = Accuracy)) +
                  ylim(0, 100) +
                  geom_bar(stat = "identity", fill = "steelblue") +
                  theme_minimal()

ggsave("plot_acc.pdf", plot = acc_plot, device = "pdf", path = "./",
       scale = 1, width = NA, height = NA, units = c("cm"),
       dpi = 300, limitsize = FALSE)

# Clusters:
plot_of_players <- ggplot() +
  geom_point(original_data, mapping=aes(x = PC1, y = PC2, group=group, col=group), position = position_dodge(width = 0.1))

plot_of_clusters <- ggplot() +
  geom_point(algorithm_visuals, mapping=aes(x = PC1, y = PC2, group=group, col=group), position = position_dodge(width = 0.1))

plot_of_all <- ggplot() +
  geom_point(all_visuals, mapping=aes(x = PC1, y = PC2, group=group, col=group), position = position_dodge(width = 0.1))

plot_hclust <- ggplot(seasons_stats.cleaned, mapping=aes(x = seasons_stats.final[, PC.one],
                                                         y = seasons_stats.final[, PC.two], 
                                                         color = seasons_stats.cleaned$Pos)) + 
                      geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut) + 
                      scale_color_manual(values = c('black', 'red'))

combine_plots <- ggarrange(plot_of_players,
                           plot_of_clusters, 
                           plot_of_all,
                           plot_hclust,
                           labels = c("Original", "Cluster", "Combine", "Hierarchical"), ncol = 2, nrow = 2)

# plot_of_players
# plot_of_clusters
# plot_of_all

ggsave("plot.pdf", plot = combine_plots, device = "pdf", path = "./",
       scale = 1, width = 60, height = 60, units = c("cm"),
       dpi = 300, limitsize = FALSE)

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
# Print results:
#Bayes#############################################################
accuracy.bayes
matrix_error.bayes
which_player.bayes.positions_error
#cTree#############################################################
accuracy.ctree
matrix_error.ctree
which_player.ctree.positions_error
#kNN###############################################################
accuracy.knn
matrix_error.knn
which_player.knn.positions_error
#kMeans############################################################
matrix_error.kmeans
accuracy.kmeans
#hClust############################################################
matrix_error.hclust
accuracy.hclust