# Uncomment and run to install packages used in project.
# install.packages(c("plyr", "dplyr", "e1071", "class", "party",
#                    "ggplot2", "ggpubr", "datasets", "graphics",
#                    "arules", "arulesViz"))
library(plyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gtable)
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

####################################################
# Helper means for some methods
give_means <- function(x) {
  mean_value <- mean(x, na.rm = TRUE)
}
####################################################

# Loading data for project.
# seasons_stats stands for csv file with stats of each player from each season.
# players_stats stands for csv file with players info name, weight, height etc.
# In EU file we can't find any data about position, instead we got converted
# height and weight values to more suitable system for me.
seasons_stats <- read.csv("~/Studia/IntOblicz/projekt2/data/Seasons_Stats.csv", header = TRUE)
players_stats_eu <- read.csv("~/Studia/IntOblicz/projekt2/data/Players.csv", header = TRUE)

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
# Association rules #TODO

# Prepare data with strings!
# I come up with idea of ABOVE and BELOW avg of league for player stats.
# Using helper means it is time to change some columns.

seasons_stats.strings <- seasons_stats.cleaned

seasons_stats.cleaned.numeric <- seasons_stats.cleaned[, 2:(ncol(seasons_stats.cleaned) - 1)]
helper_means.avg <- sapply(seasons_stats.cleaned.numeric, give_means)
helper_means.avg <- as.data.frame(helper_means.avg)

seasons_stats.strings$Height[seasons_stats.strings$Height >= helper_means.avg["Height", ]] <- "above"
seasons_stats.strings$Weight[seasons_stats.strings$Weight >= helper_means.avg["Weight", ]] <- "above"
seasons_stats.strings$PTSavg[seasons_stats.strings$PTSavg >= helper_means.avg["PTSavg", ]] <- "above"
seasons_stats.strings$TRBavg[seasons_stats.strings$TRBavg >= helper_means.avg["TRBavg", ]] <- "above"
seasons_stats.strings$ASTavg[seasons_stats.strings$ASTavg >= helper_means.avg["ASTavg", ]] <- "above"
seasons_stats.strings$STLavg[seasons_stats.strings$STLavg >= helper_means.avg["STLavg", ]] <- "above"
seasons_stats.strings$BLKavg[seasons_stats.strings$BLKavg >= helper_means.avg["BLKavg", ]] <- "above"
seasons_stats.strings$TOVavg[seasons_stats.strings$TOVavg >= helper_means.avg["TOVavg", ]] <- "above"
seasons_stats.strings$TS.[seasons_stats.strings$TS. >= helper_means.avg["TS.", ]] <- "above"
seasons_stats.strings$FG.[seasons_stats.strings$FG. >= helper_means.avg["FG.", ]] <- "above"
seasons_stats.strings$X3P.[seasons_stats.strings$X3P. >= helper_means.avg["X3P.", ]] <- "above"
seasons_stats.strings$X2P.[seasons_stats.strings$X2P. >= helper_means.avg["X2P.", ]] <- "above"
seasons_stats.strings$eFG.[seasons_stats.strings$eFG. >= helper_means.avg["eFG.", ]] <- "above"
seasons_stats.strings$FT.[seasons_stats.strings$FT. >= helper_means.avg["FT.", ]] <- "above"

seasons_stats.strings$Height[seasons_stats.strings$Height < helper_means.avg["Height", ]] <- "bellow"
seasons_stats.strings$Weight[seasons_stats.strings$Weight < helper_means.avg["Weight", ]] <- "bellow"
seasons_stats.strings$PTSavg[seasons_stats.strings$PTSavg < helper_means.avg["PTSavg", ]] <- "bellow"
seasons_stats.strings$TRBavg[seasons_stats.strings$TRBavg < helper_means.avg["TRBavg", ]] <- "bellow"
seasons_stats.strings$ASTavg[seasons_stats.strings$ASTavg < helper_means.avg["ASTavg", ]] <- "bellow"
seasons_stats.strings$STLavg[seasons_stats.strings$STLavg < helper_means.avg["STLavg", ]] <- "bellow"
seasons_stats.strings$BLKavg[seasons_stats.strings$BLKavg < helper_means.avg["BLKavg", ]] <- "bellow"
seasons_stats.strings$TOVavg[seasons_stats.strings$TOVavg < helper_means.avg["TOVavg", ]] <- "bellow"
seasons_stats.strings$TS.[seasons_stats.strings$TS. < helper_means.avg["TS.", ]] <- "bellow"
seasons_stats.strings$FG.[seasons_stats.strings$FG. < helper_means.avg["FG.", ]] <- "bellow"
seasons_stats.strings$X3P.[seasons_stats.strings$X3P. < helper_means.avg["X3P.", ]] <- "bellow"
seasons_stats.strings$X2P.[seasons_stats.strings$X2P. < helper_means.avg["X2P.", ]] <- "bellow"
seasons_stats.strings$eFG.[seasons_stats.strings$eFG. < helper_means.avg["eFG.", ]] <- "bellow"
seasons_stats.strings$FT.[seasons_stats.strings$FT. < helper_means.avg["FT.", ]] <- "bellow"

seasons_stats.strings[] <- lapply(seasons_stats.strings, factor) # the "[]" keeps the dataframe structure
col_names <- names(seasons_stats.strings)

# Run method
seasons_stats.string.stats <- seasons_stats.strings[c("Height", 
                                                      "Weight", 
                                                      "PTSavg",
                                                      "TRBavg",
                                                      "ASTavg",
                                                      "STLavg",
                                                      "BLKavg",
                                                      "X3P.",
                                                      "FT.",
                                                      "Pos")]
rules <- apriori(seasons_stats.string.stats,
                 parameter = list(minlen = 2, 
                                  maxlen = (ncol(seasons_stats.string.stats)),
                                  supp=0.0099, conf=0.99),
                 appearance = list(rhs=c("Pos=no", "Pos=yes"),
                                   default="lhs"),
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")

subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- FALSE
redundant <- colSums(subset.matrix, na.rm=T) >= 1
# which(redundant)
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

plot(rules.pruned)

# dev.new()
# plot(rules.pruned, method="graph", control=list(type="items"))

# dev.new()
# plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
# k-means
seasons_stats.log <- log(seasons_stats.cleaned[, 2:(ncol(seasons_stats.cleaned) - 1)])

# Replace -/+Inf with meansas
seasons_stats.log$Height[is.infinite(seasons_stats.log$Height)] <- NA
seasons_stats.log$Weight[is.infinite(seasons_stats.log$Weight)] <- NA
seasons_stats.log$PTSavg[is.infinite(seasons_stats.log$PTSavg)] <- NA
seasons_stats.log$TRBavg[is.infinite(seasons_stats.log$TRBavg)] <- NA
seasons_stats.log$ASTavg[is.infinite(seasons_stats.log$ASTavg)] <- NA
seasons_stats.log$STLavg[is.infinite(seasons_stats.log$STLavg)] <- NA
seasons_stats.log$BLKavg[is.infinite(seasons_stats.log$BLKavg)] <- NA
seasons_stats.log$TOVavg[is.infinite(seasons_stats.log$TOVavg)] <- NA
seasons_stats.log$TS.[is.infinite(seasons_stats.log$TS.)] <- NA
seasons_stats.log$FG.[is.infinite(seasons_stats.log$FG.)] <- NA
seasons_stats.log$X3P.[is.infinite(seasons_stats.log$X3P.)] <- NA
seasons_stats.log$X2P.[is.infinite(seasons_stats.log$X2P.)] <- NA
seasons_stats.log$eFG.[is.infinite(seasons_stats.log$eFG.)] <- NA
seasons_stats.log$FT.[is.infinite(seasons_stats.log$FT.)] <- NA

helper_means.log <- sapply(seasons_stats.log, give_means)
helper_means.log <- as.data.frame(helper_means.log)

seasons_stats.log$Height[is.na(seasons_stats.log$Height)] <- helper_means.log["Height",]
seasons_stats.log$Weight[is.na(seasons_stats.log$Weight)] <- helper_means.log["Weight",]
seasons_stats.log$PTSavg[is.na(seasons_stats.log$PTSavg)] <- helper_means.log["PTSavg",]
seasons_stats.log$TRBavg[is.na(seasons_stats.log$TRBavg)] <- helper_means.log["TRBavg",]
seasons_stats.log$ASTavg[is.na(seasons_stats.log$ASTavg)] <- helper_means.log["ASTavg",]
seasons_stats.log$STLavg[is.na(seasons_stats.log$STLavg)] <- helper_means.log["STLavg",]
seasons_stats.log$BLKavg[is.na(seasons_stats.log$BLKavg)] <- helper_means.log["BLKavg",]
seasons_stats.log$TOVavg[is.na(seasons_stats.log$TOVavg)] <- helper_means.log["TOVavg",]
seasons_stats.log$TS.[is.na(seasons_stats.log$TS.)] <- helper_means.log["TS.",]
seasons_stats.log$FG.[is.na(seasons_stats.log$FG.)] <- helper_means.log["FG.",]
seasons_stats.log$X3P.[is.na(seasons_stats.log$X3P.)] <- helper_means.log["X3P.",]
seasons_stats.log$X2P.[is.na(seasons_stats.log$X2P.)] <- helper_means.log["X2P.",]
seasons_stats.log$eFG.[is.na(seasons_stats.log$eFG.)] <- helper_means.log["eFG.",]
seasons_stats.log$FT.[is.na(seasons_stats.log$FT.)] <- helper_means.log["FT.",]

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

original_data.yes_no <- data.frame(PC1 = seasons_stats.final[, PC.one], PC2 = seasons_stats.final[, PC.two], group = seasons_stats.cleaned$Pos)
original_data.yes_no[,c(3)] <- sapply(original_data.yes_no[,c(3)],as.character)

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
all_visuals <- rbind(algorithm_visuals, original_data.yes_no)

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
                  theme_minimal() +
                  labs(x = "", y = "Accuracy", title = "Methods accuracy")

ggsave("plot_acc.pdf", plot = acc_plot, device = "pdf", path = "./",
       scale = 1, width = NA, height = NA, units = c("cm"),
       dpi = 300, limitsize = FALSE)

# Clusters:
algorithm_visuals.clusters <- algorithm_visuals[3:nrow(algorithm_visuals),]
names(algorithm_visuals.clusters) <- c("PC1", "PC2", "Clusters")
algorithm_visuals.clusters$Clusters <- as.character(algorithm_visuals.clusters$Clusters)
algorithm_visuals.clusters$Clusters[algorithm_visuals.clusters$Clusters == "1"] <- "Cluster 1"
algorithm_visuals.clusters$Clusters[algorithm_visuals.clusters$Clusters == "2"] <- "Cluster 2"
algorithm_visuals.clusters$Clusters <- as.factor(algorithm_visuals.clusters$Clusters)

algorithm_visuals.centers <- algorithm_visuals[1:2,]
names(algorithm_visuals.centers) <- c("PC1", "PC2", "Clusters")
algorithm_visuals.centers$Clusters <- as.character(algorithm_visuals.centers$Clusters)
algorithm_visuals.centers$Clusters[algorithm_visuals.centers$Clusters == "c1"] <- "Center 1"
algorithm_visuals.centers$Clusters[algorithm_visuals.centers$Clusters == "c2"] <- "Center 2"
algorithm_visuals.centers$Clusters <- as.factor(algorithm_visuals.centers$Clusters)

names(original_data) <- c("PC1", "PC2", "Clusters")

plot_kclust <- ggplot() +
    geom_point(shape = 16, 
               data = algorithm_visuals.clusters, 
               aes(x = PC1, y = PC2, col = Clusters), 
               alpha = 0.4, size = 3.5) +
    geom_point(shape = 16, 
               data = original_data, 
               aes(x = PC1, y = PC2, col = Clusters)) +
    geom_point(shape = 8, 
               data = algorithm_visuals.centers, 
               aes(x = PC1, y = PC2, col = Clusters), 
               size = 10.0) +
    scale_color_manual(
      breaks = c("Cluster 1", "Cluster 2", "PG","SG","SF", "PF", "C"),
      values = c("#E5CA00", # C
                 "black",  # center 1
                 "red", # center 2
                 "black", # cluster 1
                 "red",  # cluster 2
                 "#35F000", # PF
                 "#0047F0", # PG
                 "#0BD9AD", # SF
                 "#E300E5")) + # SG
    labs(x = "PC1", y = "PC2", title = "kMeans visualisation") +
    theme(
      legend.key = element_rect(colour = "transparent", 
                                fill = "transparent"),
      legend.background = element_rect(fill = "transparent"),
      legend.title=element_blank(),
      legend.justification=c(0,1), legend.position=c(0,1)
    ) +
    guides(colour = guide_legend(override.aes = list(shape = 16, size = 4.0)))

plot_hclust <- ggplot(seasons_stats.cleaned, 
      mapping=aes(x = seasons_stats.final[, PC.one],
      y = seasons_stats.final[, PC.two], 
      color = seasons_stats.cleaned$Pos)) + 
      geom_point(alpha = 0.4, size = 3.5) + 
      geom_point(col = clusterCut) + 
      scale_color_manual(values = c('black', 'red')) +
      labs(x = "PC1", y = "PC2", title = "Hierarchical Clustering visualisation") +
      theme(
        legend.key = element_rect(colour = "transparent", 
                                  fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.title=element_blank(),
        legend.justification=c(0,1), legend.position=c(0,1)
       ) +
       guides(colour = guide_legend(override.aes = list(shape = 16, size = 4.0)))

ggsave("plot_kclust.pdf", plot = plot_kclust, device = "pdf", path = "./",
       scale = 1, width = 15, height = 15, units = c("cm"),
       dpi = 300, limitsize = FALSE)

ggsave("plot_hclust.pdf", plot = plot_hclust, device = "pdf", path = "./",
       scale = 1, width = 15, height = 15, units = c("cm"),
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

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################

# TPR = TP / (TP + FN)
# FPR = FP / (FP + TN)
# TNR = TN / (TN + FP)
# FNR = FN / (FN + TP)
matrix_error.bayes

TP.bayes <- matrix_error.bayes[2,2] # +- 4
TN.bayes <- matrix_error.bayes[1,1] # +- 4
FP.bayes <- matrix_error.bayes[2,1] # +- 4
FN.bayes <- matrix_error.bayes[1,2] # +- 4

TPR.bayes = TP.bayes / (TP.bayes + FN.bayes)
FPR.bayes = FP.bayes / (FP.bayes + TN.bayes)
TNR.bayes = TN.bayes / (TN.bayes + FP.bayes)
FNR.bayes = FN.bayes / (FN.bayes + TP.bayes)

# Distance from ideal method
distance.bayes = sqrt(((0 - FPR.bayes) ^ 2) + ((1 - TPR.bayes) ^ 2))

TP.ctree <- matrix_error.ctree[2,2]
TN.ctree <- matrix_error.ctree[1,1]
FP.ctree <- matrix_error.ctree[2,1]
FN.ctree <- matrix_error.ctree[1,2]

TPR.ctree = TP.ctree / (TP.ctree + FN.ctree)
FPR.ctree = FP.ctree / (FP.ctree + TN.ctree)
TNR.ctree = TN.ctree / (TN.ctree + FP.ctree)
FNR.ctree = FN.ctree / (FN.ctree + TP.ctree)

# Distance from ideal method
distance.ctree = sqrt(((0 - FPR.ctree) ^ 2) + ((1 - TPR.ctree) ^ 2))

TP.knn <- matrix_error.knn[2,2]
TN.knn <- matrix_error.knn[1,1]
FP.knn <- matrix_error.knn[2,1]
FN.knn <- matrix_error.knn[1,2]

TPR.knn = TP.knn / (TP.knn + FN.knn)
FPR.knn = FP.knn / (FP.knn + TN.knn)
TNR.knn = TN.knn / (TN.knn + FP.knn)
FNR.knn = FN.knn / (FN.knn + TP.knn)

# Distance from ideal method
distance.knn = sqrt(((0 - FPR.knn) ^ 2) + ((1 - TPR.knn) ^ 2))

TP.hclust <- matrix_error.hclust[2,2]
TN.hclust <- matrix_error.hclust[1,1]
FP.hclust <- matrix_error.hclust[2,1]
FN.hclust <- matrix_error.hclust[1,2]

TPR.hclust = TP.hclust / (TP.hclust + FN.hclust)
FPR.hclust = FP.hclust / (FP.hclust + TN.hclust)
TNR.hclust = TN.hclust / (TN.hclust + FP.hclust)
FNR.hclust = FN.hclust / (FN.hclust + TP.hclust)

# Distance from ideal method
distance.hclust = sqrt(((0 - FPR.hclust) ^ 2) + ((1 - TPR.hclust) ^ 2))

# more FP gives more FPR and decrease TNR etc.

ROCSpace <- data.frame("Method" = c("Naive Bayes",
                                    "C4.5/ID3", 
                                    "Hierarchical Clustering",
                                    "kNN"),
                       "FPR" = c(FPR.bayes, FPR.ctree, FPR.hclust, FPR.knn), 
                       "TPR" = c(TPR.bayes, TPR.ctree, TPR.hclust, TPR.knn))

plot_ROCspace <- ggplot(ROCSpace, 
                        mapping=aes(x = FPR, y = TPR, col = Method)) + 
                  ylim(0, 1) +
                  xlim(0, 1) +
                  geom_point(alpha = 0.4, size = 3.5) + 
                  # scale_color_manual(values = c('black', 'red')) +
                  labs(x = "FPR", y = "TPR", title = "TPR and FPR for methods") +
                  theme(
                    legend.key = element_rect(colour = "transparent", 
                                              fill = "transparent"),
                    legend.background = element_rect(fill = "transparent"),
                    legend.title=element_blank(),
                    legend.justification=c(1,1), legend.position=c(1,1)
                  ) +
                  guides(colour = guide_legend(override.aes = list(shape = 16, size = 4.0)))

ggsave("plot_ROCspace.pdf", plot = plot_ROCspace, device = "pdf", path = "./",
       scale = 1, width = 15, height = 15, units = c("cm"),
       dpi = 300, limitsize = FALSE)