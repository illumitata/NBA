# Uncomment and run to install packages used in project.
# install.packages(c(""))

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
picked_year = 1980
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

# And replace NA's with specific values according to column type.
# Pretty obscure way, loosing classes. However we can remake them at any time.
seasons_stats <- data.frame(apply(seasons_stats, 
                                  MARGIN = c(1,2),
                                  FUN = function(x) {
                                    if(is.na(x)) {
                                      if(class(x) == "numeric") {
                                        x <- 0.0
                                      }
                                      else {
                                        x <- 0
                                      }
                                    }
                                    else {
                                      x <- x
                                    }
                                  }))