nba <- read.csv("input/Seasons_Stats.csv", header = TRUE)

#Drop the first feature
nba <- nba[,-1]

#Replace the X in the feature names 
colnames(nba) <- gsub("X", "", colnames(nba))

#Filter the observations for 2017
library(dplyr)
nba <- nba %>% filter(Year == "2017") %>% select(c("Year", "Player", "Tm", "Pos", "Age", "G","MP","FG", "FGA", "3P", "3PA", "2P", "2PA","FT", "FTA", "TRB", "AST", "STL", "BLK", "PTS", "VORP", "PER"))

#Normalize on a per game basis for better comparions 
nba_per_game <- data.frame(sapply(nba[,c("MP", "FG", "FGA", "3P", "3PA", "2P","2PA", "FT", "FTA", "TRB", "AST", "STL","BLK", "PTS")],FUN = function(x){x/nba$G}))
colnames(nba_per_game) = paste0(c("MP", "FG", "FGA", "3P", "3PA", "2P","2PA", "FT", "FTA", "TRB", "AST", "STL","BLK", "PTS"), "_pg")
nba <- cbind(nba, nba_per_game)


#Players who switched teams (duplicate observations issue)
number_teams <- data.frame(nba %>% count(Player) %>% arrange(desc(n)) %>% rename("number_teams" = "n"))
nba <- left_join(nba, number_teams, by = "Player")
players_multiple_teams <- nba %>% filter(number_teams >1)
players_minutes_max <- data.frame(players_multiple_teams %>% filter(Tm != "TOT") %>% select(Player, MP) %>% group_by(Player) %>% summarize(max_min = max(MP)))
team_minutes_max <- data.frame(left_join(players_minutes_max, players_multiple_teams[,c("Player", "MP", "Tm")], by = c("max_min" = "MP", "Player")))
player_team_assign <- data.frame(left_join(players_multiple_teams %>% select(Player, Tm) %>% filter(Tm == "TOT"), team_minutes_max %>% select(Player, Tm), by = "Player"))
players_one_team <- data.frame(nba %>% filter(number_teams == 1) %>% select(Player, Tm))
players_one_team$copy <- players_one_team$Tm
library(data.table)
setnames(players_one_team, old = names(players_one_team), new = names(player_team_assign))
final_teams <- rbind(player_team_assign, players_one_team)
nba <- left_join(nba, final_teams, by = c("Player", "Tm" = "Tm.x"))
nba <- nba[, -which(names(nba) %in% c("Tm", "number_teams"))]
library(tibble)
nba <- add_column(nba, nba$Tm.y, .after = "Player")
colnames(nba)[3] <- "Tm"
sapply(nba, function(x) sum(is.na(x)))
nba <- nba[!is.na(nba$Tm), -which(names(nba) %in% "Tm.y")]

#Check for missing values 
sapply(nba, function(x) sum(is.na(x)))


#Attach the salary dataset of the players
salaries <- read.csv("input/Salaries1.csv", header = TRUE)

#Explore the dataset 
names(salaries)

#Check for missing values 
sapply(salaries, function(x) sum(is.na(x)))

#Combine the two datasets to form the final one
finalnba <- left_join(nba, salaries, by = c("Player" = "NAME"))

#View the dataset
str(finalnba)
names(finalnba )

library(tidyr)
library(ggplot2)
library(GGally)
library(corrplot)

#Density plot 
features <- grep("_pg", names(finalnba), value = TRUE)
features <- nba[,features]
features %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(x = value)) + geom_density() + facet_wrap(~name, scales = "free") + labs(title = "Density Plot", x = "")


#Correlation plot 
a <- cor(features)
corrplot(a, type = "lower")


#install.packages("ggfortify")
#install.packages("psych")

library(ggfortify)
library(psych)

#Inspect the scale of features 
round(apply(features, MARGIN = 2, FUN = var), 2)
#Scale them
scaled_features <- scale(features)

#Use PCA
pca <- prcomp(scaled_features)
summary(pca)

#PCA plot
table(finalnba$Pos)
#finalnba$Pos <- droplevels(finalnba$Pos, exclude = if(anyNA(levels(finalnba$Pos)))NULL else NA)
table(finalnba$Pos)
autoplot(pca, data = finalnba, colour = "Pos")      
#Scree plot
#scree(scaled_features)


library(dendextend)

#Calculate the Euclidean distance:
distance <- dist(scaled_features, method = "euclidean")

#single linkage 
single <- hclust(d = distance, method = "single")

#centroid linkage
centroid <- hclust(d = distance, method = "centroid")

#Ward's minimum variance 
ward <- hclust(d = distance, method = "ward.D2")

#plots of all hierarchial clustering
plot(single, hang = -1, main = "Nearest Neighbor Method (Single Linkage)")
plot(centroid, hang = -1, main = "Groups Centroid Linkage")
dendrogram1 <- as.dendrogram(ward)
k <- 4
color <- color_branches(dendrogram1, k = k)
plot(color, main = "Ward's Minimum Variance Method; K=4")



#kmeans
#install.packages("NbClust")
library(NbClust)
library(factoextra)
size <- NbClust(data = scaled_features, max.nc = 6, method = "kmeans", index = "silhouette")
k <- 4
k4 <- kmeans(x = scaled_features, centers = k, nstart = 100, algorithm = "Hartigan-Wong")
finalnba$klab4 <- factor(k4$cluster)

#plot 
autoplot(pca, data = finalnba, colour = "klab4")
fviz_cluster(k4, geom = "point", data = scaled_features)

#Salary Analysis 
library(plyr)
finalnba$optnames <- mapvalues(finalnba$klab4, from = c(1,2,3,4), to = c("Below Average", "Average", "Good", "Excellent"))
finalnba$opt <- finalnba$klab4
ggplot(data = finalnba, aes(x = SALARY, y = PER)) +
  geom_point(aes_string(color = finalnba$optnames)) +
  ggtitle("Performance vs Salary") +
  scale_x_continuous(labels = scales::dollar) +
  labs(colour = "Cluster")


finalnba %>% 
  select(Player, G, MP_pg, Tm, SALARY, PER, optnames) %>%
  filter(optnames == "Below Average") %>% 
  arrange(desc(SALARY)) %>% 
  head()

finalnba %>% 
  select(Player, G, MP_pg, Tm, SALARY, PER, optnames) %>% 
  filter(optnames == "Excellent") %>% 
  arrange(SALARY) %>% 
  head()



