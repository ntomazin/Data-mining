# loading the required packages
for (package in c('dplyr', 'ggplot2', 'randomForest' , 
                  "arm", "stringr", "gridExtra",
                  "formattable", "corrplot")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

## importing the datasets

all.nba <- read.csv("input/All.NBA.1984-2018.csv", stringsAsFactors = FALSE, header = TRUE, skip = 1)
nba.players <- read.csv("input/Seasons_Stats.csv", stringsAsFactors = FALSE, header = TRUE)

## first few rows of each data
head(all.nba)
head(nba.players)

#filtering only the newer years, and making year and season column the same
all.nba$Year <- as.numeric(substr(all.nba$Season, start = 1, stop = 4)) + 1
nba.players.postMJ <- nba.players %>% filter(Year > 1998)
all.nba.postMJ <- all.nba %>% filter(Year > 1998 & Year < 2018)


#There are two empty columns to remove ("blanl" and "blank2" - they're not really hiding).
#Some of the columns are computed from combinations of others that could be zero values. For example, shot percentage requires shot attempts, but some players won't have put any up. We'll drop these columns when we create a new per-game dataset. NB: The column names ending in a full stop indicate percentage.
#Beyond this, I'll do a search for NA values in each column to check if there are any unexpected issues - and print this so we can check it.
                                      
nba.players.postMJ$blanl <- NULL
nba.players.postMJ$blank2 <- NULL
colSums(is.na(nba.players.postMJ))                                      

#get rid of anomalies
nba.players.postMJ <- nba.players.postMJ[-c(3819, 4136, 5099, 6069, 7957), ]
nba.players.postMJ <- subset(nba.players.postMJ, !Tm == "TOT")

which(all.nba.postMJ$Tm == "TOT")
all.nba.postMJ[239, 5] <- "ATL"
all.nba.postMJ[180, 5] <- "DEN"



## Taking only the most important 22 columns out of 51
## columns from Minutes to FG are per game
nba.pergame <- nba.players.postMJ %>% mutate(Name = Player, Position = Pos, age = Age, year = Year,  Team = Tm, Games = G, Starts = GS, Minutes = MP/G, Points = PTS/G, Rebounds = TRB/G, Assists = AST/G, Steals = STL/G, Blocks = BLK/G, Turnovers = TOV/G, Fouls = PF/G, FTs = FT/G, Threes = X3P/G, FGs = FG/G, Usage = USG., EfficiencyRating = PER, BoxPlusMinus = BPM, ShootingPercentage = eFG.)
nba.pergame <- nba.pergame[ , c(52:73)]
two.digit.round <- function(x) round(x, 2)
nba.pergame[ , c(8:18)] <- sapply(nba.pergame[ , c(8:18)], two.digit.round)
str(nba.pergame)

plot(nba.pergame$EfficiencyRating, y=NULL, main="With Outliers", xlab="playerID", ylab="PER", pch="*", col="red", cex=1)

nba.pergame <- nba.pergame %>% filter(Games > 10 & Minutes > 5)

plot(nba.pergame$EfficiencyRating, y=NULL, main="Without Outliers", xlab="playerID", ylab="PER", pch="*", col="red", cex=1)


nba.pergame[6047, 24] <- 0
nba.pergame$ID <- str_c(substr(nba.pergame$Name, start = 1, stop = 3), substr(nba.pergame$age, start = 1, stop = 2), substr(nba.pergame$Team, start = 1, stop = 3), substr(nba.pergame$year, start = 3, stop = 4), sep = "")
all.nba.postMJ$ID <- str_c(substr(all.nba.postMJ$Player, start = 1, stop = 3), substr(all.nba.postMJ$Age, start = 1, stop = 2), substr(all.nba.postMJ$Tm, start = 1, stop = 3), substr(all.nba.postMJ$Year, start = 3, stop = 4), sep = "")
nba.pergame$All.NBA <- ifelse(nba.pergame$ID %in% all.nba.postMJ$ID, 1, 0)

sum(nba.pergame$All.NBA)

## Density graphs of player output
points_density <- ggplot(nba.pergame, aes(Points)) + geom_density(fill = "skyblue") + geom_vline(aes(xintercept = mean(Points)), linetype = "dashed")
rebounds_density <- ggplot(nba.pergame, aes(Rebounds)) + geom_density(fill = "mediumorchid1") + geom_vline(aes(xintercept = mean(Rebounds)), linetype = "dashed")
assists_density <- ggplot(nba.pergame, aes(Assists)) + geom_density(fill = "tomato") + geom_vline(aes(xintercept = mean(Assists)), linetype = "dashed")
turnovers_density <- ggplot(nba.pergame, aes(Turnovers)) + geom_density(fill = "mediumaquamarine") + geom_vline(aes(xintercept = mean(Turnovers)), linetype = "dashed")
grid.arrange(points_density, rebounds_density, assists_density, turnovers_density, ncol = 2)

## The inverted u-shaped curve of age and output
nba.by.age <- nba.pergame %>% group_by(age) %>% summarise(Efficiency = mean(EfficiencyRating), Players = length(Name))
ggplot(nba.by.age, aes(age, Efficiency)) + geom_point(aes(size = Players), colour = "peru") + geom_smooth(method = "loess", colour = "seashell4", se = FALSE, linetype = "dashed") + theme_bw()
 
## Correlaction efficiency
PER.usage <- ggplot(nba.pergame, aes(Usage, EfficiencyRating)) + geom_point(colour = "lightsteelblue4", alpha = 0.5) + geom_smooth(method = lm, colour = "navyblue", linetype = "dashed")
PER.minutes <- ggplot(nba.pergame, aes(Minutes, EfficiencyRating)) + geom_point(colour = "lightsteelblue4", alpha = 0.5) + geom_smooth(method = lm, colour = "navyblue", linetype = "dashed")
PER.threes <- ggplot(nba.pergame, aes(Threes, EfficiencyRating)) + geom_point(colour = "lightsteelblue4", alpha = 0.5) + geom_smooth(method = lm, colour = "navyblue", linetype = "dashed")
PER.shooting <- ggplot(nba.pergame, aes(ShootingPercentage, EfficiencyRating)) + geom_point(colour = "lightsteelblue4", alpha = 0.5) + geom_smooth(method = lm, colour = "navyblue", linetype = "dashed")
grid.arrange(PER.minutes, PER.threes, PER.usage, PER.shooting)

#All four are variables are positively related to overall player efficiency, and these relationships seem linear, with a few notable exceptions:
#-The linear relationship between shooting percentage and PER seems to 'top out' at a shooting percentage of around 50%.
#-Given the cluster of threes in the 0-1 range, a linear trend may not be the best approximation.
 
## Predictor correlation
nba.vars.matrix <- as.matrix(nba.pergame[ , c(6:20)])
corrplot(cor(nba.vars.matrix), is.corr = FALSE, method = "circle", type = "upper")                                     


## Estimating relationships between All NBA selection and a set of predictor variables

nba.pergame$All.NBA <- as.factor(nba.pergame$All.NBA) 

log.points <- glm(All.NBA ~ Points, family = binomial, data = nba.pergame) ## run logit
points.probability <- data.frame(Points = seq(0, 40, 0.1)) ## build basis of dataframe to predict probability at each 0.1 point interval
points.prediction <- predict(log.points, points.probability, type = "response") ## run prediction based on logit
points.probability <- cbind(points.probability, points.prediction)
names(points.probability) <- c("Points", "Probability") 
points.gg <- ggplot(points.probability, aes(Points, Probability)) + geom_line() + geom_vline(xintercept = mean(nba.pergame$Points), colour = "deepskyblue", linetype = "dashed") + geom_vline(xintercept = quantile(nba.pergame$Points, 0.99), colour = "sienna2", linetype = "dashed") + annotate("text", x = 24, y = 0.8, label = "99th percentile", angle = 90, colour = "sienna2", size = 3) + annotate("text", x = 7, y = 0.8, label = "Mean average", angle = 90, colour = "deepskyblue", size = 3)  ## build graph
## Now repeat for rebounds, assists and efficiency


## rebounds
log.rebounds <- glm(All.NBA ~ Rebounds, family = binomial, data = nba.pergame)
rebounds.probability <- data.frame(Rebounds = seq(0, 25, 0.1))
rebounds.prediction <- predict(log.rebounds, rebounds.probability, type = "response") 
rebounds.probability <- cbind(rebounds.probability, rebounds.prediction)
names(rebounds.probability) <- c("Rebounds", "Probability") 
rebounds.gg <- ggplot(rebounds.probability, aes(Rebounds, Probability)) + geom_line() + geom_vline(xintercept = mean(nba.pergame$Rebounds), colour = "deepskyblue", linetype = "dashed") + geom_vline(xintercept = quantile(nba.pergame$Rebounds, 0.99), colour = "sienna2", linetype = "dashed") + annotate("text", x = 10.5, y = 0.75, label = "99th percentile", angle = 90, colour = "sienna2", size = 3) + annotate("text", x = 3, y = 0.75, label = "Mean average", angle = 90, colour = "deepskyblue", size = 3) 

## assists
log.assists <- glm(All.NBA ~ Assists, family = binomial, data = nba.pergame)
assists.probability <- data.frame(Assists = seq(0, 20, 0.1))
assists.prediction <- predict(log.assists, assists.probability, type = "response") 
assists.probability <- cbind(assists.probability, assists.prediction)
names(assists.probability) <- c("Assists", "Probability") 
assists.gg <- ggplot(assists.probability, aes(Assists, Probability)) + geom_line() + geom_vline(xintercept = mean(nba.pergame$Assists), colour = "deepskyblue", linetype = "dashed") + geom_vline(xintercept = quantile(nba.pergame$Assists, 0.99), colour = "sienna2", linetype = "dashed") + annotate("text", x = 8, y = 0.75, label = "99th percentile", angle = 90, colour = "sienna2", size = 3) + annotate("text", x = 1.2, y = 0.75, label = "Mean average", angle = 90, colour = "deepskyblue", size = 3) 

## PER
log.PER <- glm(All.NBA ~ EfficiencyRating, family = binomial, data = nba.pergame)
PER.probability <- data.frame(EfficiencyRating = seq(0, 40, 0.1))
PER.prediction <- predict(log.PER, PER.probability, type = "response")
PER.probability <- cbind(PER.probability, PER.prediction)
names(PER.probability) <- c("PER", "Probability")
PER.gg <- ggplot(PER.probability, aes(PER, Probability)) + geom_line() + geom_vline(xintercept = mean(nba.pergame$EfficiencyRating), colour = "deepskyblue", linetype = "dashed") + geom_vline(xintercept = quantile(nba.pergame$EfficiencyRating, 0.99), colour = "sienna2", linetype = "dashed") + annotate("text", x = 24, y = 0.9, label = "99th percentile", angle = 90, colour = "sienna2", size = 3) + annotate("text", x = 11, y = 0.9, label = "Mean average", angle = 90, colour = "deepskyblue", size = 3) 

## print graphs
grid.arrange(points.gg, rebounds.gg, assists.gg, PER.gg, top = "The probability of being selected for the All NBA teams, at different levels of output")



## Multivariate logistic regression
nba.pergame$ShootingPercentage <- nba.pergame$ShootingPercentage * 100
multi.log.mam <- glm(All.NBA ~ Points + Rebounds + Assists + Usage + ShootingPercentage + Steals + Blocks + Turnovers + Fouls + FTs, family = binomial, nba.pergame)
summary(multi.log.mam)
OR.table <- round(exp(cbind(Odds_Ratio = coef(multi.log.mam), confint(multi.log.mam))), 3)
formattable(OR.table)


## Random forest algorithm to predict selection to the All NBA teams

# Spliting data to train and test as 2:1
nba.train <- nba.pergame %>% filter(year < 2012)
nba.test <- nba.pergame %>% filter(year > 2011)
dim(nba.train)
dim(nba.test)

set.seed(123)  ## set a random seed to ensure replicability. 
RFmodel <- randomForest(All.NBA ~ Points + Assists + Rebounds + age + Games + Starts + Minutes + Steals + Blocks + Turnovers + Fouls + FTs + Threes + FGs + Usage + EfficiencyRating + BoxPlusMinus + ShootingPercentage, data = nba.train)
plot(RFmodel) ## check errors
varImpPlot(RFmodel) ## to look at variable importance

# checking on test dataset
RFpredictions.binary <- predict(RFmodel, nba.test, type = "response")
nba.test.check <- data.frame(cbind(nba.test, RFpredictions.binary))
nba.test.check$TruePositive <- ifelse(nba.test.check$All.NBA == 1 & nba.test.check$RFpredictions.binary == 1, 1, 0)
nba.test.check$TrueNegative <- ifelse(nba.test.check$All.NBA == 0 & nba.test.check$RFpredictions.binary == 0, 1, 0)
nba.test.check$FalseNegative <- ifelse(nba.test.check$All.NBA == 1 & nba.test.check$RFpredictions.binary == 0, 1, 0)
nba.test.check$FalsePositive <- ifelse(nba.test.check$All.NBA == 0 & nba.test.check$RFpredictions.binary == 1, 1, 0)
prediction.results <- c(sum(nba.test.check$TruePositive), sum(nba.test.check$TrueNegative), sum(nba.test.check$FalsePositive), sum(nba.test.check$FalseNegative))
predictions <- data.frame(cbind(c("True Positive", "True Negative", "False Positive", "False Negative"), prediction.results))
predictions$prediction.results <- as.numeric(as.character(predictions$prediction.results))
names(predictions) <- c("Type", "Count")
formattable(predictions)

# Correctly identify 54 of the 90 All NBA selections (60%)
# Correctly predicted 2674 of a possible 2689 non-selections (99.4%)

## Season-specific, probabalistic approach
prob.predict.RF <- predict(RFmodel, nba.test, type = "prob")
nba.test.prob <- cbind(nba.test, prob.predict.RF)
names(nba.test.prob)[names(nba.test.prob) == "1"] <- "Probability"
nba.top15 <- nba.test.prob %>% group_by(year) %>% top_n(n = 15, wt = Probability) %>% arrange(year, desc(Probability))
nba.top15$All.NBA <- as.numeric(as.character(nba.top15$All.NBA))
round((sum(nba.top15$All.NBA)/length(nba.top15$All.NBA)*100), 4)
# 74% is better than 60%

## Comparing PER leaders with All-NBA stars
nba.PER.elite <- nba.test %>% group_by(year) %>% top_n(n = 15, wt = EfficiencyRating) %>% arrange(year, desc(EfficiencyRating))
nba.PER.elite$All.NBA <- as.numeric(as.character(nba.PER.elite$All.NBA))
round((sum(nba.PER.elite$All.NBA)/length(nba.PER.elite$All.NBA)*100), 4)
# 74% > 59% so the algorith beats PER in predicting All-NBA teams


## The human bias
which(nba.top15$All.NBA == 0 & nba.top15$Probability > 0.75)
formattable(nba.top15[c(33, 38, 65, 77, 82, 90), ])


## Improvements

#Given the model's tendency to overrate players who are 
#considered defensive liabilities, adding an advanced 
#defensive metric might help.
#Often in All NBA discussions, team wins are cited as a 
#factor that journalists considered when they vote. 
#They don't like to reward players on losing teams. 
#Including team winning percentage as a variable 
#could help at the margins.








