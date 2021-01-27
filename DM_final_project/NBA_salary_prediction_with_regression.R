
# loading the required packages
for (package in c('PerformanceAnalytics', 'plotly', 'tidyverse' , 
                  "GGally", "corrplot", "data.table")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

salary.table <-read.csv("input/NBA_season1718_salary.csv")
ss <- read.csv("input/Seasons_Stats.csv")

## Mutated stats for the 2017/2018 season
stats <-ss %>% filter(Year >= 2017) %>% 
          select(Year:G, MP, PER, FG:PTS) %>% 
          distinct(Player, .keep_all = TRUE) %>% 
          mutate(MPG = MP/G, PPG = PTS/G, APG = AST/G, 
                 RPG = TRB/G, TOPG = TOV/G, BPG = BLK/G, 
                 SPG = STL/G) 
stats_salary <- merge(stats, salary.table, by.x = "Player", by.y = "Player")
names(stats_salary)[40] <- "salary17_18"
stats_salary <- stats_salary[-39]

## Correlation plot
corrplot(cor(stats_salary %>% 
               select(salary17_18, MPG:SPG, 
                      Age, PER, contains("%")), 
             use = "complete.obs"), 
         method = "circle",type = "upper")

stats_salary_cor <- 
  stats_salary %>% 
  select(salary17_18, PPG, MPG, TOPG, RPG, PER, SPG, APG)
ggpairs(stats_salary_cor)

cor(stats_salary_cor)[,"salary17_18"]
# Correlation strength is: PPG > MPG > TOPG > RPG > PER > SPG > APG
# “the more turnovers they make” means that they are more involved in ball movements in games, which means that players who make turnovers are, at some extend, important to their team. and i thought this could be expressed as “agressiveness”


names(stats_salary)[5] <- "Team"
plot_ly(data = stats_salary, x = ~salary17_18, y = ~PPG, color = ~Team,
        hoverinfo = "text",
        text = ~paste("Player: ", Player,
                      "<br>Salary: ", format(salary17_18, big.mark = ","),"$",
                      "<br>PPG: ", round(PPG, digits = 3),
                      "<br>Team: ", Team)) %>% 
  layout(
    title = "Salary vs Point Per Game",
    xaxis = list(title = "Salary USD"),
    yaxis = list(title = "Point per Game")
  )

## Regression
stats_salary %>% 
  ggplot(aes(x = salary17_18, y = PPG)) + 
  geom_point() + 
  geom_smooth(method = "lm") 

stats_salary_regression <- 
  stats_salary %>% select(salary17_18, MPG:SPG)
lm(salary17_18~., data=stats_salary_regression)

#Point per game increases salary by $686,815 per year
#The more assists they make the more salary they get

salary_prediction <- function(m, point, minutes, turn_over){
  pre_new <- predict(m, data.frame(PPG = point, MPG = minutes, TOPG = turn_over))
  msg <- paste("PPG:", point, ",MPG:", minutes, ",TOPG:", turn_over, " ==> Expected Salary: $", format(round(pre_new), big.mark = ","), sep = "")
  print(msg)
}

salary_prediction_extended <- function(m, point, assists, rebounds, minutes, turn_over){
  pre_new <- predict(m, data.frame(PPG = point, APG = assists, RPG = rebounds,MPG = minutes, TOPG = turn_over))
  msg <- paste("PPG:", point, ",MPG:", minutes, ",TOPG:", turn_over, " ==> Expected Salary: $", format(round(pre_new), big.mark = ","), sep = "")
  print(msg)
}

model1 <- lm(formula = salary17_18 ~ PPG + MPG + TOPG, data = stats_salary_regression)
salary_prediction(model1, 16.7, 31.2, 1.5) # JJ Redick

model2 <- lm(formula = salary17_18 ~ PPG + APG + RPG + MPG + TOPG, data = stats_salary_regression)
salary_prediction_extended(model2, 16.7, 5, 2, 31.2, 1.5) # JJ Redick
#salary.table[salary.table$Player=="JJ Redick",]

# Devin Booker 
salary_prediction(model1, 23, 35.5, 4)
salary_prediction_extended(model2, 23, 4.3, 3.3, 35.5, 4)
salary.table[salary.table$Player=="Devin Booker",]

# James Harden
salary_prediction(model1, 34.3, 36.5, 4.5)
salary_prediction_extended(model2, 34.3, 7.5, 6.6, 36.5, 4.5)
salary.table[salary.table$Player=="James Harden",]

# Nikola Jokić
salary_prediction(model1, 20, 32, 3)
salary_prediction_extended(model2, 20, 7, 10, 32, 3)
salary.table[salary.table$Player=="Nikola Jokic",]
