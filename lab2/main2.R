###################################################
###### TASK 1 #####################################
###################################################

rm(list=ls())
source("/home/piki/faks/erasmus_semstar/data_mining/lab2/second_lab_assignment_Nikola_Tomažin.R")

data(iris)
set.seed(100)
subid    = sample(1:150,50)
iris.dat = as.data.frame(iris[subid,1:4])
iris.lab = as.data.frame(iris[subid, 5])
FisherScore(iris.dat[1:2], iris.lab)
FisherScore(iris.dat[2:3], iris.lab)
FisherScore(iris.dat[c(2,4)], iris.lab)
FisherScore(iris.dat[3:4], iris.lab)

gini.index(iris.dat, iris.lab, feature_name = "Sepal.Width")
gini.index(iris.dat, iris.lab, feature_name = "Petal.Width")


data = read.csv("/home/piki/faks/erasmus_semstar/data_mining/lab2/weather.csv", header = TRUE, sep = ",", dec = ".")
X = as.data.frame(data[,1:4])
y = as.data.frame(data[,5])

gini.index(X, y, feature_name = "Outlook")
gini.index(X, y, feature_name = "Temp.")
gini.index(X, y, feature_name = "Wind")




###################################################
###### TASK 2 #####################################
###################################################

rm(list=ls())
source("/home/piki/faks/erasmus_semstar/data_mining/lab2/second_lab_assignment_Nikola_Tomažin.R")
library("data.tree")
data = read.csv("/home/piki/faks/erasmus_semstar/data_mining/lab2/weather.csv", header = TRUE, sep = ",", dec = ".")
X = as.data.frame(data[,1:4])
y = as.data.frame(data[,5])

tree = decision.tree.train(X, y, NULL)
plot(tree)

###################################################
###### TASK 3 #####################################
###################################################

rm(list=ls())
source("/home/piki/faks/erasmus_semstar/data_mining/lab2/second_lab_assignment_Nikola_Tomažin.R")
data = c(4.8, 4.1, 6.0, 6.5, 5.8, 5.2, 6.8, 7.4, 6.0, 5.6, 7.5, 7.8, 6.3, 5.9, 8.0, 8.4)
timeSeries = TimeSeries(data, 4)

make_forecast(timeSeries$model, 10, timeSeries$D$ST[1:4])

#install.packages("RJSONIO")
library("RJSONIO")
library("jsonlite")
data <- fromJSON(url("https://www.koronavirus.hr/json/?action=podaci"))
plot(c(1:length(data$Datum)), rev(data$SlucajeviHrvatska), "l", main="Number of infected")
data$Daily.cases = get.daily.cases(data);
plot(c(1:length(data$Datum)), rev(data$Daily.cases), "l", main="Number of daily cases")

timeSeries = TimeSeriesPoly(rev(data$Daily.cases), 7)
make_forecast(timeSeries$model, 50, timeSeries$D$ST[1:4])

