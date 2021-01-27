#################### MAIN ############################333

rm(list=ls())


#################### DIST FUNC ############################333
rm(list=ls())
source("/home/piki/faks/erasmus_semstar/data_mining/first_lab_assignment_Nikola_Tomažin.R")

# activate libs for plotting in 3D 
#install.packages("car")

library("scatterplot3d")
#library("car")
library("rgl")
library('lattice')

# let us prepare some data
x<- c(-50:50)
x<-x*0.1
y=x

#imagine 10 by 10 square with the center at origin
# compute the distance between the origin and each internal point ofthe square
elt1<-matrix(c(0, 0),2,1) #one vector
elt2<-matrix(c(1, 1),2,1) #another vector

#matrices of computed values, z axis
z1<-matrix(0,101,101) #minowski calculations go here
z2<-matrix(0,101,101) #minowski calculations go here
z3<-matrix(0,101,101) #minowski calculations go here
z4<-matrix(0,101,101) #canberra calculations go here
z5<-matrix(0,101,101) #mahalanobis calculations go here



for (i in 1:101){
  elt2[1,1]=x[i]
  for  (j in 1:101){
    elt2[2,1]=y[j]
    z1[i,j]=mydistfun(elt1,elt2,"minkowski", p=1)
    z2[i,j]=mydistfun(elt1,elt2,"minkowski", p=2)
    z3[i,j]=mydistfun(elt1,elt2,"minkowski", p=10)
    z4[i,j]=mydistfun(elt1,elt2,"canberra")
    #z5[i,j]=mydistfun(elt1,elt2,"mahalanobis", cov=var(cbind(x,y)))
    
  }
}

persp3d(x, y, z1,alpha=0.5, col="skyblue")
persp3d(x, y, z2,alpha=0.5, col="brown3",add=T)
persp3d(x, y, z3,alpha=0.5, col="green",add=T)
persp3d(x, y, z4,alpha=0.5,add=T)

x <- rnorm(100, mean=rep(1:3, each=4), sd=0.2)
y <- rnorm(100, mean=rep(c(1,2,1), each=4), sd=0.2)

for (i in 1:101){
  elt2[1,1]=x[i]
  for  (j in 1:101){
    elt2[2,1]=y[j]
    z5[i,j]=mydistfun(elt1,elt2,"mahalanobis", cov=cov(cbind(x,y)))
    
  }
}


############################### ENTROPY ################################
rm(list=ls())
source("/home/piki/faks/erasmus_semstar/data_mining/first_lab_assignment_Nikola_Tomažin.R")
library('MASS')
Sigma <- matrix(c(10,3,3,2),2,2)
Sigma
dataset = mvrnorm(n=100, rep(0, 2), Sigma)
plot(dataset)

computed_entropy = entropy(dataset = dataset, num_of_regions = 10)
cat("Entropy for the first example is: ", computed_entropy)

x <- rnorm(100, mean=rep(1:3, each=4), sd=0.2)
y <- rnorm(100, mean=rep(c(1,2,1), each=4), sd=0.2)
plot(cbind(x,y))

computed_entropy = entropy(dataset = cbind(x,y), num_of_regions = 10)
cat("Entropy for the second example is: ", computed_entropy)

x2 <- rnorm(1000, mean=rep(c(-1, 5), each=4), sd=0.2)
y2 <- rnorm(1000, mean=rep(c(-1, 5), each=4), sd=0.2)
plot(cbind(x2,y2))
computed_entropy = entropy(dataset = cbind(x2,y2), num_of_regions = 10)
cat("Entropy for the second example is: ", computed_entropy)


############################### HOPKINS STATISTICS ################################
x2 <- rnorm(1000, mean=rep(c(-1, 5), each=4), sd=0.2)
y2 <- rnorm(1000, mean=rep(c(-1, 5), each=4), sd=0.2)
plot(cbind(x2,y2))
H = hopkins(dataset = cbind(x2,y2), num_of_points = 100)
cat("Hopkins statistic for the first example is: ", H)

plot(cbind(x,y))
H = hopkins(dataset = cbind(x,y), num_of_points = 30)
cat("Hopkins statistic for the second example is: ", H)

dataset = cbind(sample(1:100, size=100),sample(1:100, size=100))
plot(cbind(sample(1:100, size=100),sample(1:100, size=100)))
H = hopkins(dataset = dataset, num_of_points = 30)
cat("Hopkins statistic for the second example is: ", H)

x2 <- rnorm(1000, mean=rep(c(-1, 1, 2, 4), each=4), sd=0.2)
y2 <- rnorm(1000, mean=rep(c(-1, 1, 2, 2), each=4), sd=0.2)
plot(cbind(x2,y2))
H = hopkins(dataset = cbind(x2,y2), num_of_points = 100)
cat("Hopkins statistic for the first example is: ", H)

dataset = cbind(sample(1:1000, size=1000),sample(1:1000, size=1000))
plot(dataset)
H = hopkins(dataset = dataset, num_of_points = 100)
cat("Hopkins statistic for the second example is: ", H)



############################### K-MEDOIDS ################################
#rm(list=ls())
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("cluster")
#install.packages("clv")
#install.packages("class")

library(ggplot2)
library(reshape2)
library (cluster)
library(clv)

source("/home/piki/faks/erasmus_semstar/data_mining/first_lab_assignment_Nikola_Tomažin.R")
set.seed(1234)
x <- rnorm(100, mean=rep(1:3, each=4), sd=0.2)
y <- rnorm(100, mean=rep(c(1,2,1), each=4), sd=0.2)
data <- data.frame(x, y)
plot(x, y, col="blue", pch=19, cex=1)

#for Euclidian
medoid_data = k_medoid(data, k=3, iterations = 20)
#print(medoid_data$data)
#print(medoid_data$best_medoid)
#print(medoid_data$dissimilarity_matrix)

ggplot() + geom_point(medoid_data$data, mapping=aes(x, y, color=factor(cluster_affilation))) +geom_point(data=medoid_data$best_medoids, mapping=aes(x=x, y=y, size=10))
sil = silhouette(medoid_data$data[,3], dmatrix=medoid_data$dissimilarity_matrix)
plot(sil)

#https://rdrr.io/cran/clv/man/cluster_scatter.html
cls.scatt.diss.mx(medoid_data$dissimilarity_matrix, medoid_data$data[,3])

#for Manhatan
medoid_data = k_medoid(data, k=3, iterations = 20, p=1)

ggplot() + geom_point(medoid_data$data, mapping=aes(x, y, color=factor(cluster_affilation))) +geom_point(data=medoid_data$best_medoids, mapping=aes(x=x, y=y, size=10))
sil = silhouette(medoid_data$data[,3], dmatrix=medoid_data$dissimilarity_matrix)
plot(sil)

#https://rdrr.io/cran/clv/man/cluster_scatter.html
cls.scatt.diss.mx(medoid_data$dissimilarity_matrix, medoid_data$data[,3])

#for Canberra
medoid_data = k_medoid(data, k=3, iterations = 20, metricf = "canberra")
#print(medoid_data$data)
#print(medoid_data$best_medoid)
#print(medoid_data$dissimilarity_matrix)

ggplot() + geom_point(medoid_data$data, mapping=aes(x, y, color=factor(cluster_affilation))) +geom_point(data=medoid_data$best_medoids, mapping=aes(x=x, y=y, size=10))
sil = silhouette(medoid_data$data[,3], dmatrix=medoid_data$dissimilarity_matrix)
plot(sil)

#https://rdrr.io/cran/clv/man/cluster_scatter.html
cls.scatt.diss.mx(medoid_data$dissimilarity_matrix, medoid_data$data[,3])

#for Mahalanobis
medoid_data = k_medoid(data, k=3, iterations = 20, metricf = "mahalanobis", cov=cov(cbind(x,y)))
#print(medoid_data$data)
#print(medoid_data$best_medoid)
#print(medoid_data$dissimilarity_matrix)

ggplot() + geom_point(medoid_data$data, mapping=aes(x, y, color=factor(cluster_affilation))) +geom_point(data=medoid_data$best_medoids, mapping=aes(x=x, y=y, size=10))
sil = silhouette(medoid_data$data[,3], dmatrix=medoid_data$dissimilarity_matrix)
plot(sil)

#https://rdrr.io/cran/clv/man/cluster_scatter.html
cls.scatt.diss.mx(medoid_data$dissimilarity_matrix, medoid_data$data[,3])

