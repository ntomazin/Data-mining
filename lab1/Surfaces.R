# clear everything
rm(list=ls())

# activate libs for plotting in 3D 
library("scatterplot3d")
library("car")
library("rgl")
library('lattice')
source("/home/piki/faks/erasmus_semstar/data_mining/mydistfun.R")

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
z2<-matrix(0,101,101) #canberra calculations go here
z3<-matrix(0,101,101) #mahalanobis calculations go here


for (i in 1:101){
  elt2[1,1]=x[i]
  for  (j in 1:101){
    elt2[2,1]=y[j]
    z1[i,j]=mydistfun(elt1,elt2,"minkowski", p=1)
    z2[i,j]=mydistfun(elt1,elt2,"canberra")
    #z3[i,j]=mydistfun(elt1,elt2,"mahalanobis", cov=var(cbind(x,y)))
    
  }
}

#persp(x,y,z1)
#par(new=TRUE)
#open3d()
#surface3d(x,y,z2)


persp3d(x, y, z1,alpha=0.5, col="skyblue")
persp3d(x, y, z2,alpha=0.5, col="brown3",add=T)
persp3d(x, y, z3,alpha=0.5, col="green",add=T)
