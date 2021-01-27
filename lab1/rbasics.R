#simple arithmetics
#sum
2018+2
#combined simple operations
27 * (90/2) + 777 - (-28)  
#more complex operations
(sqrt(1000)*log(4) + 27^2 + abs(-4) + asin(0.10))*2 + exp(5) * pi - cos(2*pi/5) + 0.178

#help menus
help.start() #manual
help(log) #function help - man in Linux
apropos("log") #functions containing a string
example(log) #examples using a specific function

#variable assignment
x <- 3 #numerical
x
x = "hello" #string
x
x <- TRUE #boolean
x
X #case sensitive x != X

#special values
#NA means NOT AVAILABLE
#Infinites
1/0
-5/0
#NaN - Not a Number
sqrt(-4)

#workspace - manipulating R objects
objects() 
#remove object
rm(x)
#objects() equivalent to
ls()
#to clear the workspace use
rm(list = ls())
ls() #all wiped

#directories
getwd() #get working directory
setwd("/home/piki/faks/erasmus_semstar/data_mining") #set new working directory
getwd()

x <- 1
y <- 10
# loops
for (i in seq(along=1:10)){
  x<-x+1
}

for (i in seq(along=1:10)){
  x<-x+1
  print(x)
}


while(y>0){
  y=y-1
  print(y)
}

# matrices
B=matrix(c(2,4,3,1,5,7),nrow=3,ncol=2)
B

# transpose
C=t(B)
C

# cbind and rbind matrix definition 
D=matrix(c(4,9),nrow=2,ncol=1)
D

#attach column D to C
F=cbind(C,D)
F

#another definition
G=matrix(c(1,5,7,2,9,3),nrow=3,ncol=2)
G

#sum of matrices
M=G+B
M

#matrix multiplication // remember inner dimensions must match m by n * n by s = m by s
P=G%*%C
dim(G)
dim(C)
P

#determinant of P
dP=det(P)
dP

#graphics
t=-1:10

for (i in seq(along=t)){
  yc<-sin(t)
}

plot(t,yc)
plot(t,yc,type="l")
plot(t,yc,type="s")


# import data
library(XLConnect)
install.packages('XLConnect', dependencies = TRUE)
#setwd("C:\Users\Kasutaja\Desktop\Data_Mining_19")
#setwd("C:/Users/Kasutaja/Documents")
getwd()
install.packages("openxlsx", dependencies = TRUE)
wb <- loadWorkbook("Book1.xlsx")
mydata<-readWorksheet(wb,sheet="Sheet1")


library(xlsx)
mydata2=read.xlsx2("C:/Users/Kasutaja/Documents/Book1.xlsx", 1, rep("numeric", 3))
coln(mydata2)

#https://www.rdocumentation.org/packages/xlsx/versions/0.6.3/topics/read.xlsx


AA<- matrix(, nrow=4,ncol=3)
AA<-as.matrix(mydata)

C=AA%*%P



#functions
# Define a simple function
myFirstFun<-function(n)
{
  n*n   
}

#calling the function
i = 3
myFirstFun(i)

#another example function
fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}

fahrenheit_to_celsius(32)

#documentation
#https://www.rdocumentation.org

#very interesting and complete introduction to R programming
#https://swcarpentry.github.io/r-novice-inflammation/


