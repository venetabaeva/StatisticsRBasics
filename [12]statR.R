#library(rafalib)
#library(swirl)
#What is the average of these numbers?
x <- c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)
avg <- function(x){
  s <- sum(x)
  n <- length(x)# Note: the variables defined inside the F are not saved in the workspace; the values of the variables are changed ony during the F's call
  s/n
}
avg(x)
#Use a for loop to determine the value of ∑25i=1i2
sum <- 0
for(i in 1:25)
  sum <- sum + i^2
sum
#The cars dataset is available in base R. You can type cars to see it. Use the class() function to determine what type of object is cars
str(cars)
class(cars)
#How many rows does the cars object have?
nrow(cars)
#What is the name of the second column of cars?
head(cars)
#The simplest way to extract the columns of a matrix or data.frame is using [. For example you can access the second column with cars[,2].
#What is the average distance traveled in this dataset?
mean(cars[,2])
#Which row of cars has a a distance of 85?
ind<-which(cars$dist == 85)
ind
#load .csv
filename <- "femaleMiceWeights.csv"
dat<- read.csv(filename, header = TRUE,sep = ",")
head(dat)
