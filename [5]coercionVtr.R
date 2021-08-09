
#coercion assumed without expeling error
x<-c(1,"canada",3)
x
class(x)
#[cr] force corcion
x<-1:5
y<- as.character(x) 
y
as.numeric(y)
#coercion and expeling NAs-> Warning message:NAs introduced by coercion 
x<-c("1","b","3")
as.numeric(x)
#coercion and expeling NAs-> Warning message:NAs introduced by coercion 
library(dslabs)# Using new dataset 
data(na_example)# Checking the structure 
str(na_example)# Find out the mean of the entire dataset 
mean(na_example)# Use is.na to create an logical index ind that tells which entries are NA
ind <- is.na(na_example)# Determine how many NA ind has using the sum function
sum(ind)
#coercion and expeling NAs-> Warning message:NAs introduced by coercion 
library(dslabs)
data(na_example)
ind <- is.na(na_example)# Create the ind vector
mean(na_example)# We saw that this gives an NA
mean(na_example[!ind])# Compute the average, for entries of na_example that are not NA 
