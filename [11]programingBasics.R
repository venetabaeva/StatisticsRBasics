#[conditional]
##[if-else]
#if(boolean condition){expressions}else{alternative expressions}
a <- 2
if (a!=0){
  print(1/a)
}else{
  print("No resiprocal for 0.")
}
##[if-else]
library(dslabs)
data(murders)
murder_rate <- murders$total/murders$population*1000000
ind <- which.min(murder_rate)
if (murder_rate[ind] <0.6){
  print(murders$state[ind])
}else{
  print("No state has murder rate that low")
}
##[ifelse] if logical is TRUE, then return 1st answer, if FALSE, then 2nd answer
a <- 0 
ifelse(a>0, 1/a,NA)
##[ifelse] works on vectors; examine each element of the logical vector ; return a corresponding answer
a <- c(0,1,2,-4,5)
result <- ifelse(a>0, 1/a, NA)
##[ifelse] substitue NAs with zeros
data(na_example)
sum(is.na(na_example))
no_nas <- ifelse(is.na(na_example),0,na_example)
sum(is.na(na_example))
##[if-else]
x <- c(1,2,-3,4)
if(all(x>0)){
  print("All Positives")
} else{
  print("Not All Positives")
}
##[ifelse]
str(murders)
x <- murders$state
xChar <- nchar(x)
y <- murders$abb
new_names<- ifelse(xChar >8,y,x)
new_names
##[any]take a vector of logicals ; return true, if any of the entries is true 
z <- c(TRUE,TRUE,FALSE)
any(z)
##[all]take a vector of logicals; return  TRUE, if all the entries are true 
z <- c(TRUE,TRUE,FALSE)
all(z)
#[any][all]
x <- c(TRUE, FALSE)
any(x)
x <- c(TRUE, TRUE)
any(x)
x <- c(TRUE, FALSE)
all(x)
x <- c(TRUE, TRUE)
all(x)
x <- c(TRUE, FALSE)
any(!x)
x <- c(TRUE, TRUE)
any(!x)
x <- c(TRUE, FALSE)
all(!x)
x <- c(TRUE, TRUE)
all(!x)
##Note: R functions can be created 
#[meanF] sum(x)/length(x) 
avg <- function(x){
  s <- sum(x)
  n <- length(x)# Note: the variables defined inside the F are not saved in the workspace; the values of the variables are changed ony during the F's call
  s/n
}
x <- 1:100 
avg(x)
identical (mean(x),avg(x))# check whether mean is identical to the function average 
##Note: R functions can be created 
compute_s_n <- function(n){# Define the function
  x <- 1:n
  sum(x^2)
}
n <- 1:25# Define the vector of n
s_n <- vector("numeric", 25)# Define the vector to store data
for(i in n){
  s_n[i] <- compute_s_n(i)
}
identical(s_n, n*(n+1)*(2*n+1)/6)# Check that s_n is identical to the formula given in the instructions.
#[F]
avg <- function(x,arithmetic=TRUE){ # calculate either geometric, or arithmetic avrg dependign on uder-defined variable
  n <- length(x)
  ifelse(arithmetic,sum(x)/n,prod(x)^(1/n))
}
#[compF] formula that tells what the sum of 1 + 2 + 3 ... 
compute_s_n <- function(n){
x<- 1:n
sum(x)
}
compute_s_n(3) #Note: use R to check the formula
#[for loops] performs same task over and over again; let to define the range that variables are to take; change value as looping; evaluate the expression every time inside the loop 
for(i in 1:5){
  print(i)
}
i #Note: i preserves the last value from the i looping 
#[for loops]
m <-25 
s_n <- vector(length = m)#create an empty vector for storing
 for(n in 1:m){
   s_n[n] <- compute_s_n(n)
 }
n <- 1:m
plot(n,s_n)
lines(n,n*(n+1)/2)#[linesF] check whether lines lies right on the dots
#[for loops]
compute_s_n <- function(n){
  nN <- 0
  for(i in 1:n){
    nN <- nN + i^2
  }
  nN
}
compute_s_n(10)
#[for loops]
compute_s_n <- function(n){# Define a function and store it in `compute_s_n`
  x <- 1:n
  sum(x^2)
}
s_n <- vector("numeric", 25)# Create a vector for storing results
for(n in 1:25){# write a for-loop to store the results in s_n
  s_n[n] <- compute_s_n(n)
}
#[for loops]
compute_s_n <- function(n){# Define the function
  x <- 1:n
  sum(x^2)
}
n <- 1:25# Define the vector of n
s_n <- vector("numeric", 25)# Define the vector to store data
for(i in n){
  s_n[i] <- compute_s_n(i)
}
plot(n,s_n)#  Create the plot 
##Note: R functions can be created 
my_func <- function(x){#function adds 1 to the number it receives as an argument
  y <- x + 1
  y
}
my_func(5)
##Note: R functions can be created 
sum_n <- function(n){
  vtr<- 1:n
  sum(vtr)
}
sum_n(5000)
##Note: R functions can be created 
altman_plot <- function(x,y){
  plot(x+y,y-x)
}
#[lexScope] check at which point which values i available
x <- 8
print(x)
my_func <- function(y){
  x <- 9
  print(x)
  y + x
}
my_func(x)
print(x)
#[functions]
##[applyF]
##[split]
##[cut]
##[quantile]
##[reduce]
##[identical]
##[unique]

