
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 
dat <- na.omit( dat ) # will remove the lines that contain missing values
library(dplyr)
head(dat)
x<- filter(dat,Sex =="M" & Diet == "chow") #Use dplyr to create a vector x with the body weight of all males on the control (chow) diet
x <-as.vector(x$Bodyweight,mode = "any")
mean(x)# What is this population's average?
library(rafalib)
popsd(x)#the rafalib package and use the popsd() function to compute the population standard deviation
set.seed(1) # Set the seed at 1
X <- sample(x,25) #Take a random sample  of size 25 from x
mean(X) #What is the sample average?
library(dplyr)
y<- filter(dat,Sex =="M" & Diet == "hf") 
y <-as.vector(y$Bodyweight,mode = "any")
mean(y)
library(rafalib)
popsd(y)
set.seed(1)
Y<- sample(y,25) 
mean(Y)  
abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )
#diffPop <- (y-x)
#diffSamp <- (Y-X)
#for(i in ((length(diffSamp)+1):length(diffPop))) # resolves problem with different lenght of vectors 
#  + { diffSamp = c(diffSamp, 0)} 
library(dplyr)
x<- filter(dat,Sex =="F" & Diet == "chow") 
x <-as.vector(x$Bodyweight,mode = "any")
mean(x)
library(rafalib) 
popsd(x)
set.seed(2)
X<- sample(x,25) 
mean(X)  

library(dplyr)
y<- filter(dat,Sex =="F" & Diet == "hf") 
y <-as.vector(y$Bodyweight,mode = "any")
mean(y)
library(rafalib) 
popsd(y)
set.seed(2)
Y<- sample(y,25) 
mean(Y) 
abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )

