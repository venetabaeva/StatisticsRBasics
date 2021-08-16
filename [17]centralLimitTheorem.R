#Normal Distribution = Gaussian Distribution = Normal Distribution 
## proportion of numbers between a and b 
## mu (average)
## sigma (standard deviation)
# if the distribution of ones data is approximated by a normal distribution, then one knows, for ones data, what proprotion of the data is in the interval 
# [zscore] standartized units = how many standard deviations from the mean? = substract the mean and divide by standard deviation for each point in the distribution 
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )#x represents the weights for the entire population.
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}

# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}
par(mfrow = c(2,1))
hist(averages5)
hist(averages50)
#For the last set of averages, the ones obtained from a sample size of 50, what proportion are between 23 and 25?
mean(averages50<25 & averages50>23)
#[pnormF] find the proportion of observations below a cutoff x given a normal distribution with mean mu and standard deviation sigma with pnorm(x, mu, sigma) or pnorm( (x-mu)/sigma )
##What is the proportion of observations between 23 and 25 in a normal distribution with average 23.9 and standard deviation 0.43?
mu <- 23.9
sigma <- 0.43
pnorm(25, mu, sigma) - pnorm(23,mu,sigma)  
