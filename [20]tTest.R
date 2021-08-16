#CLT in conjunction with t-Tests to obtain p - value and obtain confidence values 
library(dplyr)
dat <- read.csv("femaleMiceWeights.csv")
control <-filter(dat, Diet =="chow") %>% 
  select(Bodyweight) %>% unlist 
treatment <-filter(dat, Diet =="hf") %>% 
  select(Bodyweight) %>% unlist 
 N <- length(treatment)
 obs <- mean(treatment) - mean(control)
 #construct t stat =   form a t-statistic by dividing the observed difference by its estimated standard deviation
 se <- sqrt(var(treatment)/N+ #taking the square root of the variance of the sample estimate of the variance divided by n where n is teh sample size
             var(control)/N) # gives us standard error estimate.
 tstat <- obs/se
 tstat
 #will to assume that,  that the normal approximation holds for the null distribution =  don't need to have access to the population data.
 #St-statistic, central limit theorem, tells that  the null distribution is approximated by a normal distribution with mean 0 and variance 1.
 1- pnorm(tstat) # tells  what proportion of normally distributed data, with means 0 and standard deviation 1,are lower than whatever value you put here.
 2*(1- pnorm(tstat))
 # how good of an approximation this is =>access the population 
 pop <- read.csv("femaleControlsPopulation.csv")
 pop<- unlist(pop)
 n<-10000
 nulls <- vector("numeric",n)
 for(i in 1:n){
   chowVals <- sample(pop,N)
   hfVals<- sample(pop,N)
   se <- sqrt(var(treatment)/N+ #taking the square root of the variance of the sample estimate of the variance divided by n where n is teh sample size
                var(control)/N) # gives us standard error estimate.
   nulls[i]<- (mean(hfVals) - mean(chowVals))/se
 }
 # Q-Q plot against the normal for these nulls, it should be right on the line
 # Q-Q plot should have slope 1 and intercept 0
 # check through doing abline 0, 1 = a line with intercept 0 and slope 1 => go right through the data
 library(rafalib)
 mypar()
   qqnorm(nulls)
   abline(0,1)
   qqline(nulls)
 #t-test practice
   library(dplyr)
   dat <- read.csv("femaleMiceWeights.csv")
   control <-filter(dat, Diet =="chow") %>% 
     select(Bodyweight) %>% unlist 
   treatment <-filter(dat, Diet =="hf") %>% 
     select(Bodyweight) %>% unlist 
  tTest<-  t.test(treatment,control)
tTest 
# an assumption we're making when we use that distribution, the original data is normally distributed, meaning that if we had access to the entire population
qqnorm(control) # now we can test that somewhat by making q-q plots of the sample
qqline(control)
qqnorm(treatment) # now we can test that somewhat by making q-q plots of the sample
qqline(treatment)
#download if
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename) 
set.seed(1)
n <- 100 #rolling die 
sides <- 6 #see 6 s
p <- 1/sides #mean of 6 = either 6, or no (success probability)
zs <- replicate(10000,{ # roll die 10000 and keep the proportion 
  x <- sample(1:sides,n,replace=TRUE)# simulate a random variable = the proportion of times we see a 6 when rolling n=100 die
  (mean(x==6) - p) / sqrt(p*(1-p)/n) #mean/ variance = CLT 
}) 
mean(abs(zs) > 2) #Set the seed to 1, then use replicate() to perform the simulation, and report what proportion of times z was larger than 2 in absolute value (CLT says it should be about 0.05)
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
#an asymptotic result, meaning it is closer and closer to being a perfect approximation as the sample size increases
#need to decide if it is appropriate for actual sample sizes
ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}
#
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist # think of  as a random sample from the population of all mice in the control die
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist #   as a random sample from the population of all mice in the high fat diet
mean(X) #estimate average of the control population with the sample average 
sd(X)#sample standard deviation = estimation of population standard deviation
2 * ( 1-pnorm(2/sd(X) * sqrt(12) ) ) #Use the CLT to approximate the probability that our estimate X¯is off by more than 2 grams from μX
#Now we introduce the concept of a null hypothesis. 
#We don’t know μX or μY.We want to quantify what the data say about the possibility that the diet has no effect: μX= μY.
#If we use CLT, then we approximate the distribution of X¯as normal with mean μXand standard deviation σXM√and the distribution of Y¯as normal with mean μYand standard deviation σYN√with Mand Nthe sample sizes for Xand Yrespectively, in this case 12.
#This implies that the difference Y¯−X¯has mean 0. 
#We described that the standard deviation of this statistic (the standard error) is SE(X¯−Y¯)=μ2Y12+μ2X12‾‾‾‾‾‾‾‾√and that we estimate the population standard deviations σXand σY with the sample estimates. 
#What is the estimate of SE(X¯−Y¯)=μ2Y12+μ2X12‾‾‾‾‾‾‾‾√?
sqrt( sd(X)^2/12 + sd(Y)^2/12 )

#compute Y¯−X¯ as well as an estimate of this standard error and construct a t-statistic. What number is this t-statistic?
( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)
#review the t-distribution that was discussed in the lecture
#t-distribution is centered at 0 and has one parameter: the degrees of freedom, that control the size of the tails
#if X follows a t-distribution the probability that X is smaller than an extreme value such as 3 SDs away from the mean grows with the degrees of freedom
1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pnorm(3)
#apply the CLT, what is the distribution of this t-statistic? :Normal with mean 0 and standard deviation 1
#What is the probability of observing a quantity as large as what we computed in 9, when the null distribution is true?
Z <- ( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)
2*( 1-pnorm(Z))
#What is the p-value under the t-distribution approximation?
t.test(Y,X)
t.test(Y,X)$p.value
#Q:With the CLT distribution, we obtained a p-value smaller than 0.05 and with the t-distribution, one that is larger. They can’t both be right. What best describes the difference? A:These are two different assumptions. The t-distribution accounts for the variability introduced by the estimation of the standard error and thus, under the null, large values are more probable under the null distribution.
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )
library(dplyr)
set.seed(1) #Set the seed at 1, then using a for-loop take a random sample of 50 mice 1,000 times
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){#What proportion of these 1,000 averages are more than 1 gram away from the average of x ?
  X <- sample(x,50)
  averages5[i] <- mean(X)
}
hist(averages5)
mean( abs( averages5 - mean(x) ) > 1)
#What is the proportion of countries in 1952 that have a life expectancy between 40 and 60 years?
library(gapminder)
data(gapminder)
head(gapminder)
library(dplyr)
x<-filter(gapminder,year==1952) %>% select(lifeExp) %>% unlist
head(x)
mean(x<=60) - mean(x<=40)
#For the females, our sample estimates were closer to the population difference than with males. What is a possible explanation for this?
popsd(x)
popsd(y)
#The population variance of the females is smaller than that of the males; thus, the sample variable has less variability.
mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
#What is the best explanation for all these mouse weights being well approximated by the normal distribution?

