#central limit theorem 
##sample average follows (~) follows normal distribution with mean population average, so it's centered at the population average and standard deviation of sigmaX divide by the square root of the sample size
#sigmaX = the population standard deviation = teh average distance to the population mean = what is the distance of a typical individual from the mean 
#standard deviation = idea of how much the population varies from around the mean 
##CLT 
##sample average is a random variable -> take a sample, receive different variable 
##variables are getting distributed with increasing the number of taken samples
#sample of size 10 vs sample size 50  = spread of histogram is smaller when the sample size is bigger 
#interest on difference between sample averages (Y-X) = random variable = how different could it be?
#CLT says -> what variance of differences is , not what is the difference =>
## when 0 = Null hypothesis is TRUE => problem is that  we don't know population standard deviation => hence, the question is whether a value different from 0 is due to chance, or no 
#exercises relate to the normal distribution as an approximation of the distribution of a fixed list of numbers or a population
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )
pnorm(1)-pnorm(-1)#If a list of numbers has a distribution that is well approximated by the normal distribution, what proportion of these numbers are within one standard deviation away from the list's average?
pnorm(2)-pnorm(-2)#What proportion of these numbers are within two standard deviations away from the list's average?
pnorm(3)-pnorm(-3)
#
library(dplyr)
head(dat)
x<- filter(dat,Sex =="M" & Diet == "chow") #Use dplyr to create a vector x with the body weight of all males on the control (chow) diet
x <-as.vector(x$Bodyweight,mode = "any")
popMean <-mean(x)# What is this population's average?
popSD<- popsd(x)#the rafalib package and use the popsd() function to compute the population standard deviation
propWithinOneSD<- (x-popMean)/popSD
mean(abs(propWithinOneSD) <=1) #What proportion of the mice are within one standard deviation away from the average weight?
mean(abs(propWithinOneSD) <=2) #What proportion of these numbers are within two standard deviations away from the list's average?
mean(abs(propWithinOneSD) <=3) #What proportion of these numbers are within three standard deviations away from the list's average?
#Note that the numbers for the normal distribution and our weights are relatively close. Also, notice that we are indirectly comparing quantiles of the normal distribution to quantiles of the mouse weight distribution. We can actually compare all quantiles using a qqplot.
library(rafalib)
qqnorm(x)#The mouse weights are well approximated by the normal distribution, although the larger values (right tail) are larger than predicted by the normal. This is consistent with the differences seen between question 3 and 6.
abline(0,1)
#probability
#We will now take a sample of size 25 from the population of males on the chow diet. The average of this sample is our random variable. We will use the replicate() function to observe 10,000 realizations of this random variable. Set the seed at 1, then generate these 10,000 averages. Make a histogram and qq-plot of these 10,000 numbers against the normal distribution.
library(dplyr)
head(dat)
x<- filter(dat,Sex =="M" & Diet == "chow") #Use dplyr to create a vector x with the body weight of all males on the control (chow) diet
x <-as.vector(x$Bodyweight,mode = "any")
avgs <- replicate(10000, mean( sample(x, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs) #What is the average of the distribution of the sample average?
library(rafalib)
popsd(avgs)#What is the standard deviation of the distribution of sample averages (use popsd())?
#CLT practice
##the NUll distirbution is very well approximated by a normal distribution 
#check whether normal approximation applies here 
library(dplyr)
filename <- "femaleMiceWeights.csv"
dat<- read.csv(filename, header = TRUE,sep = ",")
chowVals <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
class( chowVals )
hfVals <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
class( hfVals )
obs<- mean(hfVals) - mean(chowVals)
#how statistical inference is used to support scientific statements
#[p value]
pop <- read.csv("femaleControlsPopulation.csv")
pop<- unlist(pop)
## Null Distribution = all possible realizations under the null
chowVals <- sample(pop,12)#do it multiple times = see several realizations of the difference in mean  for the null hypothesis
hfVals<- sample(pop,12)
mean(hfVals) - mean(chowVals)
### if knowing the null distribution, one can describe the proportion of values one sees for any interval of values 
####define a number of times to redo the null hypothesis check; record all differences
pop <- read.csv("femaleControlsPopulation.csv")
pop<- unlist(pop)
n<-10000
nulls <- vector("numeric",n)
for(i in 1:n){
  chowVals <- sample(pop,12)
  hfVals<- sample(pop,12)
  nulls[i]<- mean(hfVals) - mean(chowVals)
}
max(nulls)
hist(nulls)
#[null hypothesis]
sum(nulls > obs)/n #1st option: how often null values are bigger or not than observed values 
mean(nulls >obs) #2nd option: proportion of times the null is bigger than the observation 
#[p value] the probability that an outcome from the null distribution is bigger than what one observed when the null hypothesis is true 
mean(abs(nulls) >obs)#3rd option: how often it is bigger in absolute 
library(rafalib)
mypar()
qqnorm(nulls)
qqline(nulls)
#do it with sample 3 -> And when we do it with just 3, we can see that the normal approximation becomes slightly worse
#use normal approximation instead of accessing the population through changes of the sample 

