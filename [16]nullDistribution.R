library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv" 
download(url, destfile=filename)
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
#
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )#x represents the weights for the entire population.
#[null distribution] 
head(x)
set.seed(1)
n<-1000
averages <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)#using a for-loop take a random sample of 5 mice 1,000 times ; Save these averages
  averages[i]<- mean(X)
}
hist(averages)
mean( abs( averages - mean(x) ) > 1)#What proportion of these 1,000 averages are more than 1 gram away from the average of x ?
#[null distribution] 
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )#x represents the weights for the entire population.
#[null distribution] 
head(x)
set.seed(1)
n<-10000
averages <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages[i]<-mean(X)
}
hist(averages)
mean(abs(averages - mean(x))>1)#What proportion of these 10,000 averages are more than 1 gram away from the average of x ?
#distribution and normal approximation 
##describe distribution = describe the entier list => empirical cumulative distribution function 
install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)
#probability distribution; mpirical cumulative distribution function[cdf] = function F(a) for any a = tells  the proportion of the values which are less than or equal to a
#1st way 
mean(x <= a)# calculates the number of values in x which are less than or equal to a, divided by the total number of values in x, in other words the proportion of values less than or equal to a
#2nd way 
ecdf()
#1st
library(dplyr)
x <- filter(gapminder, year ==1952)%>% select(lifeExp) %>% unlist
head(x)
mean(x <= 40)
#[sapply]
##plot the proportions of countries with life expectancy q for a range of different years
#1st option
prop = function(q) {
  mean(x <= q)
}
  qs<- seq(from = min(x), to =max(x), length=20)
  props = sapply(qs,prop)
  plot(qs, props)
  props = sapply(qs, function(q) mean(x <= q))
#2nd option
  plot(ecdf(x))


