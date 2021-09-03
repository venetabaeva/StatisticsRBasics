-----------------------------------------
#DFs components
  
library(dplyr)
library(dslabs)
library(tidyverse)
library(downloader) 
library(UsingR)
library(rafalib)
------
dfTest <- data.frame(names=c("A","B","C","D"),
                     num1 = c(1,2,3,4),
                     num2 = c(10,20,30,40),
                     stringsAsFactors = FALSE)
dfTest
data(heights)
------
dfPonzo <- read.csv(
  file = ("/Users/venetabaeva/git/repository4/PonzoEyeTrack.csv"),
  header = TRUE,
  sep = ";",
  dec = ".")
View(dfPonzo)
------
dfAlc <- read.csv(
                      file = ("/Users/venetabaeva/git/repository4/gapminder.csv"),
                      header = TRUE,
                      sep = ";",
                      dec = ".")
View(dfAlc)
------
str(dfAlc)
------
tableAlcCountry <- c(dfAlc$abbrv)
table(tableAlcCountry)
country<- c("Afghanistan","Albania","Algeria","Andorra","Angola", "Antigua and Barbuda")
abbrv <- c("AF", "AL", "DZ", "AD", "AO", "AG")
------
x<-c(1,"test",3)
x
class(x)
x<-c("1","test","3")
x
class(x)
as.numeric(x)
x <- c(60,50 ,40, 30, 20)
y<- as.character(x) 
y
as.numeric(y)
x<-y
sort(x)   
index <- order(x)
index
x[index]
order(x)
rank(x)
aconsum <- c(0, 7, 1, 10, 6, 8)
abbrvCountry <- c("AF"="Afghanistan", "AL" = "Albania", "DZ" ="Algeria", "AD" = "Andorra", "AO" = "Angola", "AG"= "Antigua and Barbuda")
abbrvCountry[2] 
abbrvCountry[c(1,100)]
abbrvCountry[1:3]
abbrvCountry["AF"]
abbrvCountry[c("AL","DZ")]
dfAbbrvAconsum <- data.frame(country = abbrv, alchoholconsumption = aconsum)
seq(1,10)
1:10
class(3L)
class(dfAlc)
class(dfAlc$aconsum)
levels(dfAlc$aconsum)
nlevels(dfAlc$aconsum)
head(dfAlc)
names(dfAlc) 
colnames(dfPonzo) 
colnames(dfPonzo)[colnames(dfPonzo) == 'X.Gaze.x...'] <- 'xGaze'
colnames(dfPonzo)[colnames(dfPonzo) == 'X.Gaze.y...'] <- 'yGaze'
colnames(dfPonzo)[colnames(dfPonzo) == 'X..RespondentNr.'] <- 'respondentNr'
colnames(dfPonzo)[colnames(dfPonzo) == 'X.StimulusNr.'] <- 'stimulusNr'
colnames(dfPonzo)[colnames(dfPonzo) == 'X.timestamp.ms.'] <- 'timeStampsMs'
colnames(dfPonzo) 
names(abbrvCountry)<-abbrv 
summary(dfAlc)
sort(dfAlc$aconsum)
dfPonzo[12,3]
dfAlc$country[11]
-----------------------------------------
# DFs
  
dfAlc["aconsum"]
dfAlc[["aconsum"]]
dfAlc$aconsum
aconsum <- dfAlc$aconsum
naS <- is.na(aconsum)
sum(naS)
mean(aconsum[!naS])
mean(dfAlc[,2])
urbanrt <-dfAlc$urbanrt
employrt <- dfAlc$employrt
length(aconsum)
lengthAconsum <- length(dfAlc$aconsum)
lengthAconsum
length(seq(1,10)) 
seq(1, 10, length.out = 100)
identical(urbanrt,employrt)
iOrdDfAconsum<-order(dfAlc$aconsum)
dfAlc$abbrv[iOrdDfAconsum]
iMax<-which.max(dfAlc$aconsum)
iMax
dfAlc$aconsum[iMax]
dfAlc$abbrv[iMax]
dfAlc$abbrv[which.max(dfAlc$aconsum)]
iMin<-which.max(dfAlc$aconsum)
iMin
dfAlc$abbrv[iMin]
dfAlc$abbrv[which.min(dfAlc$aconsum)]
dfAlc$abbrv[order(dfAlc$employrt,decreasing=TRUE)]
i <- dfAlc$aconsum  < 5
i
dfAlc$abbrv[i]
sum(i,na.rm =TRUE)
chowVals <- filter(dat, Diet=="chow") %>% dplyr::select(Bodyweight) %>% unlist
class( chowVals )  
hfVals <- filter(dat, Diet=="hf") %>%dplyr:: select(Bodyweight) %>% unlist
class( hfVals ) 
mean(chowVals)
mean(hfVals)
emea<- dfAlc$region == "EMEA" 
aconsum <- dfAlc$aconsum <= 5 
i <- aconsum&emea
filter(dfAlc,region  == "EMEA" )
which(i)
dfAlc$abbrv[i]
i<-which(dfAlc$abbrv == "BG")
i
aconsum[i]
i <- match(c("BG","IT","ES"),dfAlc$abbrv)
i
dfAlc$abbrv[i]
aconsum[i]
emea<- dfAlc$region == "EMEA" 
aconsum5 <- dfAlc$aconsum <= 5 
aconsum10 <- dfAlc$aconsum <= 5 
aconsum5&emea %in% aconsum10&emea
checkAbbrv<- c("GB","BG","MZ") %in% dfAlc$abbrv
i <- which(!checkAbbrv%in%dfAlc$abbrv)
i
checkAbbrv[i] 
dfAlc <- mutate(dfAlc,rank=rank(-dfAlc$aconsum))
dfNoEMEA <- data.frame(filter(dfAlc,region!="EMEA"))
nrow(dfNoEMEA)
dfEMEAAPAC <- data.frame(filter(dfAlc,region %in% c("EMEA","APAC")))
nrow(dfEMEAAPAC)
EMEAAPACAconsum10<- filter(dfAlc,region %in% c("EMEA","APAC") & aconsum < 10)
dplyr::select(EMEAAPACAconsum10,country,aconsum,rank)
newTable <- dplyr::select(dfAlc,country,region,aconsum) 
filter(newTable,aconsum <= 10)
str(newTable)
dfAlc %>% dplyr::select(country,region,aconsum) %>% filter(aconsum <= 10)
filter(dfAlc, region %in% c("EMEA", "APAC") & aconsum < 10 )%>% dplyr::select(country, aconsum, rank)
dfAlc %>% mutate(aconsum, rank) %>% filter(region %in% c('EMEA','APAC') & aconsum <10) %>% dplyr::select(country,aconsum,rank)
------
ind <- heights$height > mean(heights$height)#How many individuals in the dataset are above average height?
sum(ind)
ind <- heights$height > mean(heights$height) & (heights$sex =="Female")#How many individuals in the dataset are above average height and are female?
sum(ind)
mean(heights$sex == "Female")# proportion of individuals in the dataset are female
minH<- min(heights$height) 
ind <- match(minH, heights$height)#Use the match() function to determine the index of the first individual with the minimum height.
heights$sex[ind]#Subset the sex column of the dataset by the index in 4b to determine the individual’s sex.
maxH <- max(heights$height)#Write code to create a vector x that includes the integers between the minimum and maximum heights (as numbers).
minH <- min(heights$height)
intgr <- c(minH:maxH)
intgr
sum(!(intgr %in% heights$height))#How many of the integers in x are NOT heights in the dataset?
heights <- mutate(heights, ht_cm = height*2.54)#create a new column of heights in centimeters named ht_cm
head(heights)
heights$ht_cm[18]
mean(heights$ht_cm) 
females <- filter(heights, sex == "Female") #females by filtering the heights2 data to contain only female individuals.
head(females)
nrow(females)#How many females are in the heights2 dataset?
mean(females$ht_cm)#What is the mean height of the females in centimeters?
------
i <- which.min(dfAlc$aconsum)
if (dfAlc$aconsum[i] < 10){
  print(dfAlc$country[i])
}else{
  print("No coutnry has alcochol rate that low")
}
------
minAlc <- which.min(dfAlc$aconsum)
ifelse(minAlc,dfAlc$country,NA) 
------
sum(is.na(dfAlc))
dfAlc[is.na(dfAlc)] = 0
View(dfAlc)
------
if(all(dfAlc$aconsum < 30)){
  print("World Alcochol rate is under 30")
} else{
  print("World Alcochol rate is not under 30")
}
------
str(dfAlc)
x <- dfAlc$country
xCharCountry <- nchar(x)
y <- dfAlc$abbrv
changeCountryToAbbrv<- ifelse(xCharCountry >2,y,x)
changeCountryToAbbrv
------
avg <- function(x){
  s <- sum(x)
  n <- length(x)# Note:  not saved in the workspace; the values of the variables are changed only during the F's call
  s/n
} 
x<- dfAlc$urbanrt
identical (mean(x),avg(x))
------
avg <- function(x,arithmetic=TRUE){ # calculate either geometric, or arithmetic avrg dependign on uder-defined variable
    n <- length(x)
    arithmetic <-sum(x)/n
    geometric<- prod(x)^(1/n)
    ifelse(arithmetic,arithmetic,geometric)
  }
x<- dfAlc$urbanrt
avg(x)
------
  
compute_s_n <- function(n){
    x<- 1:n
    sum(x) 
  }
compute_s_n(3)
m <-25 
s_n <- vector(length = m)#create an empty vector for storing
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}
n <- 1:m
plot(n,s_n)
lines(n,n*(n+1)/2)

-----------------------------------------
#Sampling
  
------
controlsXGaze <- filter(dfPonzo, stimulusNr == c(1,3,13,15,25,27)) %>% dplyr::select(xGaze) %>% summarise(mean(xGaze))
controlsXGaze
controlsYGaze <-filter(dfPonzo, stimulusNr == c(1,3,13,15,25,27)) %>% dplyr::select(yGaze) %>% summarise(mean(yGaze))
controlsYGaze
------ 
controlsXGaze <- filter(dfPonzo, stimulusNr == c(1,3,13,15,25,27)) %>% dplyr::select(xGaze) 
mean(controlsXGaze$xGaze)
controlsYGaze <-filter(dfPonzo, stimulusNr == c(1,3,13,15,25,27)) %>% dplyr::select(yGaze)
mean(controlsYGaze$yGaze)
longUpXGaze  <-filter(dfPonzo, stimulusNr == c(5,7,17,19,29,31))%>% dplyr::select(xGaze)
mean(longUpXGaze$xGaze)
longUpYGaze  <-filter(dfPonzo, stimulusNr == c(5,7,17,19,29,31))%>% dplyr::select(yGaze)
mean(longUpYGaze$yGaze)
longDownXGaze<- filter(dfPonzo, stimulusNr == c(9,11,21,23,33,35))%>% dplyr::select(xGaze)
mean(longDownXGaze$xGaze)
longDownYGaze<- filter(dfPonzo, stimulusNr == c(9,11,21,23,33,35))%>% dplyr::select(yGaze)
mean(longDownYGaze$yGaze)
------
controls <- filter(dfPonzo, stimulusNr == c(1,3,13,15,25,27)) 
head(controls)
View(controls)
mean(controls$xGaze)
mean(controls$yGaze)
longUp  <-filter(dfPonzo, stimulusNr == c(5,7,17,19,29,31))
head(longUp)
View(longUp)
mean(longUp$xGaze)
mean(longUp$yGaze)
longDown <- filter(dfPonzo, stimulusNr == c(9,11,21,23,33,35))
head(longDown)
View(longDown)
mean(longDown$xGaze)
mean(longDown$yGaze)
------ 
unlist(controls)
unlist(longUp)
unlist(longDown)
------
plot(controls$xGaze,controls$yGaze,xlab = "controlsXGaze", ylab="controlsYGaze") 
plot(longUp$xGaze,longUp$yGaze,xlab = "longUpXGaze", ylab="longUpYGaze") 
plot(longDown$xGaze,longDown$yGaze,xlab = "longDownXGaze", ylab="longDownYGaze") 
------
popDfPonzoXGaze <- unlist(dfPonzo$xGaze)
popDfPonzoYGaze <- unlist(dfPonzo$yGaze)
mean(sample(popDfPonzoXGaze,12)) #random sample; get different random sample of 12 ;random variable of random sample
mean(sample(popDfPonzoYGaze,12))
------
mean(popDfPonzoXGaze)
mean(popDfPonzoYGaze)
------ 
set.seed(1) #produces the same sample again and again = generate same set  at each time
------ 
sampleDfPonzoXGaze<- sample(popDfPonzoXGaze,5)# if I use the sample() function immediately after setting a seed, I will always get the same sample.
abs(mean(sampleDfPonzoXGaze)-mean(popDfPonzoXGaze)) #absolute value  of the difference between the average of the sample and the average of all the values?
sampleDfPonzoYGaze<- sample(popDfPonzoYGaze,5)# if I use the sample() function immediately after setting a seed, I will always get the same sample.
abs(mean(sampleDfPonzoYGaze)-mean(popDfPonzoYGaze))#absolute value  of the difference between the average of the sample and the average of all the values?
------ 
set.seed(5) 
------ 
sampleDfPonzoXGaze<- sample(popDfPonzoXGaze,5)# if I use the sample() function immediately after setting a seed, I will always get the same sample.
abs(mean(sampleDfPonzoXGaze)-mean(popDfPonzoXGaze)) #absolute value  of the difference between the average of the sample and the average of all the values?
sampleDfPonzoYGaze<- sample(popDfPonzoYGaze,5)# if I use the sample() function immediately after setting a seed, I will always get the same sample.
abs(mean(sampleDfPonzoYGaze)-mean(popDfPonzoYGaze))#absolute value  of the difference between the average of the sample and the average of all the values?
------ 
obslongUpControlsX <- mean(longUp$xGaze) - mean(controls$xGaze)
obslongUpControlsY <- mean(longUp$yGaze) - mean(controls$yGaze)
obslongDownControls <- mean(longDown$xGaze) - mean(controls$xGaze)
obslongDownControls <- mean(longDown$yGaze) - mean(controls$yGaze)
popDfPonzoXGaze <- unlist(dfPonzo$xGaze)
popDfPonzoXGaze <- unlist(dfPonzo$yGaze)
# Null Distribution = all possible realizations under the null 
# do it multiple times = see several realizations of the difference in mean  for the null hypothesis
popDfPonzoControlsX <- sample(popDfPonzoXGaze,12)
popDfPonzoLongUpX <- sample(popDfPonzoXGaze,12)
mean(popDfPonzoLongUpX) - mean(popDfPonzoControlsX)
popDfPonzoControlsY <- sample(popDfPonzoYGaze,12)
popDfPonzoLongUpY <- sample(popDfPonzoYGaze,12)
mean(popDfPonzoLongUpY) - mean(popDfPonzoControlsY)
------ 
# if knowing the null distribution, one can describe the proportion of values one sees for any interval of values 
## define a number of times to redo the null hypothesis check; record all differences  
n<-10000
nullsPopX <- vector("numeric",n)
for(i in 1:n){
  popDfPonzoControlsX <- sample(popDfPonzoXGaze,12)
  popDfPonzoLongUpX <- sample(popDfPonzoXGaze,12)
  nullsPopX[i]<- mean(popDfPonzoLongUpX) - mean(popDfPonzoControlsX)
}
max(nullsPopX)
hist(nullsPopX)
------  
n<-10000
nullsPopY <- vector("numeric",n)
for(i in 1:n){
  popDfPonzoControlsY <- sample(popDfPonzoYGaze,12)
  popDfPonzoLongUpY <- sample(popDfPonzoYGaze,12)
  nullsPopY[i]<- mean(popDfPonzoLongUpY) - mean(popDfPonzoControlsY)
}
max(nullsPopY)
hist(nullsPopY)
------   
#[null hypothesis]
sum(nulls > obs)/n #1st option: how often null values are bigger or not than observed values 
mean(nulls >obs) #2nd option: proportion of times the null is bigger than the observation 
#[p value] the probability that an outcome from the null distribution is bigger than what one observed when the null hypothesis is true 
mean(abs(nulls) >obs)#3rd option: how often it is bigger in absolute 
------    
#[null distribution] 
head(popDfPonzoXGaze)
set.seed(1)
n<-1000
averagesPopX <- vector("numeric",n)
for(i in 1:n){
  X <- sample(popDfPonzoXGaze,5)#using a for-loop take a random sample of 5 mice 1,000 times ; Save these averages
  averagesPopX[i]<- mean(X)
}
hist(averagesPopX)
mean( abs( averagesPopX - mean(popDfPonzoXGaze) ) > 0.05)#What proportion of these 1,000 averages are more than 1 gram away from the average of x ?
#[null distribution] 
head(popDfPonzoYGaze)
set.seed(1)
n<-1000
averagesPopY <- vector("numeric",n)
for(i in 1:n){
  Y <- sample(popDfPonzoYGaze,5)#using a for-loop take a random sample of 5 mice 1,000 times ; Save these averages
  averagesPopY[i]<- mean(Y)
}
hist(averagesPopY)
mean( abs( averagesPopY - mean(popDfPonzoYGaze) ) > 0.05)#What proportion of these 1,000 averages are more than 1 gram away from the average of x ?
------   
#distribution and normal approximation 
##describe distribution = describe the entier list => empirical cumulative distribution function 
###1st
head(popDfPonzoXGaze)
mean(popDfPonzoXGaze <= 0.500)
prop = function(q) {
  mean(popDfPonzoXGaze <= q)
}
qs<- seq(from = min(popDfPonzoXGaze), to = max(popDfPonzoXGaze), length=20)
props = sapply(qs,prop)
plot(qs, props)
props = sapply(qs, function(q) mean(popDfPonzoXGaze <= q))
###2nd
plot(ecdf(popDfPonzoXGaze))
------   
## proportion of numbers between a and b 
## mu (average) 
## sigma (standard deviation)
# if the distribution of ones data is approximated by a normal distribution, then one knows, for ones data, what proportion of the data is in the interval 
# [zscore] standartized units = how many standard deviations from the mean? = substract the mean and divide by standard deviation for each point in the distribution 
# Central Limit Theorem
head(popDfPonzoXGaze)
set.seed(1)
n<-1000
averagesPopX5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(popDfPonzoXGaze,5)#using a for-loop take a random sample of 5 mice 1,000 times ; Save these averages
  averagesPopX5[i]<- mean(X)
}
------   
head(popDfPonzoXGaze)
set.seed(1)
n<-1000
averagesPopX50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(popDfPonzoXGaze,50)#using a for-loop take a random sample of 5 mice 1,000 times ; Save these averages
  averagesPopX50[i]<- mean(X)
}
par(mfrow = c(2,1))
hist(averagesPopX5)
hist(averagesPopX50)
#For the last set of averages, the ones obtained from a sample size of 50, what proportion are between 0.500 and 0.500?
mean(averagesPopX50<0.600 & averagesPopX50>0.400)
------ 
#[pnorm] find the proportion of observations below a cutoff x given a normal distribution with mean mu and standard deviation sigma with pnorm(x, mu, sigma) or pnorm( (x-mu)/sigma )
##What is the proportion of observations between 0.600 and 0.400 in a normal distribution with mu and sd?
mu <- mean(averagesPopX50)
mu
sigma <- popsd(averagesPopX50)
sigma
pnorm(0.600, mu, sigma) - pnorm(0.400,mu,sigma)  
------ 
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
# the normal distribution as an approximation of the distribution of a fixed list of numbers or a population 

popMean <- mean(popDfPonzoXGaze)# What is this population's average?
popSD <- popsd(popDfPonzoXGaze)#the rafalib package and use the popsd() function to compute the population standard deviation
propWithinOneSD<- (popDfPonzoXGaze-popMean)/popSD
mean(abs(propWithinOneSD) <=1) #What proportion of the mice are within one standard deviation away from the average weight?
mean(abs(propWithinOneSD) <=2) #What proportion of these numbers are within two standard deviations away from the list's average?
mean(abs(propWithinOneSD) <=3) #What proportion of these numbers are within three standard deviations away from the list's average?
#Note that the numbers for the normal distribution and our weights are relatively close. Also, notice that we are indirectly comparing quantiles of the normal distribution to quantiles of the mouse weight distribution. We can actually compare all quantiles using a qqplot.
library(rafalib)
qqnorm(popDfPonzoXGaze)#The mouse weights are well approximated by the normal distribution, although the larger values (right tail) are larger than predicted by the normal. This is consistent with the differences seen between question 3 and 6.
abline(0,1)
#probability
#We will now take a sample of size 25 from the population of males on the chow diet. The average of this sample is our random variable. We will use the replicate() function to observe 10,000 realizations of this random variable. Set the seed at 1, then generate these 10,000 averages. Make a histogram and qq-plot of these 10,000 numbers against the normal distribution.
library(dplyr)
avgs <- replicate(10000, mean( sample(popDfPonzoXGaze, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs) #What is the average of the distribution of the sample average?
library(rafalib)
popsd(avgs)#What is the standard deviation of the distribution of sample averages (use popsd())?
------ 
#CLT practice
##the NUll distirbution is very well approximated by a normal distribution 
#check whether normal approximation applies here 
library(dplyr)
obslongUpControlsX <- mean(longUp$xGaze) - mean(controls$xGaze)
obslongUpControlsY <- mean(longUp$yGaze) - mean(controls$yGaze)
#how statistical inference is used to support scientific statements
#[p value]
popDfPonzoXGaze <- unlist(dfPonzo$xGaze)
popDfPonzoYGaze <- unlist(dfPonzo$yGaze)
## Null Distribution = all possible realizations under the null
popDfPonzoControlsX <- sample(popDfPonzoXGaze,12)
popDfPonzoLongUpX <- sample(popDfPonzoXGaze,12)
mean(popDfPonzoLongUpX) - mean(popDfPonzoControlsX) #random sample; get different random sample of 12 ;random variable of random sample
popDfPonzoControlsY <- sample(popDfPonzoYGaze,12)
popDfPonzoLongUpY <- sample(popDfPonzoYGaze,12)
mean(popDfPonzoLongUpY) - mean(popDfPonzoControlsY)
### if knowing the null distribution, one can describe the proportion of values one sees for any interval of values 
####define a number of times to redo the null hypothesis check; record all differences
n<-10000
nullsPopX <- vector("numeric",n)
for(i in 1:n){
  popDfPonzoControlsX <- sample(popDfPonzoXGaze,12)
  popDfPonzoLongUpX <- sample(popDfPonzoXGaze,12)
  nullsPopX[i]<- mean(popDfPonzoLongUpX) - mean(popDfPonzoControlsX)
}
max(nullsPopX)
hist(nullsPopX)
n<-10000
nullsPopY <- vector("numeric",n)
for(i in 1:n){
  popDfPonzoControlsY <- sample(popDfPonzoYGaze,12)
  popDfPonzoLongUpY <- sample(popDfPonzoYGaze,12)
  nullsPopY[i]<- mean(popDfPonzoLongUpY) - mean(popDfPonzoControlsY)
}
max(nullsPopY)
hist(nullsPopY)
#[null hypothesis]
sum(nullsPopX > obslongUpControlsX)/n #1st option: how often null values are bigger or not than observed values 
mean(nullsPopX >obslongUpControlsX) #2nd option: proportion of times the null is bigger than the observation 
sum(nullsPopY > obslongUpControlsY)/n 
mean(nullsPopY >obslongUpControlsY)
#[p value] the probability that an outcome from the null distribution is bigger than what one observed when the null hypothesis is true 
mean(abs(nullsPopX) >obslongUpControlsX)#3rd option: how often it is bigger in absolute 
mean(abs(nullsPopY) >obslongUpControlsY)
library(rafalib)
mypar()
qqnorm(nullsPopX)
qqline(nullsPopX)
qqnorm(nullsPopY)
qqline(nullsPopY)
#do it with sample 3 -> And when we do it with just 3, we can see that the normal approximation becomes slightly worse
#use normal approximation instead of accessing the population through changes of the sample 
------ 
#CLT in conjunction with t-Tests to obtain p - value and obtain confidence values 
#construct t stat = form a t-statistic = dividing the observed difference by its estimated standard deviation
library(dplyr)
N <- length(controlsXGaze)
se <- sqrt(var(longUpXGaze)/N+ #square root of the variance of the sample estimate of the variance divided by N=sample size
               var(controlsXGaze)/N) #gives us standard error estimate
tstatX <- obslongUpControlsX/se
tstatX
library(dplyr)
N <- length(controlsYGaze)
se <- sqrt(var(longUpYGaze)/N+ 
             var(controlsYGaze)/N) 
tstatY <- obslongUpControlsY/se
tstatY
# assume that,  that the normal approximation holds for the null distribution =  don't need to have access to the population data
# central limit theorem  tells that  the null distribution is approximated by a normal distribution with mean 0 and variance 1
1- pnorm(tstatX) #what proportion of normally distributed data, with means 0 and standard deviation 1,are lower than whatever value you put here
2*(1- pnorm(tstatX))
1- pnorm(tstatY) 
2*(1- pnorm(tstatY))
# how good of an approximation this is => access the population 
n<-10000
nullsPopX <- vector("numeric",n)
for(i in 1:n){
  popDfPonzoControlsX <- sample(popDfPonzoXGaze,12)
  popDfPonzoLongUpX <- sample(popDfPonzoXGaze,12)
  nullsPopX[i]<- mean(popDfPonzoLongUpX) - mean(popDfPonzoControlsX)
}

n<-10000
nullsPopY <- vector("numeric",n)
for(i in 1:n){
  popDfPonzoControlsY <- sample(popDfPonzoYGaze,12)
  popDfPonzoLongUpY <- sample(popDfPonzoYGaze,12)
  nullsPopY[i]<- mean(popDfPonzoLongUpY) - mean(popDfPonzoControlsY)
}
# Q-Q plot against the normal for these nulls, it should be right on the line
# Q-Q plot should have slope 1 and intercept 0
# check through doing abline 0, 1 = a line with intercept 0 and slope 1 => go right through the data
library(rafalib)
mypar()
qqnorm(nullsPopX)
abline(0,1)
qqline(nullsPopX)
qqnorm(nullsPopY)
abline(0,1)
qqline(nullsPopY)
#t-test practice 
library(dplyr)
tTestX<-  t.test(longUpXGaze$xGaze,controlsXGaze$xGaze)
tTestX
tTestX$p.value#what is the p-value under the t-distribution approximation?
tTestY<-  t.test(longUpYGaze$yGaze,controlsYGaze$yGaze)
tTestY 
tTestY$p.value
# an assumption we're making when we use that distribution, the original data is normally distributed, meaning that if we had access to the entire population
library(rafalib)
qqnorm(controlsXGaze$xGaze) # now we can test that somewhat by making q-q plots of the sample
qqline(controlsXGaze$xGaze) 
qqnorm(longUpXGaze$xGaze) # now we can test that somewhat by making q-q plots of the sample
qqline(longUpXGaze$xGaze)
library(rafalib)
qqnorm(controlsYGaze$yGaze) # now we can test that somewhat by making q-q plots of the sample
qqline(controlsYGaze$yGaze)
qqnorm(longUpYGaze$yGaze) # now we can test that somewhat by making q-q plots of the sample
qqline(longUpYGaze$yGaze)

------ 
#DFs visualization 
------
abbCountry <- dfAlc$abbrv 
suicidesPer100 <- dfAlc$suicideper100
urbanRT <- dfAlc$urbanrt
region <- dfAlc$region
ranksAConsumption <- rank(dfAlc$aconsum,na.last = NA)
i <- order(dfAlc$aconsum)
df<- data.frame(country = abbCountry[i], suicide = suicidesPer100[i], rank = ranksAConsumption[i],urbanrate = urbanRT[i],region = region[i])

View(df)
df %>%
  ggplot(aes(urbanrate, suicide, label=country,color=rank)) + geom_label()
------
  dfAlc %>%
  ggplot(aes(urbanrt, employrt, label=abbrv, color=region)) + geom_label()
------
  dfAlc %>%
  ggplot(aes(urbanrt, employrt, label=abbrv, color=aconsum)) + geom_label()
------
plot(dfAlc$suicideper100,dfAlc$aconsum, xlab = "suicides/100 people", ylab="alcohol consumption")
plot(dfAlc$employrt,dfAlc$aconsum, xlab = "employee rate", ylab="alcohol consumption")
plot(dfAlc$urbanrt,dfAlc$aconsum, xlab = "urban rate", ylab="alcohol consumption")
------
log10IncomePer1 <- log10(dfAlc$incomeper1)
log10Aconsum <- log10(dfAlc$aconsum)
plot(log10IncomePer1,log10Aconsum)
------
hist(dfAlc$aconsum,xlab = "alcohol consumption") 
dfAlc$country[which.max(dfAlc$aconsum)]
------
boxplot(aconsum~region, data=dfAlc,na.action = NULL) 
boxplot(suicidesPer100~region, data = dfAlc)
boxplot(employrt~region, data = dfAlc)
boxplot(urbanrt~region, data = dfAlc)
 
#III

------
unique(dfAlc$employrt) #discrete numeric data can be considered ordinal -> a small number of different groups, with each group having many members
length(unique(dfAlc$employrt))
------
table(dfAlc$employrt) # #compute the frequencies # of each unique value 
prop.table(table(dfAlc$region))# proportion of each unique value = frequency table = distribution 
sum(table(dfAlc$employrt)==1)
------
library(dplyr)
dfAlc[is.na(x = dfAlc)] <- 0
rangeForCdfFunction <- seq(min(dfAlc$employrt), max(dfAlc$employrt),length = 100) #define range of values spaning the dataset
cdfFunction <- function(f){ # computes probability for a single values 
  mean(dfAlc$employrt<=f)  # cdfFunction (rangeForCdfFunction) = Pr (f</=rangeForCdfFunction) 
}
#CDF defines proportion of data below a cutoff - rangeForCdfFunction
cdfValuesBelow <- sapply(rangeForCdfFunction,cdfFunction) #  CDF defines proportion of data below the  cutoff rangeForCdfFunction
plot(rangeForCdfFunction,cdfValuesBelow)
# defines proportion of data above the  cutoff rangeForCdfFunction => 1 - cdfFunction (rangeForCdfFunction)
cdfValuesAbove <- 1-(sapply(rangeForCdfFunction,cdfFunction))
plot(rangeForCdfFunction,cdfValuesAbove) 
# defines proportion of data between the  cutoff rangeForCdfFunction => 1 - cdfFunction (rangeForCdfFunction)
rangeForCdfFunctionQ <- seq(quantile(dfAlc$employrt,0.50), quantile(dfAlc$employrt,0.75),length = 100) #define range of values spaning the dataset
cdfFunction <- function(f){ # computes probability for a single values 
  mean(dfAlc$employrt<=f)  # cdfFunction (rangeForCdfFunction) = Pr (f<=rangeForCdfFunction) 
}
#CDF defines proportion of data below a cutoff - rangeForCdfFunction
cdfValuesBellowQ <- sapply(rangeForCdfFunctionQ,cdfFunction) - (sapply(rangeForCdfFunction,cdfFunction))# define proportion of values between rangeForCdfFunction and rangeForCdfFunctionQ
plot(rangeForCdfFunctionQ,cdfValuesBellowQ)
------
# normal distribution = Gaussian distribution = bell curve = probability to have x value between value a and value b using parameters mean and standard deviation
# why using the normal distribution -> rather than using data, the normal distribution is defined with a mathematical formula 
# normal distribution <- 95% are within 2SD from the average 
# => if a dataset is approximated by a normal distribution, then to describe the distribution only the average and the standard deviation are needed 
meanDfAlcEmpRate <- sum(dfAlc$employrt)/ length(dfAlc$employrt)
sdDfAlcEmpRate <- sqrt(sum((dfAlc$employrt-mean)^2)/ length(dfAlc$employrt))
iEMEA <- dfAlc$region == "EMEA"
xEMEAEmplRt <- dfAlc$employrt[iEMEA]
mEMEAEmplRt <- mean(xEMEAEmplRt)
sdEMEAEmplRt <- sd(xEMEAEmplRt)
c(mEMEAEmplRt=mEMEAEmplRt,sdEMEAEmplRt=sdEMEAEmplRt)
------
# standard units -> z = (x-average/sd)
# if data is approximatley normal, then think in standard units
# standard unit of value = how many standard deviations away from the average this values is
# if z=0, the normal distribution is at its maximum, the mean (μ); if z=0, then the function is defined symmetric 
# normal distirbution of z - scores is standard normal distribution μ = 0 and σ  = 1
# z scores near 0 are average; z score > 2, or below -2 are significantly above or below the mean   
zEMEAEmplRt = scale(xEMEAEmplRt) # converts a vector of approximatley normally distributed values into z-scores
mean(abs(xEMEAEmplRt)<2) # compute proportion of observations that are within 2 standard deviations of the mean 
------
# 68-95-99.7 rule = probability of observing events within a certain number of standard deviations of the mean 
# above 68% observations will be within 1sd of the mean = |z|</= 1 (μ +/- 1σ )
# about 95% observations  will be within 2sd of the mean  =  |z|</= 2  (μ +/- 2σ )
# about 99.7% observations will be within 3sd of the mean =  |z|</= 3  (μ +/- 3σ )
------
#The normal distribution has a mathematically defined CDF which can be computed in R with the function pnorm(a, avg, s)
1 - pnorm(45, mean(xEMEAEmplRt),sd(xEMEAEmplRt)) #whether the probability that a randomly selected employRt is bigger than 45
------
plot(prop.table(table(xEMEAEmplRt)), xlab = "a = EMEAEmplRt", ylab = "Pr(xEMEAEmplRt = a)")
------
mean(xEMEAEmplRt>49 & xEMEAEmplRt<=52) 
#no access to data, can you approximate the proportion of the data that is between 69 and 72 ?
pnorm(72, mean(xEMEAEmplRt),sd(xEMEAEmplRt))- pnorm(69,  mean(xEMEAEmplRt),sd(xEMEAEmplRt)) 
------
#the approximation is not always useful; example is for the more extreme values, often called the "tails" of the distribution
exact <- mean(xEMEAEmplRt > 21 & xEMEAEmplRt <= 23) # calculate the proportion of heights between 79 and 81
avg <- mean(xEMEAEmplRt)
sd <- sd(xEMEAEmplRt)
approx <- pnorm(81, avg, sd) - pnorm(79, avg, sd)# estimate the proportion of heights between 79 and 81 
exact/approx # report how many times bigger the actual proportion is compared to the approximation
------
p <- seq(0.01, 0.99, 0.01)
quantile(p) # quantile
percentiles <- quantile(heights$height,p) # percentile
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "50%"]
percentiles[names(percentiles) == "75%"]
------
library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
percentiles_male <- quantile(male, seq(.01, 0.99, 0.01))
percentiles_female<- quantile(female, seq(.01, 0.99, 0.01))
male_percentiles <- c(percentiles_male[names(percentiles_male) == "10%"],percentiles_male[names(percentiles_male) == "30%"],percentiles_male[names(percentiles_male) == "50%"],percentiles_male[names(percentiles_male) == "70%"],percentiles_male[names(percentiles_male) == "90%"])
female_percentiles <- c(percentiles_female[names(percentiles_female) == "10%"],percentiles_female[names(percentiles_female) == "30%"],percentiles_female[names(percentiles_female) == "50%"],percentiles_female[names(percentiles_female) == "70%"],percentiles_female[names(percentiles_female) == "90%"])
df <- data.frame(female = female_percentiles, male = male_percentiles)
------
summary(heights$height)# quartile
qnorm(p, mean(heights$height), sd(heights$height))  # theoretical quantile
pnorm(-1.96)
qnorm(0.025)
mean(heights$height <= 68.5)  # if p = 0.5, then q = 68.5 
------
p <- seq(0.01, 0.99, 0.01)
theoreticalQuantiles <- qnorm(p, mean(heights$height), sd(heights$height)) 
------
library(tidyverse)
library(dslabs)
z <- scale(xEMEAEmplRt)
mean(xEMEAEmplRt <= 54)# proportion of data below 68.5
p <- seq(0.05,0.95,0.05) #Given a proportion p 
observedQuantiles <- quantile(xEMEAEmplRt,p)
theoreticalQuantiles <- qnorm(p, mean = mean(xEMEAEmplRt), sd = sd(xEMEAEmplRt))
plot(theoreticalQuantiles, observedQuantiles) # QQ plot check whether distributions are well-approximated by a normal distribution
abline(0,1)
observedQuantiles <- quantile(z,p)# if standard units are used no need of defining the mean and the standard deviation 
theoreticalQuantiles <- qnorm(p) 
plot(theoreticalQuantiles, observedQuantiles)
abline(0,1)
summary(xEMEAEmplRt)
------
library(dslabs)
iEMEA <- dfAlc$region == "EMEA"
iAPAC <- dfAlc$region == "APAC"
xEMEAEmplRt <- dfAlc$employrt[iEMEA]
xAPACEmplRt <- dfAlc$employrt[iAPAC]
percentilesEMEA <- quantile(xEMEAEmplRt, seq(.01, 0.99, 0.01))
percentilesAPAC <- quantile(xAPACEmplRt, seq(.01, 0.99, 0.01))
EMEApercentiles <- c(percentilesEMEA[names(percentilesEMEA) == "10%"],percentilesEMEA[names(percentilesEMEA) == "30%"],percentilesEMEA[names(percentilesEMEA) == "50%"],percentilesEMEA[names(percentilesEMEA) == "70%"],percentilesEMEA[names(percentilesEMEA) == "90%"])
APACpercentiles <- c(percentilesAPAC[names(percentilesAPAC) == "10%"],percentilesAPAC[names(percentilesAPAC) == "30%"],percentilesAPAC[names(percentilesAPAC) == "50%"],percentilesAPAC[names(percentilesAPAC) == "70%"],percentilesAPAC[names(percentilesAPAC) == "90%"])
theoreticalQuantilesEMEA <- qnorm(seq(.01, 0.99, 0.01), mean = mean(xAPACEmplRt), sd = sd(xAPACEmplRt))
theoreticalQuantilesAPAC <- qnorm(seq(.01, 0.99, 0.01), mean = mean(xAPACEmplRt), sd = sd(xAPACEmplRt))
plot(theoreticalQuantilesEMEA, percentilesEMEA)
abline(0,1)
plot(theoreticalQuantilesAPAC, percentilesAPAC)
abline(0,1)
zEMEA <- scale(xEMEAEmplRt)
zAPAC <- scale(xAPACEmplRt)
percentilesEMEA <- quantile(zEMEA, seq(.01, 0.99, 0.01))
percentilesAPAC <- quantile(zAPAC, seq(.01, 0.99, 0.01))
theoreticalQuantilesEMEA <- qnorm(seq(.01, 0.99, 0.01))
theoreticalQuantilesAPAC <- qnorm(seq(.01, 0.99, 0.01))
plot(theoreticalQuantilesEMEA, percentilesEMEA)
abline(0,1)
plot(theoreticalQuantilesAPAC, percentilesAPAC)
abline(0,1)
------
# when the distribution is not normal -> boxplots 
# range, quartiles and 25th, 50th and 75th percentiles; ignore outliers when computing the range and plot the outliers as independent points
------
  # error check outlier
  error_avg <- function(k){
    dfAlc$employrt[1]<-k
    mean(dfAlc$employrt)
  }

error_avg (10000)
error_avg (-10000)
  
  
  




  