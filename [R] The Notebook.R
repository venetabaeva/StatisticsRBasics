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
dfAlc %>%
  filter(region == "EMEA") %>%
  summarize(median = median(aconsum), #call only  functions that return a single value
            minimum = min(aconsum),
            maximum = max(aconsum))
dfAlcEMEAMedian<- dfAlc %>%
  filter(region == "EMEA") %>%
  summarize(median = median(aconsum), #call only  functions that return a single value
            minimum = min(aconsum),
            maximum = max(aconsum))%>%
  .$median #the dot as a placeholder for the data that is being passed through the pipe
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
sdDfAlcEmpRate <- sqrt(sum((dfAlc$employrt-meanDfAlcEmpRate)^2)/ length(dfAlc$employrt))
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
  #group data frame = as many tables with the same columns but not necessarily the same rows that are stacked together into one object.
  dfAlc %>%
  group_by(region) %>%
  summarize(average = mean(aconsum), standard_deviation = sd(aconsum))
------
  # ascend arrange
  dfAlc %>% arrange(aconsum) %>% head()
------
  # descend arrange
  dfAlc %>% arrange(desc(aconsum)) %>% head()
------
  # nested arrange by region alphabetically, then by alcohol consumption rate within each region
  dfAlc %>% arrange(region, aconsum) %>% head()
------
  # top 10  with highest alcohol consumption rate,not ordered by rate
  dfAlc %>% top_n(10, aconsum)
------
  # top 10  with highest alcohol consumption rate,ordered by rate
  dfAlc %>% arrange(desc(aconsum)) %>% top_n(10)
  dfAlc %>% slice_max(aconsum, n = 10) #alternative 
  # top 10  arrange by region alphabetically, then by alcohol consumption rate within each region,ordered by rate
  dfAlc %>% arrange(desc(region, aconsum))  %>% top_n(10) 
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
------ 
library(tidyverse)
#gg = grammer of graphics 
#ggplot2 is part of the tidyverse suite of packages, which you can load with library(tidyverse)
#ggplot2 is designed to work exclusively with tidy data (rows are observations and columns are variables)
dfAlc <- read.csv(
  file = ("/Users/venetabaeva/git/repository4/gapminder.csv"),
  header = TRUE,
  sep = ";",
  dec = ".")
#pipe the data
dfAlc %>% ggplot#or, 
#associates the data set with the plotting object
ggplot(data=dfAlc)  #define a ggplot object # 1st argument : what data is associated with the object.
ggplotdfAlc <- ggplot(data=dfAlc) 
class(ggplotdfAlc)
print(ggplotdfAlc)
#layers
#Layers can define geometries, compute summary statistics,define what scales to use, and even change styles.
# the first added layer defines the geometry
#  what geometry do we use?
# geom_nameofgeometry
# provide data and a mapping
?geom_point
# aes = aesthetic mapping -> the outcome of this function is often used as the argument of a geometry function
head(dfAlc)
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = employrt, y = aconsum ))
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = urbanrt, y = aconsum ))
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = suicideper100, y = aconsum ))
gridExtra::grid.arrange(dfAlc %>% 
                          filter(region == "EMEA")%>% 
                          ggplot() +
                          geom_point(aes(x = employrt, y = aconsum ),show.legend = TRUE),
                        dfAlc %>% 
                          filter(region == "EMEA")%>% 
                          ggplot() +
                          geom_point(aes(x = urbanrt, y = aconsum ),show.legend = TRUE),
                        dfAlc %>% 
                          filter(region == "EMEA")%>% 
                          ggplot() +
                          geom_point(aes(x = suicideper100, y = aconsum ),show.legend = TRUE),
                        ncol = 3)
#label text
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = employrt, y = aconsum ),show.legend = TRUE) +
  geom_text(aes(x = employrt, y = aconsum, label = abbrv ))
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = urbanrt, y = aconsum ),show.legend = TRUE)+
  geom_text(aes(x = urbanrt, y = aconsum, label = abbrv ))
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = suicideper100, y = aconsum ),show.legend = TRUE)+
  geom_text(aes(x = suicideper100, y = aconsum, label = abbrv ))
gridExtra::grid.arrange(dfAlc %>% 
                          filter(region == "EMEA")%>% 
                          ggplot() +
                          geom_point(aes(x = employrt, y = aconsum),show.legend = TRUE)+
                          geom_text(aes(x = employrt, y = aconsum, label = abbrv )),
                        dfAlc %>% 
                          filter(region == "EMEA")%>% 
                          ggplot() +
                          geom_point(aes(x = urbanrt, y = aconsum ),show.legend = TRUE)+
                          geom_text(aes(x = urbanrt, y = aconsum, label = abbrv )),
                        dfAlc %>% 
                          filter(region == "EMEA")%>% 
                          ggplot() +
                          geom_point(aes(x = suicideper100, y = aconsum),show.legend = TRUE)+
                          geom_text(aes(x = suicideper100, y = aconsum, label = abbrv )),
                        ncol = 3)
  
#label text
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot(aes(x = employrt, y = aconsum, label = abbrv )) +
  geom_label()
dfAlc %>% 
  filter(region = "EMEA")%>% 
  ggplot(aes(x = urbanrt, y = aconsum, label = abbrv )) +
  geom_label()
dfAlc %>% 
  filter(region = "EMEA")%>% 
  ggplot(aes(x = suicideper100, y = aconsum, label = abbrv )) +
  geom_label()
gridExtra::grid.arrange(dfAlc %>%
                          filter(region = "EMEA")%>% 
                          ggplot(aes(x = employrt, y = aconsum, label = abbrv )) +
                          geom_label(),
                        dfAlc %>% 
                          filter(region = "EMEA")%>% 
                          ggplot(aes(x = urbanrt, y = aconsum, label = abbrv )) +
                          geom_label(),
                        dfAlc %>%
                          filter(region = "EMEA")%>% 
                          ggplot(aes(x = suicideper100, y = aconsum, label = abbrv )) +
                          geom_label(),
                        ncol = 3)
# dot size 
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = employrt, y = aconsum ),show.legend = TRUE ,size = 0.6) +
  geom_text(aes(x = employrt, y = aconsum, label = abbrv ))
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = urbanrt, y = aconsum ),show.legend = TRUE,size = 0.6)+
  geom_text(aes(x = urbanrt, y = aconsum, label = abbrv ))
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = suicideper100, y = aconsum ),show.legend = TRUE,size = 0.6)+
  geom_text(aes(x = suicideper100, y = aconsum, label = abbrv ))

# dot size 
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = employrt, y = aconsum,size = aconsum ),show.legend = TRUE) +
  geom_text(aes(x = employrt, y = aconsum, label = abbrv ))
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = urbanrt, y = aconsum,size = aconsum ),show.legend = TRUE)+
  geom_text(aes(x = urbanrt, y = aconsum, label = abbrv ))
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = suicideper100, y = aconsum,size = aconsum ),show.legend = TRUE)+
  geom_text(aes(x = suicideper100, y = aconsum, label = abbrv ))

# label position relative to the dots 
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = employrt, y = aconsum ),show.legend = TRUE ,size = 0.6) +
  geom_text(aes(x = employrt, y = aconsum, label = abbrv ), nudge_x = 1 ,nudge_y = 1)
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = urbanrt, y = aconsum ),show.legend = TRUE ,size = 0.6)+
  geom_text(aes(x = urbanrt, y = aconsum, label = abbrv ),nudge_x = 1 ,nudge_y = 1)
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = suicideper100, y = aconsum ),show.legend = TRUE ,size = 0.6)+
  geom_text(aes(x = suicideper100, y = aconsum, label = abbrv ),nudge_x = 1 ,nudge_y = 1)
# bubblechart
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot() +
  geom_point(aes(x = employrt, y = aconsum,size = urbanrt,color = suicideper100 ),alpha=0.5) +
  scale_size(range = c(.5, 20), name="urbanrt")+
  ggrepel::geom_text_repel(
    aes(x = employrt, y = aconsum, label = abbrv, point.size = suicideper100), 
    size = 4, # font size in the text labels
    point.padding = 0, #  padding around each point
    min.segment.length = 0, # draw all line segments
    box.padding = 0.7, #padding around each text label
    max.overlaps = 100
  ) + 
  theme(legend.position = "right")
# global aesthetic mapping 
ggplotDfAlcAlconsumEmplRt <- dfAlc %>%  filter(region == "EMEA")%>% ggplot(aes(employrt,aconsum,label = abbrv))
ggplotDfAlcAlconsumEmplRt + 
  geom_point(size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)
ggplotDfAlcAlconsumUrbanRt <- dfAlc %>%  filter(region == "EMEA")%>% ggplot(aes(urbanrt,aconsum,label = abbrv))
ggplotDfAlcAlconsumUrbanRt +
  geom_point(size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)
ggplotDfAlcAlconsumSuicide100<- dfAlc %>%  filter(region == "EMEA")%>% ggplot(aes(suicideper100,aconsum,label = abbrv))
ggplotDfAlcAlconsumSuicide100 +
  geom_point(size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)
# scale
ggplotDfAlcAlconsumEmplRt + 
  geom_point(size=1)+
  geom_text(nudge_x = 0.3 ,nudge_y = 0.3) +
  scale_x_continuous(trans = "log2")+
  scale_y_continuous(trans = "log2")
# scale
ggplotDfAlcAlconsumEmplRt + 
  geom_point(size=1)+
  geom_text(nudge_x = 0.3 ,nudge_y = 0.3) +
  scale_x_log10()+
  scale_y_log10()
# scale before plotting ; If we log the data, we can more easily interpret intermediate values in the scale.
# Using base 2 for, example, means that every time a value doubles,the log transformation increases by one.
# Log transformations change multiplicative changes into additive ones
dfAlc %>%
  filter(region == "EMEA")%>% 
  ggplot(aes(log2(aconsum))) +
  geom_histogram(binwidth = 1, fill ="lemonchiffon3" ,color = "lemonchiffon") 
dfAlc %>%
  filter(region == "EMEA")%>% 
  ggplot(aes(log2(employrt))) +
  geom_histogram(binwidth = 1, fill ="lightpink3" ,color = "lightpink") 
dfAlc %>%
  filter(region == "EMEA")%>% 
  ggplot(aes(log2(urbanrt))) +
  geom_histogram(binwidth = 1, fill = "lightgoldenrod3",color = "lightgoldenrod")
dfAlc %>%
  filter(region == "EMEA")%>% 
  ggplot(aes(log2(suicideper100))) +
  geom_histogram(binwidth = 1, fill = "lightblue3",color = "lightblue")
# scale after plotting ; make plots where the scales have been log transformed
dfAlc %>%
  filter(region == "EMEA")%>% 
  ggplot(aes(aconsum)) +
  geom_histogram(binwidth = 1, fill ="lemonchiffon3" ,color = "lemonchiffon") +
 scale_x_continuous(trans = "log2")
dfAlc %>%
  filter(region == "EMEA")%>% 
  ggplot(aes(employrt)) +
  geom_histogram(binwidth = 1, fill ="lightpink3" ,color = "lightpink") +
  scale_x_continuous(trans = "log2")
dfAlc %>%
  filter(region == "EMEA")%>% 
  ggplot(aes(urbanrt)) +
  geom_histogram(binwidth = 1, fill = "lightgoldenrod3",color = "lightgoldenrod")+
  scale_x_continuous(trans = "log2")
dfAlc %>%
  filter(region == "EMEA")%>% 
  ggplot(aes(suicideper100)) +
  geom_histogram(binwidth = 1, fill = "lightblue3",color = "lightblue")+
  scale_x_continuous(trans = "log2")
# change label and add title 
ggplotDfAlcAlconsumEmplRt + 
  geom_point(size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Employment Rate")+
  ggtitle("EMEA Alcohol Consumption")
ggplotDfAlcAlconsumUrbanRt +
  geom_point(size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Urban Rate")+
  ggtitle("EMEA Alcohol Consumption")
ggplotDfAlcAlconsumSuicide100 +
  geom_point(size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Suicide/100 People")+
  ggtitle("EMEA Alcohol Consumption")
# color
ggplotDfAlcAlconsumEmplRt + 
  geom_point(size=0.6,color ="red3")+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Employment Rate")+
  ggtitle("EMEA Alcohol Consumption")
ggplotDfAlcAlconsumUrbanRt +
  geom_point(size=0.6,color ="royalblue1")+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Urban Rate")+
  ggtitle("EMEA Alcohol Consumption")
ggplotDfAlcAlconsumSuicide100 +
  geom_point(size=0.6,color ="limegreen")+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Suicide/ 100 People")+
  ggtitle("EMEA Alcohol Consumption")
# color to category 
ggplotDfAlcAlconsumEmplRt + 
  geom_point(aes(col=region),size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Employment Rate")+
  ggtitle("EMEA Alcohol Consumption")
ggplotDfAlcAlconsumUrbanRt +
  geom_point(aes(col=region),size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Urban Rate")+
  ggtitle("EMEA Alcohol Consumption")
ggplotDfAlcAlconsumSuicide100 +
  geom_point(aes(col=region),size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Suicide/ 100 People")+
  ggtitle("EMEA Alcohol Consumption")
# color to category 
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot(aes(x = employrt, y = aconsum, label = abbrv, color = region )) +
  geom_label() +
  geom_text(check_overlap = TRUE)
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot(aes(x = urbanrt, y = aconsum, label = abbrv, color = region )) +
  geom_label() +
  geom_text(check_overlap = TRUE)
dfAlc %>% 
  filter(region == "EMEA")%>% 
  ggplot(aes(x = suicideper100, y = aconsum, label = abbrv,color = region )) +
  geom_label()+
  geom_text(check_overlap = TRUE)
  
# line representing the average 


dfAlcEMEA <- dfAlc %>% filter(region == "EMEA")
lm.rAcEmp <- summary(lm(dfAlcEMEA$aconsum ~ dfAlcEMEA$employrt ))# intercept extraction 
lm.rAcUrb <- summary(lm(dfAlcEMEA$aconsum  ~ dfAlcEMEA$urbanrt)) # intercept extraction 
lm.rAcSu <- summary(lm(dfAlcEMEA$aconsum  ~ dfAlcEMEA$suicideper100)) # intercept extraction 
plot(lm(dfAlc$aconsum ~ dfAlc$employrt ))
plot(lm(dfAlc$aconsum ~ dfAlc$urbanrt ))
plot(lm(dfAlc$aconsum ~ dfAlc$suicideper100 ))

ggplotDfAlcAlconsumEmplRt + 
  geom_point(aes(col=region),size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Employment Rate")+ 
  ggtitle("World Alcohol Consumption")+
  geom_abline(na.rm = TRUE, intercept = 4.84388)
ggplotDfAlcAlconsumUrbanRt +
  geom_point(aes(col=region),size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Urban Rate")+
  ggtitle("World Alcohol Consumption")+
  geom_abline(na.rm = TRUE, intercept = 3.85179)
ggplotDfAlcAlconsumSuicide100 +
  geom_point(aes(col=region),size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Suicide/ 100 People")+
  ggtitle("World Alcohol Consumption")+
  geom_abline(na.rm = TRUE, intercept = 2.04562)

# line representing the average color 

ggplotDfAlcAlconsumEmplRt + 
  geom_point(aes(col=region),size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Employment Rate")+
  ggtitle("EMEA Alcohol Consumption")+
  geom_abline(na.rm = TRUE, intercept= 4.84388,lty = 1, color = "red" )
ggplotDfAlcAlconsumUrbanRt +
  geom_point(aes(col=region),size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Urban Rate")+
  ggtitle("EMEA Alcohol Consumption")+
  geom_abline(na.rm = TRUE, intercept = 3.85179,lty =2 , color = "red" )
ggplotDfAlcAlconsumSuicide100 +
  geom_point(aes(col=region),size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Suicide/ 100 People")+
  ggtitle("EMEA Alcohol Consumption")+
  geom_abline(na.rm = TRUE, intercept = 2.04562,lty = 3, color = "red" )

# add - on packages 
# ggthemes

?ds_theme_set()
?ggthemes

ggplotDfAlcAlconsumSuicide100 +
  ggthemes::theme_economist()+
  geom_point(aes(col=region),size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Suicide/ 100 People")+
  ggtitle("EMEA Alcohol Consumption")+
  geom_abline(na.rm = TRUE, intercept = 2.04562,lty = 3, color = "red" )

ggplotDfAlcAlconsumSuicide100 +
  ggthemes::theme_fivethirtyeight()+
  geom_point(aes(col=region),size=0.6)+
  geom_text(nudge_x = 1 ,nudge_y = 0.5)+
  ylab("Alcohol Consumption Rate")+
  xlab("Suicide/ 100 People")+
  ggtitle("EMEA Alcohol Consumption")+
  geom_abline(na.rm = TRUE, intercept = 2.04562,lty = 3, color = "red" )

# ggrepel
#includes a geometry that adds labels ensuring that they don't fall on top of each other.
ggplotDfAlcAlconsumSuicide100 +
  ggthemes::theme_fivethirtyeight()+
  geom_point(aes(col=region),size=1)+
  ggrepel::geom_text_repel(
    nudge_x = .15,
    box.padding = 0.5,
    nudge_y = 1,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 30,
    max.overlaps = 100) + 
  theme_bw() + 
  theme(legend.position = 'none')+
  ylab("Alcohol Consumption Rate")+
  xlab("Suicide/ 100 People")+
  ggtitle("EMEA Alcohol Consumption")+
  geom_abline(na.rm = TRUE, intercept = 2.04562,lty = 3, color = "red" )

ggplotDfAlcAlconsumSuicide100 +
  ggthemes::theme_fivethirtyeight()+
  geom_point(aes(col=region,size = aconsum ))+
  ggrepel::geom_text_repel(
    nudge_x = .15,
    box.padding = 0.5,
    nudge_y = 1,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 30,
    max.overlaps = 100) + 
  theme_bw() + 
  theme(legend.position = 'none')+
  ylab("Alcohol Consumption Rate")+
  xlab("Suicide/ 100 People")+
  ggtitle("EMEA Alcohol Consumption")+
  geom_abline(na.rm = TRUE, intercept = 2.04562,lty = 3, color = "red" )
------ 
# geom_histogram 
dfAlcEMEA <- dfAlc %>%
filter(region == "EMEA") %>%
ggplot(aes(x = aconsum))
dfAlcEMEA + 
  geom_histogram()
# geom_histogram add bin width
dfAlcEMEA + 
  geom_histogram(binwidth = 1)
# geom_histogram color
dfAlcEMEA + 
  geom_histogram(binwidth = 1, fill = "cyan3", col = "cyan4") 
# geom_histogram xlabel and title 
dfAlcEMEA + 
  geom_histogram(binwidth = 1, fill = "cyan3", col = "cyan4") +
  xlab("EMEA Alcohol Consumption") +
  ggtitle("Histogram")
# geom_density
dfAlcEMEA + 
  geom_density()
# geom_density color
dfAlcEMEA + 
  geom_density(fill = "bisque2", col = "bisque3") 
  xlab("EMEA Alcohol Consumption") +
  ggtitle("Density Plot")
# geom_qq plot
dfAlcEMEA <- dfAlc %>%
filter(region == "EMEA") %>%
ggplot(aes(sample = aconsum))
dfAlcEMEA + 
  geom_qq(fill = "darkgoldenrod2", col = "darkgoldenrod3") +
  geom_qq_line(col = "darkgray") +
  xlab("EMEA Alcohol Consumption") +
  ggtitle("QQ Plot")
# Note: By default, the Q-Q plot is compared to the normal distribution with average zero and standard deviation one
# Q-Q plot is plotted against a normal distribution with the same mean and standard deviation as our data
# QQ-plot against a normal distribution with same mean/sd as data
params <- dfAlc %>%
  filter(region == "EMEA") %>%
  summarize(mean = mean(aconsum), sd = sd(aconsum))
dfAlcEMEA +
  geom_qq(dparams = params, fill = "darkgoldenrod2", col = "darkgoldenrod3") +
  geom_qq_line(col = "darkgray") + 
  geom_abline(col = "brown3")+ #how well the normal approximation works #the points fall roughly on the line. This is because this data is approximately normal.
  xlab("EMEA Alcohol Consumption") +
  ggtitle("QQ Plot")
# 2nd option: scale data in standard units -> plot 
# QQ-plot of scaled data against the standard normal distribution
dfAlc %>%
filter(region == "EMEA") %>%
  ggplot(aes(sample = scale(aconsum))) +
  geom_qq(fill = "darkgoldenrod2", col = "darkgoldenrod3") +
  geom_abline(col = "cornflowerblue")+
  xlab("EMEA Alcohol Consumption") +
  ggtitle("QQ Plot")
# put plots next to each other
dfAlcEMEA <- dfAlc %>%
  filter(region == "EMEA") %>%
  ggplot(aes(x = aconsum))
dfAlcAPAC <- dfAlc %>%
  filter(region == "APAC") %>%
  ggplot(aes(x = aconsum))
histEMEA <- dfAlcEMEA + geom_histogram(binwidth = 1, fill = "aquamarine2", col = "aquamarine4")
histAPAC <- dfAlcAPAC + geom_histogram(binwidth = 1, fill = "aquamarine4", col = "aquamarine2")
gridExtra::grid.arrange(histEMEA, histAPAC, ncol = 2)
# group by category 
dfAlc %>%
  ggplot(aes(aconsum, group = region))+
  geom_density()
# color by category 
dfAlc %>%
  ggplot(aes(aconsum, color = region))+
  geom_density()
# fill by category 
dfAlc %>%
  ggplot(aes(aconsum, fill = region))+
  geom_density()
# fill by category and transparent
dfAlc %>%
  ggplot(aes(aconsum, fill = region))+
  geom_density(alpha = 0.2)
#gapminder data set
library(dslabs)
data(gapminder)
head(gapminder)
View(gapminder)
# filter 
gapminder %>%
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey"))%>%
  select(country, infant_mortality)
gapminder %>%
  filter(year == 2015 & country %in% c("Malaysia", "Russia"))%>%
  select(country,infant_mortality)
gapminder %>%
  filter(year == 2015 & country %in% c("Syria", "Turkey"))%>%
  select(country,infant_mortality)
# scatterplot
ds_theme_set()    # set plot theme
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()
# facet_grid 
filter(gapminder, year %in% c(1962, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent ~ year)
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)
# facet_grid motion 
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)
# scatterplot time series 
gapminder %>%
  filter(country == "Turkey") %>%
  ggplot(aes(year, fertility)) +
  geom_point(aes(colour = country))+
  xlim(1962,2012)+
  ylim(0,10)
gapminder %>%
  filter(country == "Sri Lanka") %>%
  ggplot(aes(year, fertility)) +
  geom_point(aes(colour = country))+
  xlim(1962,2012)+
  ylim(0,10)
gapminder %>%
  filter(country == "Syria") %>%
  ggplot(aes(year, fertility)) +
  geom_point(aes(colour = country))+
  xlim(1962,2012)+
  ylim(0,10)
# linePlot time series 
gapminder %>%
  filter(country == "Turkey") %>%
  ggplot(aes(year, fertility)) +
  geom_line(aes(colour = country))+
  xlim(1962,2012)+
  ylim(0,10)
gapminder %>%
  filter(country == "Sri Lanka") %>%
  ggplot(aes(year, fertility)) +
  geom_line(aes(colour = country))+
  xlim(1962,2012)+
  ylim(0,10)
gapminder %>%
  filter(country == "Syria") %>%
  ggplot(aes(year, fertility)) +
  geom_line(aes(colour = country))+
  xlim(1962,2012)+
  ylim(0,10)
# linePlot multiple time series 
gapminder %>% filter(country %in% c("Turkey","Sri Lanka","Syria")) %>%
  ggplot(aes(year, fertility, group = country)) +
  geom_line()+
  xlim(1962,2012)+
  ylim(0,10)
# linePlot multiple time series color
gapminder %>% filter(country %in% c("Turkey","Sri Lanka","Syria")) %>%
  ggplot(aes(year, fertility, col = country)) +
  geom_line()+
  xlim(1962,2012)+
  ylim(0,10)
# linePlot multiple time series color, label
countries <-  c("Turkey","Sri Lanka","Syria")
labels <- data.frame(country = countries, x = c(1964, 1965,1964), y = c(6.40,5.50,7.85))
gapminder %>% filter(country %in% c("Turkey","Sri Lanka","Syria")) %>%
  ggplot(aes(year, fertility, col = country)) +
  geom_line() +
  xlim(1962,2012)+
  ylim(0,10)+
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")
# mutate dataFrame 
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)
# histogram 
year1962 <- 1962 
gapminder %>%
  filter(year == 1962 & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, fill = "lightgoldenrod1", color = "black")+
  xlim(0,150)+
  ylim(0,30)
year2011 <- 2011
gapminder %>%
  filter(year == year2011 & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, fill = "indianred1", color = "black") +
  xlim(0,150)+
  ylim(0,30)
# log2 transformation  -< needed due to the majority of the x-axis  dedicated to the 35 countries with averages above 10
# log the values before plotting them
year1962 <- 1962 
gapminder %>%
  filter(year == year1962 & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, fill = "lightgoldenrod1", color = "black")+
  xlim(0,150)+
  ylim(0,30) 
year2011 <- 2011
gapminder %>%
  filter(year == year2011 & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, fill = "indianred1", color = "black") +
  xlim(0,150)+
  ylim(0,30)
# Note: do not recommend usage of log10, the natural log for data exploration and visualization
# Note: a bin width of 1 will translate to bins with range x to 2 to the x
# log the values on the x - axis 
year1962 <- 1962 
gapminder %>%
  filter(year == year1962 & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, fill = "lightgoldenrod1", color = "black")+
  xlim(0,32)+
  ylim(0,30)+
  scale_x_continuous(trans = "log2")
year2011 <- 2011
gapminder %>%
  filter(year == year2011 & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, fill = "indianred1", color = "black") +
  xlim(0,32)+
  ylim(0,30)+
  scale_x_continuous(trans = "log2")
# boxplot
year1962 <- 1962 
gapminder %>%
  filter(year == year1962 & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
year2011 <- 2011 
gapminder %>%
  filter(year == year2011 & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# stratify factor by level
length(levels(gapminder$continent))
# check levels of a factor
regionFac <- levels(gapminder$continent)
# reorder factor levels on a summary computed on a numeric vector
value <- c(10, 11, 12, 6, 4)
facRegByValue <- reorder(regionFac, value, FUN = mean)
levels(facRegByValue)
# boxplot reordered by median income, scaled, and color by continent
gapminder %>%
  filter(year == year1962 & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%   
  ggplot(aes(region, dollars_per_day, fill = continent)) +    
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")
gapminder %>%
  filter(year == year2011 & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%   
  ggplot(aes(region, dollars_per_day, fill = continent)) +    
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")
# log2 scale y-axis 
gapminder %>%
  filter(year == year1962 & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%   
  ggplot(aes(region, dollars_per_day, fill = continent)) +    
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")+
  scale_y_continuous(trans = "log2")
gapminder %>%
  filter(year == year2011 & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%   
  ggplot(aes(region, dollars_per_day, fill = continent)) +    
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")+
  scale_y_continuous(trans = "log2")

# add points on boxplot 

gapminder %>%
  filter(year == year1962 & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%   
  ggplot(aes(region, dollars_per_day, fill = continent)) +    
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")+
  scale_y_continuous(trans = "log2")+ 
  geom_point(show.legend = FALSE)
gapminder %>%
  filter(year == year2011 & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%   
  ggplot(aes(region, dollars_per_day, fill = continent)) +    
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")+
  scale_y_continuous(trans = "log2")+ 
  geom_point(show.legend = FALSE)
# define group  - vector 
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
# facet groups
gapminder %>%
  filter(year == year1962 & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ group)
# facet groups
gapminder %>%
  filter(year %in% c(year1962, year2011) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)
# define groups 
countryList1962 <- gapminder %>%
  filter(year == year1962 & !is.na(dollars_per_day)) %>% .$country
countryList2011 <- gapminder %>%
  filter(year == year2011 & !is.na(dollars_per_day)) %>% .$country
country19622011Intersect <- intersect(countryList1962, countryList2011)
# histogram
gapminder %>%
  filter(year %in% c(year1962, year2011) & country %in% country19622011Intersect) %>%    # keep only selected countries
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)
# boxplot
gapminder %>%
  filter(year %in% c(year1962, year2011) & country %in% country19622011Intersect) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  scale_y_continuous(trans = "log2")+ 
  geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~ .)
# boxplot
gapminder %>%
  filter(year %in% c(year1962, year2011) & country %in% country19622011Intersect) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  scale_y_continuous(trans = "log2")+  
  geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))
# smooth density plots ; areas of the densities be proportional to the size of the groups,multiply the y-axis values by the size of the group
gapminder %>%
  filter(year == year1962 & country %in% country19622011Intersect) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2")+ 
  geom_density(alpha = 0.2, bw = 0.75) + # bw for smoothness 
  facet_grid(year ~ .)
# case_when
# add group as a factor
gapminder<-gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))
# reorder factor levels
gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))
# stacked density plot
gapminder %>%
  filter(year %in% c(year1962, year2011) & country %in% country19622011Intersect) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")+
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)
# weighted stacked density plot 
gapminder %>%
  filter(year %in% c(year1962, year2011) & country %in% country19622011Intersect) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)
# ecological fallacy = the almost perfect relationship between survival rates and income is only observed for the averages at the regional level
# add additional cases
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))
# logit transformation = f of p equals the log of p divided by 1 minus p
# when one wants to highlight differences that are near 0 or near 1
surv_income <- gapminder %>%
  filter(year %in% year2011 & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)
surv_income %>% 
  ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE)
# ggplot aes 
gapminder %>% 
  filter( year == 2011,continent == 'Africa') %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point() 
# ggplot color category
gapminder %>% 
  filter(year ==2011 & continent == "Africa") %>%
  ggplot(aes(fertility, life_expectancy, color = region))+
  geom_point()
# conditions  
gapminder %>%
  filter(year == 2011 & continent =="Africa" & fertility <=3 & life_expectancy >= 70)%>%
  select(country,region)
# line plot time series plot 
gapminder %>%
  filter(country %in% c("Vietnam","United States","Cambodia") & year>=1960 & year <=2010) %>%
  ggplot(aes(year, life_expectancy,color = country))+
  geom_line()
# line plot frequency 
gapminder %>% 
  mutate(dollars_per_day = gdp/population/365)%>%
  filter(continent == "Africa", year == 2010,!is.na(gdp)) %>%
  ggplot(aes(dollars_per_day, y = ..count..))+
  geom_density()+
  scale_x_continuous(trans = "log2")
# density plot 
gapminder %>%
  filter(continent == "Africa", year  %in% c(1970,2010),!is.na(gdp)) %>% mutate(dollars_per_day = gdp/population/365) %>%
  ggplot(aes(dollars_per_day, y =..count..)) +
  geom_density()+
  scale_x_continuous(trans="log2")+
  facet_grid(.~year)
# stack density plot
gapminder %>%
  filter(continent == "Africa", year  %in% c(1970,2010),!is.na(gdp)) %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  ggplot(aes(dollars_per_day, y =..count.., fill = region)) +
  geom_density(bw = 0.5, position = "stack")+
  scale_x_continuous(trans="log2")+
  facet_grid(year~.)
# scatter plot
gapminder %>%
  filter(continent == "Africa", year  == 2010,!is.na(gdp)) %>% 
  mutate(dollars_per_day = gdp/population/365)%>% 
  ggplot(aes(dollars_per_day,infant_mortality,color = region))+
  geom_point()
# scale scatter plot
gapminder %>%
  filter(continent == "Africa", year  == 2010,!is.na(gdp)) %>% 
  mutate(dollars_per_day = gdp/population/365)%>% 
  ggplot(aes(dollars_per_day,infant_mortality,color = region))+
  geom_point()+ 
  scale_x_continuous(trans="log2")
# label scatter plot 
gapminder %>%
  filter(continent == "Africa", year  == 2010,!is.na(gdp)) %>% 
  mutate(dollars_per_day = gdp/population/365)%>% 
  ggplot(aes(dollars_per_day,infant_mortality,color = region, label = country))+
  geom_point()+ 
  scale_x_continuous(trans="log2")+
  geom_text()
# scatter plot facet 
gapminder %>%
  filter(continent == "Africa", year  %in% c(1970,2010),!is.na(gdp),!is.na(infant_mortality)) %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  ggplot(aes(dollars_per_day,infant_mortality, color = region, label = country)) +
  geom_point()+
  scale_x_continuous(trans="log2")+
  geom_text()+
  facet_grid(year~.)
# Visual Principles; descending in importance
## position
## aligned lengths
## angles
## area
## brightness
## color hue
### Do not use:
#### pie chart <- reprs quantities with both areas & angles
#### donut chart <- reprs quantities with only areas 
###Why?  humans are better at judging linear measures => bar plot = bars of length proportional to the quantity of interest
#### zero -> when position
#### quantities distortion -> when  the radius, rather than the area, was made to be proportional to the quantity, which implies that the proportions between the areas is squared. So 2.6 turns into 6.5, and 5.8, turns into 34.1
#### bubblechart instead of bar plot 
#### non meaningfull value as alphabetic order 
###Use:
#### zero -> when length 
#### meaningfull value to order 
# Showing data 
# dot plot instead of bar plot 
heights %>%
  ggplot(aes(sex, height)) + 
  geom_point()
# jitter = adding a small random shift to each point and alpha blending 
heights %>% 
  ggplot(aes(sex, height)) + 
  geom_jitter(width = 0.1, alpha = 0.2)+ 
  geom_boxplot(alpha= 0.5)
# histogram ->  keep the axes the same when comparing data across plots
# comparing data across plots -> align plots vertical to see horizontal changes, and horizontally to see vertical changes
# log transformation -> when the changes are multiplicative
# logistic transformation -> for  better see fold changes in odds
# square root transformation ->  for count data
# reorder
usContDiseases <- us_contagious_diseases %>%
  filter(year == 1967 & disease=="Measles" & 
           !is.na(population)) %>% 
  mutate(rate = count / population * 10000 * 52 / weeks_reporting)
state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)
state <- reorder(state,rate, FUN = mean)
# numeric order 
library(dplyr)
library(ggplot2)
library(dslabs)
data(us_contagious_diseases)
usContDiseasesMeasles <- us_contagious_diseases %>% 
  filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting)
usContDiseasesMeasles %>% 
  mutate(state = reorder(state, rate, FUN = mean)) %>% 
  ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()
# numeric order 
gapminder %>%  
  filter(!is.na(fertility)) %>%
  mutate(region = reorder(region, fertility, FUN = median)) %>% 
  ggplot(aes(region, fertility)) +
  geom_bar(stat="identity") +
  coord_flip()
# numeric order  
library(dplyr)
library(ggplot2)
library(dslabs)
View(dfAlc)
data("murders")
murders %>% mutate(rate = total/population*100000) %>%
  mutate(region = reorder(region, rate, FUN = median)) %>% 
  ggplot(aes(region,rate)) +
  geom_boxplot() + 
  geom_point()
# slope chart 
# when comparing variables of the same type; at different time points;  for a relatively small number of comparison
library(tidyverse)
library(dslabs) 
data(gapminder)
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder20002015Slope <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)
gapminder20002015Slope %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 
#The Bland-Altman plot = Tukey Mean Different plot
library(ggrepel)
gapminder20002015Slope %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")
# plot intercept
library(tidyverse)
library(dslabs)
the_disease <- "Measles"
usContDisMeasles <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))
usContDisMeasles %>% 
  filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue") 
# tile plot of disease rate by state and year heatmap
usContDisMeasles %>% 
  ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")
#line plot
# compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)
#  line plot of measles rate by year by state
usContDisMeasles %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")
# When choosing colors to quantify a numeric variable, we choose between two options, sequential and diverging.
# Sequential palettes are suited for data that goes from high to low
# color brewer 
# tile plot
library(dplyr)
library(ggplot2)
library(dslabs)
the_disease = "Smallpox"
usContDiseasesSmallpox <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))
usContDiseasesSmallpox %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")
#series plot
the_disease = "Smallpox"
usContDiseasesSmallpox <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))
avg <- us_contagious_diseases %>%
  filter(disease==the_disease & weeks_reporting >= 10) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)
usContDiseasesSmallpox %>% 
  ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")
#series plot
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)
us_contagious_diseases %>% 
  filter(state=="California"  & weeks_reporting >= 10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate,color = disease)) + 
  geom_line()
#
us_contagious_diseases %>%
  filter(!is.na(population)) %>%
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color = disease)) +
  geom_line()

#Titanic package
options(digits = 3)  
library(tidyverse)
library(titanic)
# select mutate 
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
# density plots  facet
titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2) +
  facet_grid(Sex ~ .)
# density plots  without facet
titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2)
# frequency plot
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack")
#QQ-plot 
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
titanic %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()
# barplot
# plot 1 - survival filled by sex
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar()
# plot 2 - survival filled by sex with position_dodge
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())
# plot 3 - sex filled by survival
titanic %>%
  ggplot(aes(Sex, fill = Survived)) +
  geom_bar()
# density plot
#Which age group is the only group more likely to survive than die? 0-8
#Which age group had the most deaths? 18-30
#Which age group had the highest proportion of deaths? 70-80
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)
# boxplot
# Filter the data to remove individuals who paid a fare of 0
# Make a boxplot of fare grouped by survival status. 
# Try a log2 transformation of fares
# Add the data points with jitter and alpha blending.
titanic %>%
  filter(Fare != 0)%>%
  ggplot(aes(Survived,Fare)) +
  geom_boxplot(alpha = 0.2) +
  scale_y_continuous(trans = "log2")+
  geom_jitter()
#Passengers who survived generally payed higher fares than those who did not survive.
#The median fare was lower for passengers who did not survive.
#Most individuals who paid a fare around $8 did not survive.
# barplot
# basic barplot of passenger class filled by survival
titanic %>%
  ggplot(aes(Pclass, fill = Survived))+
  geom_bar()
# same barplot but use the argument position = position_fill() to show relative proportions in each group instead of counts
titanic %>%
  ggplot(aes(Pclass, fill = Survived))+
  geom_bar(position = position_fill())
# barplot of survival filled by passenger class using position = position_fill()
titanic %>%
  ggplot(aes(Survived, fill = Pclass))+
  geom_bar(position = position_fill())
#There were more third class passengers than passengers in the first two classes combined.
#Survival proportion was highest for first class passengers, followed by second class. Third-class had the lowest survival proportion.
#Most passengers in first class survived. Most passengers in other classes did not survive.
#The majority of those who did not survive were from third class.
# grid of density plots 
titanic %>%
  ggplot(aes(Age, fill = Survived, y = ..count..)) +
  geom_density(alpha = 0.2) +
  facet_grid(Pclass~ Sex) 
# The largest group of passengers was third-class males.
# Most first-class and second-class females survived.
# Almost all second-class males did not survive, with the exception of children.
# Properties of starts exercise 
library(tidyverse)
library(dslabs)
data(stars)
# digits' number 
options(digits = 3)
# mean and sd
mean(stars$magnitude)
sd(stars$magnitude)
#  density plot of the magnitude for peaks number check 
stars %>%
  ggplot(aes(magnitude)) +
  geom_density(alpha = 0.2) 
# distribution of star temperature
stars %>% 
  ggplot(aes(temp)) +
  geom_histogram()# The majority of stars have a low temperature.
#  a scatter plot: temperature x-axis/ magnitude  y-axis/  examine the relationship  Note:  lower magnitude means a more luminous (brighter) star
stars %>% 
  ggplot(aes(temp, magnitude)) +
  geom_point() #decreasing exponential
#astronomers usually transform values of star luminosity and temperature before plotting
# log transform 
stars %>% 
  ggplot(aes(temp, magnitude)) +
  scale_y_reverse()+
  scale_x_continuous(trans = "log10")+
  scale_x_reverse()+
  geom_point()#The brighest, highest temperature stars are in the upper left corner of the plot; For main sequence stars, hotter stars have higher luminosity
# estimate the average temperature of a giant.
estimateGigants<- stars%>%
  filter(temp <10000)
mean(estimateGigants$temp)
# label
stars %>% 
  ggplot(aes(temp, magnitude,label = star)) +
  scale_y_reverse()+
  scale_x_continuous(trans = "log10")+
  scale_x_reverse()+
  geom_label(size = 3, show.legend = TRUE) 
# color point
head(stars)
stars %>%  
  ggplot(aes(temp, magnitude, color = type)) +
  scale_y_reverse()+
  scale_x_continuous(trans = "log10")+
  scale_x_reverse()+
  geom_point()
# Climate Change Exercises
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
head(temp_carbon)
# code blocks return the latest year for which carbon emissions are reported
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()
#
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()
#
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()
#  the first year for which carbon emissions (carbon_emissions) data are available
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  min()
# How many times larger were carbon emissions in the last year relative to the first year?
lastYear <- temp_carbon %>%
  filter(year == 2014)
firstYear <- temp_carbon %>%
  filter(year == 1751)
proportion<- lastYear$carbon_emissions/firstYear$carbon_emissions
#  the first year for which global temperature anomaly (temp_anomaly) data are available
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  pull(year) %>%
  min()
#  the last year for which global temperature anomaly data are available
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  pull(year) %>%
  max()
# How many degrees Celsius has temperature increased over the date range? 
# Compare the temperatures in the most recent year versus the oldest year.
lastYear <- temp_carbon %>%
  filter(year == 2018)
firstYear <- temp_carbon %>%
  filter(year == 1880)
difference<- lastYear$temp_anomaly - firstYear$temp_anomaly
# 20th century mean temperature
twCentury <- temp_carbon %>%
  filter(!is.na(temp_anomaly) & year %in% c(1901:2000))
# Create a time series line plot of the temperature anomaly
# Only include years where temperatures are reported
twCentury %>%
  ggplot(aes(year,temp_anomaly)) +
  geom_line() +
  geom_hline(yintercept = 0, col="blue") +
  ylab("t anomaly in C degrees") +
  ggtitle("t anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue") +
  geom_vline(aes(xintercept=1939),col="red") +
  geom_vline(aes(xintercept=1976),col="green") +
  geom_text(aes(x=1936,y=0.05,label="1939"),col="red") +
  geom_text(aes(x=1978,y=0.05,label="1976"),col="green")
# line plot 
temp_carbon%>%
  filter(year %in% c(1880:2020)) %>%
  filter(!is.na(temp_anomaly)) %>%
  filter(!is.na(ocean_anomaly))%>%
  filter(!is.na(land_anomaly))%>%
  ggplot() +
  geom_line(aes(x = year,y=temp_anomaly), col = "black")+
  geom_line(aes(x = year,y=ocean_anomaly), col = "blue")+
  geom_line(aes(x = year,y=land_anomaly), col = "green")+ 
  ylab("anomaly")+
  geom_hline(yintercept = 0, col="black",alpha=0.5) +
  geom_vline(xintercept = 2018, col="black",alpha=0.5)
# line plot intercept 
greenhouse_gases %>%
  ggplot(aes(x= year,y = concentration)) +
  geom_line() +
  facet_grid(gas ~., scales = "free") +
  geom_vline(xintercept = 1850)+ #Add a vertical line with an x-intercept at the year 1850, noting the unofficial start of the industrial revolution and widespread fossil fuel consumption
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")
# line plot : a time series line plot of carbon emissions
temp_carbon%>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot() +
  geom_line(aes(x = year,y=carbon_emissions), col = "black")+
  ylab("carbon emissions")
# line plot of co2 concentration over time (year), coloring by the measurement source (source)
co2_time <- historic_co2 %>%
  ggplot(aes(year,co2,color = source))+
  geom_line()
co2_time_recent <- historic_co2 %>%
  filter(year>1500) %>%
  ggplot(aes(year,co2,color=source)) +
  geom_line()
# x-axis limits to -800,000 and -775,000
# About how many years did it take for co2 to rise from 200 ppmv to its peak near 275 ppmv?
historic_co2 %>%
  ggplot(aes(year,co2,color=source)) +
  geom_line() +
  xlim(-800000,-775000)
# Change the x-axis limits to -375,000 and -330,000
# About how many years did it take for co2 to rise from the minimum of 180 ppm to its peak of 300 ppmv?
historic_co2 %>%
  ggplot(aes(year,co2,color=source)) +
  geom_line() +
  xlim(-375000 ,-330000)
# Change the x-axis limits to -140,000 and -120,000
# About how many years did it take for co2 to rise from 200 ppmv to its peak near 280 ppmv?
historic_co2 %>%
  ggplot(aes(year,co2,color=source)) +
  geom_line() +
  xlim(-140000 ,-120000)
# Change the x-axis limits to -3000 and 2018 to investigate modern changes in co2
# About how many years did it take for co2 to rise from its stable level around 275 ppmv to the current level of over 400 ppmv?
historic_co2 %>%
  ggplot(aes(year,co2,color=source)) +
  geom_line() +
  xlim(1700,2018)

# Probability 
# cumulative distribution function
## Do not: assign a very small probability to every single value
## Do:  define a function that operates on intervals rather than single values
# F(a) = Pr (x<=a)
## Pr(x>70.5) = 1- Pr(x<=70.5) = 1-F(70.5)
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% 
  filter(sex=="Male") %>% 
  pull(height)
# theoretical distribution 
# the cumulative distribution for the normal distribution
F <- function(a) mean(x <= a)
1 - F(70) # Pr of x <= a
# Pr
## pnorm() ->  distribution function which is integral of the density function
## Note: no need of data ->  just mean and sd 
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% 
  filter(sex=="Male") %>% 
  pull(height)
# the cumulative distribution for the normal distribution
1 - pnorm(70.5, mean(x), sd(x)) 
# Note:  the normal distribution is defined for continuous variables; not described for discrete variables
# theoretical distribution
# Note: a continuous variable can be taken as categorical -> each specific x as a unique category -> Pr distribution is then defined by the proportion of n - number of subjects reporting each of those unique x
# plot distribution of exact x's
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")
# Pr on actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)
# Pr in normal approximation -> check whether match 
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x)) 
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))
# Note: Do not : calculate Pr on actual data over ranges differing from 1 
# discretization = true x distribution is continuous and  reported x's tend to be more common at discrete values, in this case, due to rounding
# Pr density  -> if Pr(x)= freq of x/ N of X, then  if Pr(4), then 1/6 (die example)
# CDF -> if F(4) = Pr(x<=4), then Pr(x=4) + Pr(x=3) + Pr(x=2) + Pr(x=1)
# Note: for continuous distributions, the probability of a single value is not defined
# PDF  
## Note: has a similar to CDF interpretation
## PDF -> the quantity with the most similar interpretation to the Pr of a single value  x is the PDF ->  f(x)
## integral of f(x) over a range gives the CDF of that range 
# dnorm() -> density function 
## Note: PDF for the normal distribution
## Note:  essential to those wanting to fit models to data for which predefined functions are not available
# Pr density for the normal distribution
library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()
# dnorm(z) -> density curve (calculating the density over a range of possible values of z) for the normal distribution -> Pr density f(z) of a certain z-score
## series of z-scores 
## calculate f(z), which is dnorm() of the series of z-scores
## plot z  against f(z) 
## Pr for alternative normal distributions with mean mu and sd sigma can be evaluated 
### dnorm(z, mu, sigma) 
# Monte Carlo Simulations -> answer questions related to what could happen by chance   
# rnorm(n, avg, s) -> generates n random numbers from the normal distribution with average avg and  sd s
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% 
  filter(sex=="Male") %>% 
  pull(height)
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s) # generates simulated x data using normal distribution
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2) 
# Monte Carlo simulation -> for  tallest person over 7 feet 
B <- 10000  
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random x
  max(simulated_data)    # determine max x from 800 normally distributed random x with avg x and s x
}) 
mean(tallest >= 7*12)    # proportion of times that max x exceeded 7 feet (84 inches)
#  continuous distributions, other
library(tidyverse)
library(dslabs)
data(heights)
y <- heights %>% 
  filter(sex=="Female") %>% 
  pull(height)
fAvg <- mean(y)
fSd <- sd(y)
pnorm(5*12,fAvg,fSd) # If we pick a female at random, what is the probability that she is 5 feet or shorter?
1 - pnorm(6*12,fAvg,fSd) # If we pick a female at random, what is the probability that she is 6 feet or taller?
pnorm(67,fAvg,fSd) - pnorm(61,fAvg,fSd)# If we pick a female at random, what is the probability that she is between 61 and 67 inches?
oneSdUp <- fAvg + fSd # x  within 1 SD up from the average height
oneSdDown <- fAvg - fSd #   x  within 1 SD down from the average height
pnorm(oneSdUp,fAvg,fSd) - pnorm(oneSdDown,fAvg,fSd)#  Pr that x of a randomly chosen female is within 1 SD from the average height
qnorm(0.99,fAvg,fSd) # determine x of a female in the 99th percentile
# IQ example
## distribution of IQ scores is approximately normally distributed
## avg =  100
## sd = 15
## 10,000 people are born each year
## want to know the distribution of the person with the highest IQ
nTSim <- 1000
set.seed(1) 
hIQ <- replicate(nTSim, {
  simData <- rnorm(10000, 100, 15)    
  max(simData)    
})
hist(hIQ)
# ACT example 
set.seed(16,sample.kind = "Rounding")
actScr <- rnorm(10000, 20.9 , 5.7)
mean(actScr) 
sd(actScr)
# perfect ACT score is 36 or >
sum(actScr>=36) # how many perfect scores are there out of 10,000 simulated tests?
mean(actScr>=30) # what is the Pr of an ACT score <  or = to 30?
mean(actScr <=10)   # what is the Pr of an ACT score <  or = to 10?
# determine the value of the Pr density function over x 
x <- seq(1, 36)
fx <- dnorm(x, mean = 20.9, sd = 5.7)
plot(x, fx)
# Pr of a Z-score > than 2 sd above the mean
zScr <- (actScr - mean(actScr)) / sd(actScr)
mean(zScr > 2)
# score value corresponds to 2 sd above the mean (Z = 2 which corresponds to 97.5th percentile)?
2*sd(actScr) + mean(actScr)
qnorm(.975, mean(actScr), sd(actScr)) # determine the 97.5th percentile of normally distributed data
cdf <- sapply(1:36, function (a){ # takes value ->  produces the Pr of a score < or = to that value
  mean(actScr <= a)
})
min(which(cdf >= .95)) # min integer score such that the Pr of that score or lower is at least .95
qnorm(.95, 20.9, 5.7) # determine the expected 95th percentile, the value for which the probability of receiving that score or lower is 0.95
p <- seq(0.01, 0.99, 0.01) # quantiles for p
sampleQuantiles <- quantile(actScr, p)
names(sampleQuantiles[max(which(sampleQuantiles< 26))]) # In what percentile is a score of 26?
library(tidyverse)
library(ggplot2)
p <- seq(0.01, 0.99, 0.01)
sampleQuantiles <- quantile(actScr, p)
theoretQuantiles <- qnorm(p, 20.9, 5.7) # a corresponding set of theoretical quantiles
qplot(theoretQuantiles, sampleQuantiles) + geom_abline()
# random variables are resulting from a random process
beads <- rep(c("red", "blue"), times = c(2, 3)) #define random variable x to be 1 if blue, 0 otherwise
#the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
# quantify the uncertainty introduced by randomness -> statistical inference
# sampling models = models the random behavior of a process
# Pr distribution of a random variable = Pr of the observed value falling in any given interval
# expected value = average of many draws of a random variable
# standard error = sd of many draws of a random variable
# Distribution vs. Pr Distribution
# random variable has a distribution function <- theoretical concept
# CLT -> when the number of independent draws-- also called sample size-- is large,the Pr distribution of the sum of these draws is approximately normal
## If  known that the distribution of a list of numbers is approximated by the normal distribution, all needed to be described  are the average and the sd 
## same applies to probability distribution -> If a random variable has a Pr distribution that is approximated with the normal distribution, then describe the Pr distribution through average(expected value) and the sd(standard error)
### E of X = mu ; E = number of draws times the average of the numbers in the urn
### a random variable will vary around an E in a way that, if one takes the average of many, many draws, the average of the draws will approximate the E
### SE gives an idea of the size of the variation around the E
### if independent draws, then SE = square root of the number of draws times the sd of the numbers in the urn
### SE = the typical difference between a random variable and its expectation 
# An American roulette
## Note:Each red and black pocket is associated with a number from 1 to 36. The two remaining green slots feature “0” and “00”.
green <- 2
black <- 18
red <- 18
pInGreen = green/(green+black+red)
# payout of $17, if one bets $1 ones
set.seed(1)
pNotInGreen <- 1- pInGreen
X <- sample(c(17,-1), 1, replace = TRUE, prob=c(pInGreen, pNotInGreen)) #the random variable `X`, one's winnings from betting on green
X
# compute the E of X
pInGreen * 17 + pNotInGreen * (-1)
# compute the SE of X
abs((17 - -1))*sqrt(pInGreen*pNotInGreen) #a single outcome after one spin of the roulette wheel
set.seed(1)
n <- 1000
S <- sum(X) # a random variable S that sums one's winnings after betting on green 1,000 times
S
# E outcome of a bet
n * (pInGreen * 17 + pNotInGreen * (-1))
# SE of the sum of 1,000 outcomes
sqrt(n) * abs((17 + 1))*sqrt(pInGreen*pNotInGreen)
# averages and proportions
# Note:  the law of averages is sometimes misinterpreted
## if one tosses a coin five times and one sees heads each time, one might hear someone argue that the next toss is probably a tail because of the law of averages
## These events - tail vs. head are independent -> the chance of a coin landing heads is 50%, regardless of the previous five
# Note: The law of averages applies only when the number of draws is very, very large, not in small samples
# how large is large CLT?
# if the probability of success is very small, one needs larger sample sizes -> Poisson distribution is needed
# American Roulette probability of winning money
pInGreen <- 2 / 38
pNotInGreen <- 1-pInGreen
n <- 100
avg <- n * (17*pInGreen + -1*pNotInGreen)
se <- sqrt(n) * (17 - -1)*sqrt(pInGreen*pNotInGreen)
1-pnorm(0,avg,se)
# American Roulette Monte Carlo simulation
pInGreen <- 2 / 38
pNotInGreen <- 1-pInGreen
n <- 100
B <- 10000 
set.seed(1)
S <- replicate(B,{
  X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(pInGreen, pNotInGreen))
  sum(X)
})
mean(S)
sd(S)
# American Roulette Monte Carlo vs CLT
# average winnings per bet
set.seed(1)
n <- 10000
pInGreen <- 2 / 38
pNotInGreen <- 1-pInGreen
X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(pInGreen, pNotInGreen))
Y <- mean(X)
Y
# American Roulette per bet expected value
# What is the expected value of Y, the average outcome per bet after betting on green 10,000 times?
pInGreen <- 2 / 38
pNotInGreen <- 1-pInGreen
Y <- pInGreen * 17 + pNotInGreen * (-1)
Y
# American Roulette per bet standard error
n <- 10000
pInGreen <- 2 / 38
pNotInGreen <- 1-pInGreen
abs((17 - (-1))*sqrt(pInGreen*pNotInGreen) / sqrt(n))# Compute the standard error of 'Y', the mean outcome per bet from 10,000 bets
# American Roulette winnings per game are positive
avg <- 17*pInGreen + -1*pNotInGreen
se <- 1/sqrt(n) * (17 - -1)*sqrt(pInGreen*pNotInGreen)
1 - pnorm(0, avg, se)
# American Roulette Monte Carlo again
n <- 10000
B <- 10000
set.seed(1)
S <- replicate(B,{  
  X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(pInGreen, pNotInGreen))
  mean(X)
})
mean(S)
sd(S)
# American Roulette comparison
mean(S>0)# What is the probability of winning more than $0 as estimated by your Monte Carlo simulation?
# SAT 
# entrance exam had a -0.25 point penalty for every incorrect answer and awarded 1 point for a correct answer
# 44 multiple-choice questions each with 5 answer choices
# Suppose one chooses answers by guessing for all questions on the test
# What is the probability of guessing correctly for one question?
p<-1/5
p
cr <- 1
# What is the expected value of points for guessing on one question?
mtk <- -0.25
mu <- cr*p+mtk*(1-p)
mu
# What is the expected score of guessing on all 44 questions?
totNQues <- 44
totNQues*mu
# What is the standard error of guessing on all 44 questions?
sigma <- sqrt(totNQues) * abs(mtk-cr) * sqrt(p*(1-p))
sigma
# use CLT to determine the Pr that a guessing student scores 8 points or higher on the test
1-pnorm(8,mu,sigma) 
# What is the Pr that a guessing student scores 8 points or higher?
set.seed(21,sample.kind = "Rounding")
B <- 10000
n <- 44
examScores <- replicate(B,{ # Monte Carlo simulation
  X <- sample(c(1, -0.25), n, replace = TRUE, prob=c(p, 1-p))
  sum(X)
})
mean(examScores>=8)# Pr of 8 or higher
# the number of multiple choice options is 4 
# there is no penalty for guessing - that is, an incorrect question gives a score of 0
cr <- 1
mtk <- 0    # no penalty for incorrect answer
p <- 0.25
n <- 44
mu <- (cr*p)+(mtk*(1-p))# expected value of 1 question
mu*n # expected value of test
# consider a range of correct answer Prs  <- seq(0.25, 0.95, 0.05) representing a range of student skills
# What is the lowest p such that the Pr of scoring over 35 exceeds 80%?
# mu = n * (a*p + b*(1-p))
# sigma = sqrt(n) * abs(b-a) * sqrt(p*(1-p))
p <- seq(0.25, 0.95, 0.05)
expectedValue <- sapply(p, function(x) {
  mu <- n* (cr*x + mtk*(1-x))
  sigma <- sqrt(n) * abs(mtk-cr) * sqrt(x*(1-x))
  1 - pnorm(35, mu, sigma)
})
# each p have a different probability
plot(p, expectedValue)
p[which(expectedValue > 0.8)]
min(p[which(expectedValue > 0.8)])
# Betting on Roulette
# a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets
# The bet pays out 6 to 1, a losing bet yields -$1 and a successful bet yields $6
# A gambler wants to know the chance of losing money if he places 500 bets on the roulette House Special
#What is the expected value of the payout for one bet?
p <- 5/38
a <- 6
b <- -1
mu <- a*p + b*(1-p)
mu
#What is the standard error of the payout for one bet?
sigma <- abs(b-a) * sqrt(p*(1-p))
sigma
#What is the expected value of the average payout over 500 bets?
n <- 500
mu
#What is the standard error of the average payout over 500 bets?
sigma/sqrt(n)
#What is the expected value of the sum of 500 bets?
mu500<- n*mu
mu500
#What is the standard error of the sum of 500 bets?
sd500 <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
sd500
#Use pnorm() with the expected value of the sum and standard error of the sum to calculate the probability of losing money over 500 bets, Pr(x<=0)
pnorm(0, mu500, sd500)
# The big short 
## Interest rates for loans are set using the Pr of loan defaults to calculate a rate that minimizes the probability of losing money
# random variable -> outcome of loans ; also, random variable -> sum of outcomes of many loans
# CLT ->  use properties of the normal distribution to calculate the interest rate needed to ensure a certain probability of losing money for a given probability of default
# to decide what interest rates we should charge
# loans = 1,000 for 180,000 this year
# lost, after adding up all the costs = $200,000 per foreclosure
# not paying = 2% 
# sampling model
# Pr of defaulting
n <- 1000
lossPerForeclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * lossPerForeclosure) #random variable 
n <- 1000
lossPerForeclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * lossPerForeclosure) #random variable 
# Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * lossPerForeclosure)
})
#plot expected loses(values)
library(tidyverse)
data.frame(lossesInMillions = losses/10^6) %>%
  ggplot(aes(lossesInMillions)) +
  geom_histogram(binwidth = 0.6, col = "black")
# CLT no need of Monte Carlo 
## tells that because losses are a sum of independent draws, its distribution is approximately normal with expected value and standard deviation 
# E and SE of the sum of 1,000 loans
n*(p*lossPerForeclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(lossPerForeclosure)*sqrt(p*(1-p))    # standard error
#  set an interest rate to guarantee that on average, one breaks even -> add quantity x to each loan, represented by draws so that the expected values equals zero 
x = - lossPerForeclosure*p/(1-p) # x= -(lp/1-p)
x # about 2% interest rate
x/180000 #On a $180,000 loan, this equals an interest rate of
# still there's a 50% chance that one will lose money
# pick an interest rate that makes it unlikely for this to happen and that is not too high so to make the clients to choose another bank 
# choose chances of losing money to be one in 100
# what does x have to be now? -> the sum, S, to have the Pr of S < than zero to be 0.01
# {lp+x(1-p)}n <- expected value of S; n = number of draws= number of lawns
#  interest rate for 1% Pr of losing money
l <- lossPerForeclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))\x
x/180000    # interest rate
lossPerForeclosure*p + x*(1-p)    # expected value of the profit per loan
n*(lossPerForeclosure*p + x*(1-p)) # expected value of the profit over n loans
#Monte Carlo simulation for 1% probability of losing money
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, lossPerForeclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money
# Big short
# One points out that since the bank is making about $2,000 per loan, then one should give out more loans, not just n?
# Another explains that finding those n clients is hard, needed is a group that is predictable, and that keeps the chances of defaults low
# Then is pointed out that even if the Pr of default is high, as long as one's expected value is positive, one can minimize the chances of losing money by increasing n, the number of loans, and relying on the law of large numbers.
# It is claimed that even if the default rate is twice as high, say 4%, if the rate is set just a bit higher so that this happens, one will get a positive expected valuee
# by making n large, one claims to minimize the standard error of our per-loan profit
# Not the case -> for this rule to hold, the X's must be independent draws -> The fact that one person defaults must be independent of other people defaulting
# E value with higher default and interest rate
p <- .04
lossPerForeclosure <- -200000
r <- 0.05
x <- r*180000
lossPerForeclosure*p + x*(1-p)
# Calculate number of loans for desired Pr of losing money
z <- qnorm(0.01)
l <- lossPerForeclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(lossPerForeclosure*p + x * (1-p))    # expected profit over n loans
# Monte Carlo simulation with known default probability
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, lossPerForeclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)
# The case -> more realistic simulation
# a global event that affects everybody
# One will assume that with a 50-50 chance, all the probabilities go up or down slightly to somewhere between 0.03 and 0.05
# But it happens to everybody at once, not just one person -> these draws are not independent
# Monte Carlo simulation with unknown default probability
p <- 0.04 
x <- 0.05*180000
profit <- replicate(B, {
  newP <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, lossPerForeclosure), n, 
                   prob=c(1-newP, newP), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million
# Bank earnings
# a bank that gives out 10,000 loans
# default rate is 0.03 
# loose $200,000 in each foreclosure
# Create a random variable S that contains the earnings of your bank
n <- 10000
lossPerForeclosure <- -200000
pDefault <- 0.03
set.seed(1) # sure your answer matches the expected result after random sampling
defaults <- sample( c(0,1), n, replace = TRUE, prob=c(1-pDefault, pDefault))#the default outcomes of `n` loans
S <- sum(defaults * lossPerForeclosure) # the total amount of money lost across all foreclosures
S
# Bank earnings Monte Carlo
n <- 10000
lossPerForeclosure <- -200000
p <- 0.03 # the probability of default
set.seed(1)
B <- 10000 # the number of times we want the simulation to run
S <- replicate(B, { # generate a list of summed losses for 'n' loans
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * lossPerForeclosure)
})
hist(S) # a histogram of 'S'
#Bank earnings E value of  S
## the sum of losses over 10,000 loans
## assume a bank makes no money if the loan is paid
n <- 10000
lossPerForeclosure <- -200000
p <- 0.03
n*(p*lossPerForeclosure + (1-p)*0) # expected loss due to default out of 10,000 loans
#Bank earnings SE
n <- 10000
lossPerForeclosure <- -200000
p <- 0.03
sqrt(n) * abs(lossPerForeclosure) * sqrt(p*(1 - p))#the SE of the sum of 10,000 loans
# Bank earnings interest rate - 1
# Assume one gives out loans for $180,000
# How much money one  will need to make when people pay their loans so that one's net loss is $0?
# In other words, what interest rate do we need to charge in order to not lose money?
lossPerForeclosure <- -200000
p <- 0.03
x <- -(lossPerForeclosure*p) / (1 - p) # total amount necessary to have an expected outcome of $0
x/180000 # Convert `x` to a rate, given that the loan amount is $180,000
# Note: one still loses money 50% of the time
#Bank earnings interest rate - 2
n <- 10000
lossPerForeclosure <- -200000
pDefault <- 0.03
z <- qnorm(0.05) # a variable `z`
x <- -loss_per_foreclosure*( n*p_default - z*sqrt(n*p_default*(1 - p_default)))/ ( n*(1 - p_default) + z*sqrt(n*p_default*(1 - p_default)))
x / 180000 # Convert `x` to an interest rate, given that the loan amount is $180,000
# Big Short
# insurance company offers a one-year term life insurance policy that pays $150,000 in the event of death within one year
# premium (annual cost) for this policy for a 50 year old female is $1,150
# in the event of a claim, the company forfeits the premium and loses a total of $150,000
# if there is no claim the company gains the premium amount of $1,150
# the company plans to sell 1,000 policies to this demographic
library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)
# Insurance rates, part 1
# death_prob = estimated probability of death within 1 year 
#  determine the death probability of a 50 year old female, p
pF <- death_prob %>% 
  filter(age==50, sex=="Female")%>%
  pull(prob)
pF
plotPF <- ggplot(death_prob, aes(x=age, y=prob)) +
  geom_line(aes(col=sex)) +
  theme(panel.background = element_blank()) +
  xlab("age") +
  ylab("probability") +
  ggtitle("Death probability male vs. female by age") 
plotPF
# filter by age
age25 <- death_prob %>%
  filter(age <= 25)
age50 <- death_prob %>%
  filter(age > 25 & age <=50)
age75 <- death_prob %>%
  filter(age > 50 & age <=75)
age100 <- death_prob %>%
  filter(age > 75 & age <=100)
# plot filtered
library(gridExtra)
manual_color <- c("Male" = "#e50914", "Female" = "#34a853")
plotProb1 <- ggplot(age25, aes(x=age, y=prob)) +
  geom_line(aes(col=sex)) + 
  theme(axis.text = element_text(face = "bold"), panel.background = element_blank()) +
  scale_color_manual(values = manual_color) +
  xlab("age") +
  ylab("probability") +
  ggtitle("Death probability - age<=25") 
plotProb2 <- ggplot(age50, aes(x=age, y=prob)) +
  geom_line(aes(col=sex)) +
  theme(axis.text = element_text(face = "bold"), panel.background = element_blank()) +
  scale_color_manual(values = manual_color) +
  xlab("age") +
  ylab("probability") +
  ggtitle("Death probability - age: 26 to 50") 
# subplot
grid.arrange(plotProb1, plotProb2, ncol=1)
plotProb3 <- ggplot(age75, aes(x=age, y=prob)) +
  geom_line(aes(col=sex)) +
  scale_color_manual(values = manual_color) +
  theme(axis.text = element_text(face = "bold"), panel.background = element_blank()) +
  xlab("age") +
  ylab("probability") +
  ggtitle("Death probability - age: 51 to 75") 
plotProb4 <- ggplot(age100, aes(x=age, y=prob)) +
  geom_line(aes(col=sex)) +
  scale_color_manual(values = manual_color) +
  theme(axis.text = element_text(face = "bold"), panel.background = element_blank()) +
  # theme_minimal(base_size = 12) +
  xlab("age") +
  ylab("probability") +
  ggtitle("Death probability - age: 76 to 100") 
grid.arrange(plotProb3, plotProb4, ncol=1)
# Note: the loss = -$150,000 ;  gain if the policy holder remains alive = $1,150
# E value of the company's net profit on one policy for a 50 year old female
-150000*p+1150*(1-p)
abs(-150000-1150)*sqrt(p*(1-p))# SE of the profit on one policy for a 50 year old female
1000*(-150000*p+1150*(1-p))# E of the company's profit over all 1,000 policies for 50 year old females
sqrt(1000)*(abs(-150000-1150)*sqrt(p*(1-p)))# SE of the sum of the expected value over all 1,000 policies for 50 year old females
pnorm(0,1000*(-150000*p+1150*(1-p)), sqrt(1000)*(abs(-150000-1150)*sqrt(p*(1-p))))# CLT to calculate the Pr that the insurance company loses money on this set of 1,000 policies.
p <- death_prob%>% #  determine the Pr of death within one year for a 50 year old male
  filter(age==50,sex=="Male")%>%pull(prob)
p
# wants  E profits from 1,000 50 year old males with $150,000 life insurance policies to be $700,000
# E[S] = mu_S = 700000
# n = 1000
# p = death prob of 50y Males
# a = 150000 loss
# b = premium to solve
# E[S] = n*(ap +b(1-p))
# b = ((E[S]/n)-ap/(1-p
# What premium should be charged?
b<- ((700000/1000)- - 150000*p)/(1-p)
b
serr <- sqrt(1000)*abs(b--150000)*sqrt(p*(1-p))# SE  of the sum of 1,000 premiums
serr
pnorm(0,1000*(-150000*p+b*(1-p)),serr) # Pr of losing money on a series of 1,000 policies to 50 year old males
   
# Life insurance rates are calculated using mortality statistics from the recent past
# a scenario in which a lethal pandemic disease increases the Pr of death within 1 year for a 50 year old to .015
# E value of the company's profits over 1,000 policies
mu <- 1000 * (-150000*0.015+ 1150*(1-0.015))
mu
serr <- sqrt(1000)*abs(-150000 - 1150)*sqrt(0.015*(1-0.015))# SE of the E value of the company's profits over 1,000 policies
serr
pnorm(0,mu,serr)# Pr of the company losing money
# can afford to sustain only one-time losses of $1 million
pnorm(-1000000,mu, serr)# Pr of losing more than $1 million
# death Pr 
# lowest death Pr for which the chance of losing money exceeds 90%
p <- seq(.01, .03, .001)
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000
pLoseMoney <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(0, exp_val, se)
})

data.frame(p, pLoseMoney) %>%
  filter(pLoseMoney > 0.9) %>%
  pull(p) %>%
  min()
# death Pr p <- seq(.01, .03, .0025)
# lowest death Pr for which the chance of losing over $1 million exceeds 90%
pLoseMillion <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(-1*10^6, exp_val, se)
})
data.frame(p, pLoseMillion) %>%
  filter(pLoseMillion > 0.9) %>%
  pull(p) %>%
  min()
# a sampling model for simulating the total profit over 1,000 loans with Pr of claim p_loss = .015, loss of -$150,000 on a claim, and profit of $1,150 when there is no claim
# reported profit (or loss) in millions (that is, divided by 10^6)
set.seed(25)
n <- 1000
pLoss <- 0.015
X <- sample(c(0,1),n, replace = TRUE, prob=c((1-pLoss),pLoss)) 
loss<- -150000*sum(X==1)/10^6
profit<- 1150*sum(X==0)/10^6
loss+profit
# Monte Carlo simulation of the sampling model with 10,000 replicates to simulate the range of profits/losses over 1,000 loans
# observed Pr of losing $1 million or more
set.seed(27)
S <- replicate(10000,
               {
                 X <- sample(c(0,1),1000, replace = TRUE, prob=c((1-0.015),0.015)) 
                 loss<- -150000*sum(X==1)/10^6
                 profit<- 1150*sum(X==0)/10^6
                 loss+profit
                 
               })
sum(S<=-1)/10000
# massive demand for life insurance due to the pandemic
# need to find a premium cost for which the probability of losing money is under 5%
# assume the death rate stays stable at  p = 0.015
# calculate the premium required for a 5% chance of losing money given n = 1000 loans probability of death p = 0.015, and loss per claim l=-150000
p <- 0.015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*(n*p-z*sqrt(n*p*(1-p)))/(n*(1-p)+z*sqrt(n*p*(1-p)))
x
l*p + x*(1-p) # E profit per policy at this rate
n*(l*p + x*(1-p)) # E profit over 1,000 policies
# Monte Carlo simulation 
# determine the Pr of losing money on 
# Set the seed to 28 before running your simulation
set.seed(28)
S <- replicate(10000, {
  X <- sample(c(0,1), n, replace = TRUE, prob=c((1-p), p))
  loss <- l*sum(X==1)/10^6 
  profit <- x*sum(X==0)/10^6
  loss+profit
})
sum(S<0)/10000
# Note: cannot predict whether the pandemic death rate will stay stable
# randomly changes p by adding a value between -0.01 and 0.01 with sample(seq(-0.01,0.01,length = 100),1)
# uses the new random p to generate a sample of n = 1000 policies with premium x and loss per claim  l = -150000
# returns profit over n policies(sum of random variable )
# the outcome should be a vector of B total profits 
set.seed(29,sample.kind="Rounding")
p <- 0.015
n <- 1000
l <- -150000
B <- 10000
x <- 3268
X <- replicate(B,{
  next_p <- p+sample(seq(-0.01, 0.01, length=100),1)
  Y <- sample(c(x,l),n,replace=TRUE,prob=c(1-next_p,next_p))
  sum(Y)
})
mean(X) # E value over 1,000 policies
sum(X<0)/B # Pr of losing money
mean(X < -1000000)# Pr of losing more than $1 million
# Inference
# sampling model parameters and estimates
library(tidyverse)
library(dslabs)
take_poll(25)    # draw 25 beads - observation # shows a random draw

