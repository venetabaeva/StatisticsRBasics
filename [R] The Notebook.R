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
  
  
  
controlsXGaze<- filter(dfPonzo, stimulusNr == c(1,3,13,15,25,27)) %>% dplyr::select(xGaze) %>% unlist
controlsYGaze<- filter(dfPonzo, stimulusNr == c(1,3,13,15,25,27)) %>% dplyr::select(yGaze) %>% unlist
longUpXGaze<- filter(dfPonzo, stimulusNr == c(5,7,17,19,29,31)) %>% dplyr::select(xGaze) %>% unlist 
longUpYGaze<- filter(dfPonzo, stimulusNr == c(5,7,17,19,29,31)) %>% dplyr::select(yGaze) %>% unlist
longDownXGaze<- filter(dfPonzo, stimulusNr == c(9,11,21,23,33,35)) %>% dplyr::select(xGaze) %>% unlist
longDownYGaze<- filter(dfPonzo, stimulusNr == c(9,11,21,23,33,35)) %>% dplyr::select(yGaze) %>% unlist
------
meanCXGaze<- mean(controlsXGaze,2)
meanCYGaze<-mean(controlsYGaze,2)
meanLUXGaze<-mean(longUpXGaze,2)
meanLUYGaze<-mean(longUpYGaze,2)
meanLDXGaze<-mean(longDownXGaze,2)
meanLDYGaze<-mean(longDownYGaze,2)
------
samplexGaze<-dfPonzo$xGaze
sampleyGaze<-dfPonzo$yGaze
length(samplexGaze)
length(sampleyGaze)
round(sample(samplexGaze,40),3)
round(sample(sampleyGaze,40),3)
------
seq(floor(min(samplexGaze)),ceiling(max(samplexGaze)))
seq(floor(min(sampleyGaze)),ceiling(max(sampleyGaze)))
------
mean(sample(stimNREqual$xGaze,40),2)#random sample; get different random sample of 40 ;random variable of random sample
mean(controlsXGaze,2)  
mean(longUpXGaze,2)
mean(longDownXGaze,2)
------
set.seed(1)
sample<- sample(stimNREqual$xGaze,40)
abs(mean(sample)-mean(stimNREqual$xGaze))
set.seed(5)
sample<- sample(stimNREqual$xGaze,40)
abs(mean(sample)-mean(stimNREqual$xGaze))
------
obsCLU<- mean(longUpXGaze) - mean(controlsXGaze)
obsCLD<- mean(longDownXGaze) - mean(controlsXGaze)
pop <- stimNREqual$xGaze
pop<- unlist(pop)

head(pop)
set.seed(1)
n<-1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  POP <- sample(pop,5)
  averages5[i]<- mean(POP)
}
------
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  POP <- sample(pop,50)
  averages50[i]<- mean(POP)
}
par(mfrow = c(2,1))
hist(averages5)
hist(averages50)
------
mu <- mean(pop) 
sigma <- sd(pop)
pnorm(0.600, mu, sigma) - pnorm(0.400,mu,sigma)  
mu <- mean(pop) 
sigma <- sd(pop)
pnorm(0.600, mu, sigma) - pnorm(0.300,mu,sigma)  
mu <- mean(pop) 
sigma <- sd(pop)
pnorm(0.600, mu, sigma) - pnorm(0.200,mu,sigma) 
mu <- mean(pop) 
sigma <- sd(pop)
pnorm(0.700, mu, sigma) - pnorm(0.400,mu,sigma) 
------

library(dplyr)
controlsMXGaze<- filter(dfPonzo, respondentNr==c(8,2), stimulusNr == c(1,3,13,15,25,27))
controlsMXGaze <- as.vector(controlsMXGaze$xGaze,mode = "any") 
mcontrolsMXGaze<-mean(controlsMXGaze)
library(rafalib)
popsd(controlsMXGaze)
set.seed(1) 
XcontrolsMXGaze <- sample(controlsMXGaze,10) 
McontrolsMXGaze<-mean(XcontrolsMXGaze) 
library(dplyr)
longUpMXGaze<- filter(dfPonzo,respondentNr==c(8,2) & stimulusNr == c(5,7,17,19,29,31)) 
longUpMXGaze <-as.vector(longUpMXGaze$xGaze,mode = "any")
mlongUpMXGaze<-mean(longUpMXGaze)
library(rafalib)
popsd(longUpMXGaze)
set.seed(1)
YlongUpMXGaze <- sample(longUpMXGaze,10) 
MlongUpMXGaze<-mean(YlongUpMXGaze)  
abs( ( mlongUpMXGaze - mcontrolsMXGaze ) - ( MlongUpMXGaze - McontrolsMXGaze ) )

-----------------------------------------
#DFs visualization 
  
------
abbCountry <- dfAlc$abbrv 
suicidesPer100 <- dfAlc$suicideper100
urbanRT <- dfAlc$urbanrt
region <- dfAlc$region
ranksAConsumption <- rank(dfAlc$aconsum,na.last = NA)
i <- order(dfAlc$aconsum)
df<- data.frame(country = abbCountry[i], suicide = suicidesPer100[i], rank = ranksAConsumption[i],urbanrate = urbanRT[i],region = region[i])
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
------
hist(samplexGaze,freq = TRUE, breaks = c(0.00,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.00,1.05,1.10,1.15,1.20),main="xGaze Position", xlab="xGaze position in %")
hist(samplexGaze,freq = FALSE, breaks = c(0.00,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.00,1.05,1.10,1.15,1.20),main="xGaze Position", xlab="xGaze position in %")
hist(sampleyGaze,freq = TRUE, breaks = c(0.00,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.00,1.05,1.10,1.15,1.20),main="yGaze Position", xlab="yGaze position in %")
hist(sampleyGaze,freq = FALSE, breaks = c(0.00,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.00,1.05,1.10,1.15,1.20),main="yGaze Position", xlab="yGaze position in %")
------
samplexGazeS<- seq(floor(min(samplexGaze)),ceiling(max(samplexGaze)),0.1)
plot(samplexGazeS,ecdf(samplexGaze)(samplexGazeS),type="l",xlim=c(0.0,2.0),
     xlab="xGaze position in %",ylab="F(samplexGaze)")
sampleyGazeS<- seq(floor(min(sampleyGaze)),ceiling(max(sampleyGaze)),0.1)
plot(sampleyGazeS,ecdf(samplexGaze)(sampleyGazeS),type="l",xlim=c(0.0,2.0),
     xlab="yGaze position in %",ylab="F(sampleyGaze)")
------
mean(samplexGaze<50)
pnorm(50,mean(samplexGaze),sd(samplexGaze))
mean(sampleyGaze<50)
pnorm(50,mean(sampleyGaze),sd(sampleyGaze))
------
ps<- seq(0.01,0.99,0.01)
qs<- quantile(samplexGaze,ps)
normalQs <- qnorm(ps, mean(samplexGaze), sd(samplexGaze))
plot(normalQs,qs,xlab = "Normal percentiles", ylab="xGaze percentiles")
qs<- quantile(sampleyGaze,ps)
normalQs <- qnorm(ps, mean(sampleyGaze), sd(sampleyGaze))
plot(normalQs,qs,xlab = "Normal percentiles", ylab="yGaze percentiles")
------
par(mfrow = c(2,2)) 
qqnorm(samplexGaze)
qqline(samplexGaze)
qqnorm(sampleyGaze)
qqline(sampleyGaze)
------
par(mfrow = c(2,2)) 
boxplot(samplexGaze,ylab="xGaze position",ylim=c(0,1))
boxplot(sampleyGaze,ylab="yGaze position",ylim=c(0,1))
------
par(mfrow = c(1,1)) 
boxplot(split(dfPonzo$xGaze,dfPonzo$stimulusNr))
boxplot(split(dfPonzo$xGaze,dfPonzo$respondentNr))
boxplot(split(dfPonzo$yGaze,dfPonzo$stimulusNr))
boxplot(split(dfPonzo$yGaze,dfPonzo$respondentNr))
------
length(controlsXGaze)
length(longUpXGaze)
length(longDownXGaze)

controlsXGaze<- append(controlsXGaze, c(NA,NA,NA,NA))
controlsYGaze<- append(controlsYGaze, c(NA,NA,NA,NA))
longDownXGaze<- append(longDownXGaze, c(NA,NA,NA))
longDownYGaze<- append(longDownYGaze, c(NA,NA,NA))

library(ggplot2)
plot(controlsXGaze,longUpXGaze,xlab = "controls XGaze", ylab="longUp XGaze") 
plot(controlsXGaze,longDownXGaze,xlab = "controls XGaze", ylab="longDown XGaze") 
plot(controlsYGaze,longUpYGaze,xlab = "controls YGaze", ylab="longUp YGaze") 
plot(controlsYGaze,longDownYGaze,xlab = "controls YGaze", ylab="longDown YGaze") 



