-----------------------------------------
#DFs components
  
library(dplyr)
library(dslabs)
library(tidyverse)
library(downloader) 
library(UsingR)
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

stimNREqual <- dfPonzo[dfPonzo$stimulusNr == c(1,3,13,15,25,27),]
stimNREqual <- as.data.frame(stimNREqual) 
mean(stimNREqual$xGaze)



zeroRespondentXGaze <- dfPonzo$xGaze
mean(zeroRespondentXGaze[1:350])


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
select(EMEAAPACAconsum10,country,aconsum,rank)
newTable <- select(dfAlc,country,region,aconsum) 
filter(newTable,aconsum <= 10)
str(newTable)
dfAlc %>% select(country,region,aconsum) %>% filter(aconsum <= 10)
filter(dfAlc, region %in% c("EMEA", "APAC") & aconsum < 10 )%>% select(country, aconsum, rank)
dfAlc %>% mutate(aconsum, rank) %>% filter(region %in% c('EMEA','APAC') & aconsum <10) %>% select(country,aconsum,rank)
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
  
sample(stimNREqual$xGaze,1)
sample(stimNREqual$yGaze,1)
------
set.seed(1) 
------
controls <- filter(dfPonzo, stimulusNr == c(1,3,13,15,25,27))
longUp <-filter(dfPonzo, stimulusNr == c(5,7,17,19,29,31))
longDown<- filter(dfPonzo, stimulusNr == c(9,11,21,23,33,35))
head(controls)
head(longUp)
head(longDown)
------ 
controlsXGaze<- select(controls, xGaze) 
controlsYGaze<- select(controls, yGaze) 
------
unlist(controlsXGaze)
unlist(controlsYGaze)
------
controlsXGaze<- filter(dfPonzo, stimulusNr == c(1,3,13,15,25,27)) %>% select(xGaze) %>% unlist
controlsYGaze<- filter(dfPonzo, stimulusNr == c(1,3,13,15,25,27)) %>% select(yGaze) %>% unlist
------
mean(controlsXGaze)
mean(controlsYGaze)
------
controlsXGaze <- filter(dfPonzo, stimulusNr == c(1,3,13,15,25,27)) %>% select(xGaze) %>% summarise(mean(xGaze))
controlsXGaze
controlsYGaze <-filter(dfPonzo, stimulusNr == c(1,3,13,15,25,27)) %>% select(yGaze) %>% summarise(mean(yGaze))
controlsYGaze
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


