-----------------------------------------
#DFs components
  
library(dplyr)
library(dslabs)
library(tidyverse)

dfTest <- data.frame(names=c("A","B","C","D"),
                     num1 = c(1,2,3,4),
                     num2 = c(10,20,30,40),
                     stringsAsFactors = FALSE)
dfTest
data(heights)
dfAlc <- read.csv(
                      file = ("/Users/venetabaeva/git/repository4/gapminder.csv"),
                      header = TRUE,
                      sep = ";",
                      dec = ".")
View(dfAlc)
str(dfAlc)
tableAlcCountry <- c(dfAlc$abbrv)
table(tableAlcCountry)
country<- c("Afghanistan","Albania","Algeria","Andorra","Angola", "Antigua and Barbuda")
abbrv <- c("AF", "AL", "DZ", "AD", "AO", "AG")
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
names(abbrvCountry)<-abbrv 
summary(dfAlc)
sort(dfAlc$aconsum)
-----------------------------------------
# DFs
  
dfAlc["aconsum"]
dfAlc[["aconsum"]]
dfAlc$aconsum
aconsum <- dfAlc$aconsum
naS <- is.na(aconsum)
sum(naS)
mean(aconsum[!naS])
urbanrt <-dfAlc$urbanrt
employrt <- dfAlc$employrt
length(aconsum)
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
boxplot(aconsum~region, data = dfAlc,na.action = NULL) 
boxplot(suicidesPer100~region, data = dfAlc)
boxplot(employrt~region, data = dfAlc)
boxplot(urbanrt~region, data = dfAlc)
