library(dplyr)
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
x <- c(60,50 ,40, 30, 20)
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

dfAlc["aconsum"]
dfAlc[["aconsum"]]
dfAlc$aconsum
aconsum <- dfAlc$aconsum
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
iMin<-which.max(dfAlc$aconsum)
iMin
dfAlc$abbrv[iMin]

library(dslabs)
library(tidyverse)
abbCountry <- dfAlc$abbrv 
suicidesPer100 <- dfAlc$suicideper100
urbanRT <- dfAlc$urbanrt
ranksAConsumption <- rank(dfAlc$aconsum,na.last = NA)
i <- order(dfAlc$aconsum)
df<- data.frame(country = abbCountry[i], suicide = suicidesPer100[i], rank = ranksAConsumption[i],urbanrate = urbanRT[i])
df %>%
  ggplot(aes(urbanrate, suicide, label=country,color=rank)) +
  geom_label()

library(dslabs)
library(tidyverse)
dfAlc %>%
  ggplot(aes(urbanrt, employrt, label=abbrv, color=aconsum)) +
  geom_label()






