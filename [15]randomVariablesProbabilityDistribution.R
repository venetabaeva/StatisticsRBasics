library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv" 
download(url, destfile=filename)
library(dplyr)
filename <- "femaleMiceWeights.csv"
dat<- read.csv(filename, header = TRUE,sep = ",")
colnames(dat)
chowVals <- filter(dat, Diet=="chow") %>% dplyr::select(Bodyweight) %>% unlist
class( chowVals )  
View(chowVals)
hfVals <- filter(dat, Diet=="hf") %>%dplyr:: select(Bodyweight) %>% unlist
class( hfVals ) 
mean(chowVals)
mean(hfVals)
plot(chowVals,hfVals,xlab = "chowVals", ylab="hfVals") 
popDat<- read.csv("femaleControlsPopulation.csv") #population 
popDat<- unlist(popDat)#turn into numeric vector
#compare over and over again with different sample
mean(sample(popDat,12))#random sample; get different random sample of 12 ;random variable of random sample
mean(chowVals)  
mean(hfVals)
#random varibales
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
RNGkind("Mersenne-Twister", "Inversion", "Rejection") # random number generator
mean(x)
set.seed(1)#produces the same sample again and again = generate same set  at each time
sample<- sample(x,5)# if I use the sample() function immediately after setting a seed, I will always get the same sample.
abs(mean(sample)-mean(x)) #absolute value  of the difference between the average of the sample and the average of all the values?
set.seed(5) #sets the starting number used to generate a sequence of random numbers
sample<- sample(x,5)
abs(mean(sample)-mean(x))
#the average of the samples is a random variable =>[inferentialStat] needed

