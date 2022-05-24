#[help]
?read.csv
#download
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv" 
download(url, destfile=filename)
#[DFF] access
filename <- "femaleMiceWeights.csv"
View(filename)
dat<- read.csv(filename, header = TRUE,sep = ",") 
View(dat)
#[clmn] exact name 
colnames(dat)
#[[]]extract specific rows and columns of the table
dat[12,2]
#[$] extract a column from a table;  return it as a vector; return specific row
dat$Bodyweight[11]
#[$] extract a column from a table;  return it as a vector; return specific row
weights <- micewt$Bodyweight
weights[11]
#[lngthF]  return  number of elements in a vector
lengthDat <- length(dat$Diet)
lengthDat
#[lngthF]  return  number of elements in a vector
length(dat$Diet)
#[clmn] filter value; [clmn] average
hfDiet <- dat[dat$Diet == "hf",]
mean(hfDiet$Bodyweight)
#[clmn] filter value; [clmn] average
bodyWeight <- dat$Bodyweight
mean(weights[13:24])
#[smpl] take a random sample of size 
?sample
sample(hfDiet$Bodyweight,1)
#[smpl] take a random sample of size 
set.seed(1)
sampleHFDietBodyWeight<- sample(13:24,1)
dat$Bodyweight[sampleHFDietBodyWeight]
#[dplyr]
help(package = dplyr) 
#[fltr] filter
filename <- "femaleMiceWeights.csv"
dat<- read.csv(filename)
View(dat)
library(dplyr) 
controls <- filter(dat, Diet=="chow") #keep only the ones with chow diet
View(controls) 
#[slct] select
controls<- select(controls, Bodyweight) 
#[vctr] make DF into numeric vector
unlist(controls)
#[ppng] piping
controls<- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
#download
library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)
dat<- read.csv("msleep_ggplot2.csv")
class(dat)#Read in the msleep_ggplot2.csv file with the function read.csv() and use the function class() to determine what type of object is returned.
library(dplyr) 
head(dat)
primates <- filter(dat, order =="Primates")
nrow(primates)# How many animals in the table are primates?
class(primates)#What is the class of the object you obtain after subsetting the table to only include primates?
View(primates)
primatesSleepTotal <-select(primates, sleep_total)
class(primatesSleepTotal)
primates <- filter(dat, order =="Primates") %>% select(sleep_total)%>% unlist 
mean(primates) #requires a vector 
class(primates)
primates <- filter(dat, order =="Primates") %>% select(sleep_total)%>% summarise(mean(sleep_total))
primates
