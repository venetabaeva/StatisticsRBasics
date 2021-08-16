#t-test
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
set.seed(1)
dat.ns<- sample(bwt.nonsmoke,25) 
dat.s <- sample(bwt.smoke,25) 
tval <- t.test(dat.ns,dat.s)$statistic
tval
#using a t-statistics because we know that in situations where the null hypothesis is true,and the sample size is relatively large, this t-value will have an approximate standard normal distribution
#know the distribution of the t-value under the null, we can quantitatively determine how unusual the observed t-value would be if the null hypothesis were true
#examine the probability a t-statistic that actually does follow the null hypothesis would have larger absolute value than the absolute value of the t-value we just observed -- this is called a two-sided test.
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
pval #incomplete #with very large sample sizes, scientifically insignificant differences between two groups can lead to small p-values
#Confidence intervals are more informative as they include the estimate itself
#confidence interval 
set.seed(1)
chowPop <- read.csv("femaleControlsPopulation.csv")
chowPop <- unlist(chowPop)
mu_chow<- mean(chowPop)
print(mu_chow)
N<-30
chow<- sample(chowPop,N)
print(mean(chow))
se<- sd(chow)/sqrt(N)
print(se)
pnorm(2)-pnorm(-2)
#construct a random variable is normally distributed, has mean 0 and standard deviation 1;assume that the sample average is normally distributed.
Q <- qnorm(1- 0.05/2)
interval <- c(mean(chow)-Q*se, mean(chow)+Q*se )
interval
interval[1] < mu_chow & interval[2] > mu_chows
library(rafalib)
B <- 250
mypar()
plot(mean(chowPop)+c(-7,7),c(1,1),type="n",
     xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPop))
for (i in 1:B) {
  chow <- sample(chowPop,N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
  covered <- 
    mean(chowPop) <= interval[2] & mean(chowPop) >= interval[1]
  color <- ifelse(covered,1,2)
  lines(interval, c(i,i),col=color)
}
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
N<-25
set.seed(1)
dat.ns<- sample(bwt.nonsmoke,N) 
dat.s <- sample(bwt.smoke,N) 
qt(0.995,2*N-2)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )#If instead of CLT, we use the t-distribution approximation, what do we add and subtract to obtain a 99% confidence interval (use 2*N-2 degrees of freedom)?
#determine how small the p-value needs to be to reject the null by deciding how often we would be willing to mistakenly reject the null hypothesis
#choose some small value  (in most disciplines the conventional choice is  ) and reject the null hypothesis if the p-value is less than . We call  the significance level of the test
#event of rejecting the null hypothesis, when it is in fact true, a Type I error
#probability of making a Type I error, the Type I error rate, and we say that rejecting the null hypothesis when the p-value is less than , controls the Type I error rate so that it is equal to 
#Type II error rate of the test, or the probability that we fail to reject the null hypothesis when the alternative hypothesis is true
N<-5
c
dat.ns<- sample(bwt.nonsmoke,N) 
dat.s <- sample(bwt.smoke,N) 
tval <- t.test(dat.ns,dat.s)
tval
#power calculation
library(dplyr)
dat<- read.csv("mice_pheno.csv")
controlPop<- filter(dat,Sex =="F" & Diet == "chow") %>% select(Bodyweight) %>% unlist
hfPop<- filter(dat,Sex =="F" & Diet == "hf") %>% select(Bodyweight) %>% unlist
mu_hf <- mean(hfPop)
mu_control <- mean(controlPop)
print(mu_hf - mu_control)
set.seed(1)
N<- 5 
hf<- sample(hfPop,N)
control <- sample(controlPop,N)
t.test(hf,control)$p.value
#increase the sample to have p-value such to reject 
N<-12
hf<- sample(hfPop,N)
control <- sample(controlPop,N)
t.test(hf,control)$p.value
#power = helps quantifying the Type Error II 
N<-12
alpha <- 0.05
B<- 2000 #going to do, over and over again, I'm going to run t-test.
reject <- function (N, alpha =0.05){
  hf<- sample(hfPop,N)
  control <- sample(controlPop,N)
  pval <- t.test(hf,control)$p.value
}
replication <- replicate(B,reject(N))
mean(replication)
Ns <- c(5,50,5)
power<- sapply(Ns, function(N){
  rejections <- replicate(B,reject(N))
  mean(rejections)
})
plot(Ns, power, type="b")
#power calculations
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
set.seed(1)
N<-5
dat.ns<- sample(bwt.nonsmoke,N) 
dat.s <- sample(bwt.smoke,N) 
pValue <- t.test(dat.ns,dat.s)$p.value>0.05
pValue# type II error
#Set the seed at 1, then use the replicate() function to repeat the code used in the exercise above 10,000 times. What proportion of the time do we reject at the 0.05 level?
set.seed(1)
N<-5
B<- 10000
alpha =0.05
rejects <- replicate(B,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)
#Note that, not surprisingly, the power is lower than 10%. Repeat the exercise above for samples sizes of 30, 60, 90 and 120. Which of those four gives you power of about 80%?
Ns <- c(30,60,90,120)
power<- sapply(Ns, function(N){
  set.seed(1)
  rejections <- replicate(B,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.05
})
  mean(rejections)
})
Ns[which.min(abs(power-.8))]
plot(Ns, power, type="b")
#Repeat the problem above, but now require an  alpha level of 0.01. Which of those four gives you power of about 80%?
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
Ns <- c(30,60,90,120)
power<- sapply(Ns, function(N){
  set.seed(1)
  rejections <- replicate(B,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.01
  })
  mean(rejections)
})
Ns[which.min(abs(power-.8))]
plot(Ns, power, type="b")


