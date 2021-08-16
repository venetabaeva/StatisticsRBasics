# do is do Monte Carlo simulation to test out the t-distribution approximation, how well it works
set.seed(1)
library(rafalib)
dat<-read.csv("mice_pheno.csv")
controlPop<- read.csv("femaleControlsPopulation.csv")
controlPop <- unlist(controlPop)
tTestGenerator <- function(n){ # We're going to call it ttestgenerator, that for any given sample size n, it takes a random sample from the control population for cases and controls
  cases <- sample(controlPop,n)
  controls <- sample(controlPop,n)
  tstat <- (mean(cases)- mean(controls))/ 
    sqrt(var(cases)/n + var(controls)/n)
  return(tstat)
}
tTests <- replicate(1000,tTestGenerator(10)) #generate 1,000 t-statistics with a sample size of 10 = Monte Carlo Simulation 
hist(tTests)
qqnorm(tTests)#We can see how well the normal approximation works => Central limit theorem works pretty well for this particular dataset when the sample size is 10
abline(0,1)
tTests <- replicate(1000,tTestGenerator(3)) 
hist(tTests)
qqnorm(tTests)# Not well approximating 
abline(0,1)
#- how well the t-distribution approximates the results,  using a qq-plot,plotting q-values from the t-distribution against the Monte Carlo values
ps<- (seq(0,999)+0.5)/1000
qqplot(qt(ps,df=2*3-2),tTests,xlim=c(-6,6),ylim=c(-6,6)) # better approximation 
abline(0,1)
qqnorm(controlPop)
# non having an access to the population =>  know mean, standard deviaiton and generate a control population using a Monte Carlo-- parametric Monte Carlo
controls<-(rnorm(5000,mean=24,sd=3.5))# rnorm= creating a population of 5000
tTestGenerator <- function(n,mean=24,sd=3.5){ # We're going to call it ttestgenerator, that for any given sample size n, it takes a random sample from the control population for cases and controls
  cases <- rnorm(n,mean,sd)
  controls <- rnorm(n,mean,sd)
  tstat <- (mean(cases)- mean(controls))/ 
    sqrt(var(cases)/n + var(controls)/n)
  return(tstat)
}
tTests <- replicate(1000,tTestGenerator(10)) 
hist(tTests)
qqnorm(tTests)# Not well approximating 
abline(0,1)
#derived the distribution of the t-statistic when the sample comes from a normal distribution; to check the results; start by creating an outcome
set.seed(1)
X <- rnorm(5) # generate a random sample of size 5 from standard normal distribution ; a random number generator for normally distributed data
t<- (sqrt(5)*mean(X)/sd(X)) #a random variable, follows a t-distribution with  N-1 degrees of freedom
t
#Monte Carlo simulations can be used to check that a random variable, follows a t-distribution with  N-1 degrees of freedom
set.seed(1)
tTestGenerator <- function(n) {
  X <- rnorm(n)
  tstat <- (sqrt(n)*mean(X))/sd(X) 
  return(tstat)
}

ttests <- replicate(1000, tTestGenerator(5))
mean(ttests>=2)
#theoretical prediction: 1-pt(2,df=4) = Monte Carlo simulations  => check several such quantiles using the qqplot function
#obtain quantiles for the t-distribution
Ns<- seq(5,30,5)
B<-1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*mean(X)/sd(X)
  })
ps <- seq(1/(B+1), 1-1/(B+1),len=B)#generate percentiles from just above 0 to just below 1
qqplot(qt(ps,df=N-1),ts,main=N,# compute the quantiles ;#compare these theoretical quantiles to those obtained in the Monte Carlo simulation
       xlab="Theoretical", ylab="Observed",
       xlim=LIM, ylim=LIM)
abline(0,1)
}
#The approximations are spot on for all sample sizes.
#Use Monte Carlo simulation to corroborate that the t-statistic comparing two means and obtained with normally distributed (mean 0 and sd) data follows a t-distribution.
Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B,{
    x <- rnorm(N)
    y <- rnorm(N)
    t.test(x,y, var.equal = TRUE)$stat
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=2*N-2),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
}  
#The approximations are spot on for all sample sizes.
#Is the following statement true or false? If instead of generating the sample with X=rnorm(15) we generate it with binary data (either positive or negative 1 with probability 0.5) X =sample(c(-1,1), 15, replace=TRUE) then the t-statistic
set.seed(1)
N <- 15
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)
#The population data is not normal thus the theory does not apply.
#Is the following statement true or false ? If instead of generating the sample with X=rnorm(N) with N=1000, we generate the data with binary data X= sample(c(-1,1), N, replace=TRUE), then the t-statistic sqrt(N)*mean(X)/sd(X) is approximated by a t-distribution with 999 degrees of freedom.
set.seed(1)
N <- 1000
B <- 10000
tstats <- replicate(B,{
  X <-  sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
qqnorm(tstats)
abline(0,1)
#with N=1000, CLT and t-statistic is approximated with normal 0,1=> t-distribution with df=999 and normal are the same
#can derive approximation of the distribution of the sample average or the t-statistic theoretically
#interested in the distribution of a statistic for which a theoretical approximation is not immediately obvious
#the sample median as an example
#Monte Carlo for ; determine which of the following best approximates the median of a sample taken from normally distributed population with mean 0 and standard deviation 1.
set.seed(1)
Ns <- seq(5,45,5)
library(rafalib)
mypar(3,3)
for(N in Ns){
  medians <- replicate(10000, median ( rnorm(N) ) )
  title <- paste("N=",N,", avg=",round( mean(medians), 2) , ", sd*sqrt(N)=", round( sd(medians)*sqrt(N),2) )
  qqnorm(medians, main = title )
  qqline(medians)
}
#The sample median is approximately normal with mean 0 and SD larger than 1N√
#permutation tests
#a situation in which none of the standard mathematical statistical approximations apply. We have computed a summary statistic, such as the difference in mean, but do not have a useful approximation, such as that provided by the CLT. In practice, we do not have access to all values in the population so we can’t perform a simulation as done above
#Permutation tests take advantage of the fact that if we randomly shuffle the cases and control labels, then the null is true. So we shuffle the cases and control labels and assume that the ensuing distribution approximates the null distribution. Here is how we generate a null distribution by shuffling the data 1,000 times:

dat=read.csv("femaleMiceWeights.csv")
library(dplyr)
control <- filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat,Diet=="hf") %>% select(Bodyweight) %>% unlist
obsdiff <- mean(treatment)-mean(control)
N <- 12
avgdiff <- replicate(1000, {
  all <- sample(c(control,treatment))
  newcontrols <- all[1:N]
  newtreatments <- all[(N+1):(2*N)]
  return(mean(newtreatments) - mean(newcontrols))
})
hist(avgdiff) #of difference between averages from permutations
abline(v=obsdiff, col="red", lwd=2)
(sum(abs(avgdiff) > abs(obsdiff)) + 1) / (length(avgdiff) + 1) #How many of the null means are bigger than the observed value? That proportion would be the p-value for the null. We add a 1 to the numerator and denominator to account for misestimation of the p-value
#permutations exercises
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)
#question is whether this observed difference is statistically significant.
#do not want to rely on the assumptions needed for the normal or t-distribution approximations to hold, so instead we will use permutations
#reshuffle the data and recompute the mean
#create one permuted sample
library(dplyr)
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)
#question is whether this observed difference is statistically significant
#We do not want to rely on the assumptions needed for the normal or t-distribution approximations to hold, so instead we will use permutations. We will reshuffle the data and recompute the mean. We can create one permuted sample with the following code
dat <- c(smokers,nonsmokers)
shuffle <- sample( dat )
smokersstar <- shuffle[1:N]
nonsmokersstar <- shuffle[(N+1):(2*N)]
mean(smokersstar)-mean(nonsmokersstar) #is one observation from the null distribution we will construct
#Set the seed at 1, and then repeat the permutation 1,000 times to create a null distribution. What is the permutation derived p-value for our observation?
set.seed(1)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean(smokersstar)-mean(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 ) 
#median instead of mean 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- median(smokers) - median(nonsmokers)
dat <- c(smokers,nonsmokers)
shuffle <- sample( dat )
smokersstar <- shuffle[1:N]
nonsmokersstar <- shuffle[(N+1):(2*N)]
median(smokersstar)-median(nonsmokersstar) 
set.seed(1)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  median(smokersstar)-median(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 ) 
#Association Tests
#she could tell if the milk was poured before or after the tea
#3/4 of time correct answer
#Do we believe that she has a special ability?
#try to answer with statistical inference.
#hypothesis testing = could this happen by chance
#going to quantify the answer to the question, could this happen by chance?
#statistic:summary of the data and probability distribution = hypothesis test 
# perform what is called a chi-square test => take the differences between the expected and observed outcomes
# square them and we divide them by expected squared=> then we can prove that has an asymptotic distributionof a chi-square
# the more data you have,  more unlikely to see differences-- big differences, as big as the one we seen by chance
# more data, the variability due to randomnessdecreases with respect to the real differences
#how to generate the table from data which is in the form of a dataframe, so that you can then perform an association test to see if two columns have an enrichment (or depletion) of shared occurences
d = read.csv("assoctest.csv")
str(d)
#Compute the Chi-square test for the association of genotype with case/control status 
#Examine the table to see if it looks enriched for association by eye.
library(dplyr)
library(rafalib)
d <- read.csv("assoctest.csv")
tab <- table(d)
chisq.test(tab)
fisher.test(tab)
# p- value can be computed -> pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
#a simpler way to calculate the probability that a t-value under the null could have a larger absolute value than 
2*pnorm(-abs(tval))
# If increase the confidence level, the confidence interval will increase
#If you decrease the sample size, the confidence interval will increase
#a way to decrease Type I errors -> Use a lower alpha level not by Perform the study multiple times
#The null distribution created with permutations will have   larger   tails compared to the actual null distribution. This is why permutations result in conservative p-values.
# if there is hidden structure in your data (such that samples are not independent), then permutation tests can result in estimated null distributions that   underestimate   the size of tails because the permutations may destroy the existing structure in the original data.






