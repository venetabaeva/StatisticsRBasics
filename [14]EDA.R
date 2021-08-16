#[EDA] exploratory data analysis
library(UsingR)
x=father.son$fheight 
length(x)
round(sample(x,20),1)
#[histogram]
hist(x, breaks =  seq(floor(min(x)),ceiling(max(x))),main="Height histogram", xlab="Height in inches")# breaks = where to draw intervals; report the number of individuals for each interval
seq(floor(min(x)),ceiling(max(x)))
xs <- seq 
#[scatterplot]empirical cumulative distribution ; shows % of individuals below a threshold
xs<- seq(floor(min(x)),ceiling(max(x)),0.1)
plot(xs,ecdf(x)(xs),type="l",
xlab="Height in inches",ylab="F(x)")
View(x)
#[QQ] 
mean(x)
sd(x)
mean(x>70) # proportion of people that are taller than 70
1-pnorm(70,mean(x),sd(x))#proportion of people that are taller than 70 through normal approximation
mean(x<70)
pnorm(70,mean(x),sd(x))
mean(x<59) # check whether mean agrees with pnorm,if TRUE, then the normal distribution is very good approximation
pnorm(59,mean(x),sd(x))
# [QQ]do the check systematically
## look at the percentiles from 1% to 99%
ps<- seq(0.01,0.99,0.01)
##compute from the data what those percentiles are
qs<- quantile(x,ps)
qs
## compute same percentiles for the normal distribution; compare both by ploting and comparing the plots
normalqs <- qnorm(ps, mean(x), sd(x))
plot(normalqs,qs,xlab = "Normal percentiles", ylab="Height percentiles")
abline(0,1)#identity line
##[QQ] automatic
qqnorm(x)
qqline(x)
#[QQ] plot multiple
load("skew.RData")
dim(dat)#a 1000 x 9 dimensional matrix 'dat'
str(dat)
names(dat)
par(mfrow = c(3,3))#want a multifigure grid filled in row-by-row
for (i in 1:9) { #loop  
  x<- dat[,i]#through the columns
  qqnorm(x, xlab="quantiles",main=paste0("Col.No=",i))# display one qqnorm() plot at a time
  qqline(x)
}
#[histogram] plot multiple
par(mfrow = c(3,3))
for (i in 1:9) {
  x <- dat[,i]
  hist(x,xlab="X",main=paste0("Col.No=",i))
}
#non normally distributed data
hist(exec.pay)
qqnorm(exec.pay)
qqline(exec.pay)
#[boxplot] use for non normally distributed data
boxplot(exec.pay,ylab="10,000s of $",ylim=c(0,400))
#[boxplot]
head(InsectSprays)# DS measures the counts of insects in agricultural experimental units treated with different insecticides
boxplot(split(InsectSprays$count,InsectSprays$spray))#draw  boxplot split
boxplot(InsectSprays$count~InsectSprays$spray) #draw boxplot factor
#[boxplot][histogram]
library(dplyr)
data(nym.2002, package="UsingR")
head(nym.2002)
str(nym.2002)
males<- filter(nym.2002, gender == "Male")
females <-  filter(nym.2002, gender == "Female")
par(mfrow = c(3,3))
boxplot(males$time,females$time)#Male and females have similar right skewed distributions with the former, 20 minutes shifted to the left.
hist(females$time,xlim=c(range( nym.2002$time)))
hist(males$time,xlim=c(range( nym.2002$time)))
#[histogram]
library(dslabs)
library(dplyr)
data("heights")
head(heights)
heightsHeight<-heights$height #What distribution does this data resemble?
hist(heightsHeight)#normal



