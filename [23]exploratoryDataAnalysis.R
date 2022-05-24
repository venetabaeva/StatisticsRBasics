#two dimensional data
#heights for fathers and sons
library(UsingR)
str(father.son)
x <- father.son$fheight
y<- father.son$sheight
plot(x,y,xlab ="Father Height Inches",ylab="Son Height Inches", main=paste("correlation =",signif(cor(x,y),2)))
#correlation -> When the data is normally distributed and is two dimensional, correlation adds the extra information to describe the proportion of data in any given two dimensional interval
#use of correlation when we make predictions.
# asked to predict the height of a person who has been randomly selected from among the sons
# change the problem; know the father's height; know that it's 72, how does that change the prediction for the son that is known to have a father who is 72 inches
#We wouldn't predict the average anymore, because we know this son has a taller than average father, and therefore we expect the son to also be taller than average.
#and therefore we expect the son to also be taller than average;what is the actual prediction?
#stratify data to notice a pattern 
boxplot(split(y,round(x))) #round the father's height to the closest inch; group the son's heights by that value.
print(mean(y[round(x)==72]))
#linear relationship 
#standartize data
x=(x-mean(x))/sd(x)
y=(y-mean(y))/sd(y)
means=tapply(y,round(x*4)/4,mean) #tapplyF compute the mean of each group determinedby rounding the heights, now in standard units
means #for all the fathers who are -1.75 standard units away from the mean, the average height of those sons was this much, -0.75
fatherheights=as.numeric((names(means))) #extract the father heights, which are actually in the names of this vector; turn those into numbers 
plot(fatherheights, means,ylab="average of strata of son heights", ylim =range(fatherheights))  
abline(0,cor(x,y)) #linear ;  need now to make predictions is the slope of that line, which turns out to be the correlation
# interpretation of the correlation -> if you have two data sets that are normally distributed and they have some correlation.
##If you turn it into standard units, then the correlation is the slope of the line that you would use to predict variable y,given variable x.
##two means, the two standard deviations, and  the correlation are a very good summary
data(nym.2002, package="UsingR")
library(dplyr)
str(nym.2002)
head(nym.2002)
#create two new data frames: males and females, with the data for each gender
m<- filter(nym.2002, gender=="Male") #males, what is the Pearson correlation between age and time to finish
f<- filter(nym.2002, gender=="Female")#female, what is the Pearson correlation between age and time to finish
mAgeTime<- cor(m$age,m$time)
fAgeTime<-cor(f$age,f$time)
#without visualizing the data, we would conclude that the older we get, the slower we run marathons, regardless of gender
library(rafalib)
mypar(2,2)
plot(m$age,m$time)
group <- floor(m$age/5)*5
boxplot(m$time~group)
plot(f$age,f$time)
group <- floor(f$age/5)*5
boxplot(f$time~group)
#symmetry of Log Ratios  -> So when we ask if a gene is different in one sample versus the other,this is typically quantified with a ratio.
#How many times bigger is the expression in this sample compared to this other sample using ratios
#mathematical relationship-> the log of ratios is the difference of logs, which is the negative of this difference of logs,which is, then, the negative of the log ratio of the reciproca
#multiplicative changes are symmetric around 0 when we are on the logarithmic scale = 1/2 times a number x, and 2 times a number x, are equally far away from x
time = sort(nym.2002$time)
#What is the fastest time divided by the median time?
min(time)/median(time)
#What is the slowest time divided by the median time?
max(time)/median(time)
#plots to avoid: 
## humans aren't very good at determining area-- relative area -> pie chart without % ; solution numbers without pie chart 
## instead of pie, use bar plot
##box plot and antena-> better points for spread of data with antenas and outlyiers to be seen 
##actual scale -> better log scale 
##comparing pairs of numbers with two box plots -> better scatterplot
## pseudo 3D
#When is it appropriate to use pie charts or donut charts? Never
# The use of pseudo-3D plots in the literature mostly adds: Confusion 
library(dslabs)
data("divorce_margarine")
plot(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine)
cor(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine)
#robust summaries statistics to outliers 
#outliers problem -> if we want to compute using the observed data, the mean and the standard deviation of our distribution, which in this caseis 0 and 1, this one point is going to have a big influence on the samplemean of the average and the standard deviation
#median is not sensitive to outliers 
#MAD (median absolute deviaiton) = a robust estimate of the standard deviation =So again, very simple-- you compute the rank of each vector and then compute the correlation
## compute the median of sample;compute the distance of each point to the median;compute the distance as the absolute value of the difference.;take the median of those deviations-- that's
#Spearman correlation  - correlation computed on ranks
##Instead of looking at the values(outliers are driving the majority of the values), we look at the ranks;
##So the outlier is  no longer way out there and pulling the correlation up towards 1, hence the correlation on ransk is pulling down toward the 0 
data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet) # rows here represent time points rather than individuals
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide") #facilitate the comparison of weights at different time points and across the different chicks
#reshape the data from long to wide, where the columns Chick and Diet are the ID's and the column Time indicates different observations for each ID
chick = na.omit(chick)
#How much does the average of chick weights at day 4 increase if we add an outlier measurement of 3000 grams?
head(chick)
chickout<-c(chick$weight.4,3000) # adding the outlier value of 3000;saw how sensitive the mean is to outliers check;what is the average weight of the day 4 chicks, including the outlier chick, divided by the average of the weight of the day 4 chicks without the outlier
mean(chickout)/mean(chick$weight.4)
# Compute the same ratio, but now using median instead of mean; what is the median weight of the day 4 chicks, including the outlier chick, divided by the median of the weight of the day 4 chicks without the outlier
chickout<-c(chick$weight.4,3000) # adding the outlier value of 3000;saw how sensitive the mean is to outliers check
median(chickout)/median(chick$weight.4)
sd(c(chick$weight.4, 3000))/sd(chick$weight.4)#try the same thing with the sample standard deviation;How much does the standard deviation change? What's the standard deviation with the outlier chick divided by the standard deviation without the outlier chick?
#MAD is unaffected by the addition of a single outlier
#includes the scaling factor 1.4826, such that mad() and sd() are very similar for a sample from a normal distribution
#What's the MAD with the outlier chick divided by the MAD without the outlier chick?
mad(c(chick$weight.4, 3000))/mad(chick$weight.4)
#how the Pearson correlation is affected by an outlier as compared to the Spearman correlation
plot(chick$weight.4,chick$weight.21)
cor(chick$weight.4,chick$weight.21) #Calculate the Pearson correlation of the weights of chicks from day 4 and day 21
plot(c(chick$weight.4,3000),c(chick$weight.21,3000))#Plot the weights of chicks from day 4 and day 21
cor(c(chick$weight.4,3000),c(chick$weight.21,3000))# calculate how much the Pearson correlation changes if we add a chick that weighs 3000 on day 4 and 3000 on day 21
cor(c(chick$weight.4,3000),c(chick$weight.21,3000))/cor(chick$weight.4,chick$weight.21) # divide the Pearson correlation with the outlier chick over the Pearson correlation computed without the outliers.
#Mann-Whitney-Wilcoxon Test
#outliers can make p-value to be incorrect 
#a z-score for this sum of rank statistic
#exercise
data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
head(chick)
chick = na.omit(chick)
#
x = chick$weight.4[chick$Diet == 1]#Save the weights of the chicks on day 4 from diet 1 as a vector x
y = chick$weight.4[chick$Diet == 4]#Save the weights of the chicks on day 4 from diet 4 as a vector y.
t.test(c(x, 200), y)$p.value #a t-test comparing x and y
wilcox.test(c(x,200),y,exact=FALSE)$p.value#a wilcoxon test 
#investigate a possible downside to the Wilcoxon-Mann-Whitney test statistic.
#make three boxplots, showing the true Diet 1 vs 4 weights, and then two altered versions: one with an additional difference of 10 grams and one with an additional difference of 100 grams
library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)
t.test(x,y+10)$statistic - t.test(x,y+100)$statistic #What is the difference in t-test statistic (obtained by t.test(x,y)$statistic) between adding 10 and adding 100 to all the values in the group y? 
#Wilcoxon problem-> for small sample sizes, the p-value can't be very small, even when the difference is very large
#quiz
head(mtcars$mpg)
head(mtcars$hp)
#If you wanted to compare the mpg and hp values against each other, what kind of plot would you use? :Scatterplot
#Examine the Wilcoxon test statistic for x and y+10 and for x and y+100. Because the Wilcoxon works on ranks, once the two groups show complete separation, that is all points from group y are above all points from group x, the statistic will not change, regardless of how large the difference grows. Likewise, the p-value has a minimum value, regardless of how far apart the groups are. This means that the Wilcoxon test can be considered less powerful than the t-test in certain contexts. In fact, for small sample sizes, the p-value can't be very small, even when the difference is very large
x<- c(1,2,3)
y<- c(4,5,6)
wilcox.test(x, y)#What is the p-value if we compare c(1,2,3) to c(4,5,6) using a Wilcoxon test?
wilcox.test(x, y + 10)
wilcox.test(x, y + 100)
wilcox.test(c(1, 2, 3))
wilcox.test(c(4, 5, 6))
#What is the p-value if we compare c(1,2,3) to c(400,500,600) using a Wilcoxon test?
wilcox.test(c(1,2,3),c(400,500,600))$p.value
#
data(nym.2002, package="UsingR")
time = sort(nym.2002$time)
#compare
##A plot of the ratio of times to the median time, with horizontal lines at twice as fast as the median time, and twice as slow as the median time.
plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))
##A plot of the log2 ratio of times to the median time. The horizontal lines indicate the same as above: twice as fast and twice as slow
plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)
#Why do we see this relationship?: Because log ratios transform to differences of logs

