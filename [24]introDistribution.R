library(dslabs)
#data() #check all available data sets 
data(heights)
names(heights)#extract the variable names from a dataset
head(heights)
#discrete numeric data can be considered ordinal
#for variables belonging to a small number of different groups, with each group having many members
unique(heights$height) #how many unique values are used by the heights variable
length(heights$height)
length(unique(heights$height)) # how many unique (numeric ordinal) variables are there 
tab <- table(heights$height) #compute the frequencies # of each unique value 
sum(tab==1) #count the number of times a unique value appears
#with categorical data, the distribution describes the proportions of each unique category 
prop.table(table(heights$sex)) # proportion of each unique value = frequency table = distribution 
#to define a distribution for numerical data is more effective to define a function that reports the proportion of the data below a value A for all possible values of A
# cumulative distribution function (CDF)  F (a) = Pr (x</=a)  <- proportion of data below a given value a 
# any continuous dataset has a CDF, not only normal distribution 
# NB: male heights(continuous data type) on x and proportion on y axis 
# calculating probability related to continuous dataset not equal to calculate probability of a specific exact value -> not informative because each value is unique
# note: histograms are preferred rather than ECDF
# histogram = divide span of data into non - overlapping bins of the same size; for each bin count the number of values that fall in that interval; plot these counts as bars with the base of teh bar the interval 
# Note: histogram is an approximation -> all values in each interval are treated as the same when computing the bin heights
rangeForCdfFunction <- seq(min(heights$height), max(heights$height),length = 100) #define range of values spaning the dataset
cdfFunction <- function(f){ # computes probability for a single values 
  mean(heights$height<=f) # cdfFunction (rangeForCdfFunction) = Pr (f</=rangeForCdfFunction) 
}
#CDF defines proportion of data below a cutoff - rangeForCdfFunction
cdfValues <- sapply(rangeForCdfFunction,cdfFunction ) #  CDF defines proportion of data below the  cutoff rangeForCdfFunction
plot(rangeForCdfFunction,cdfValues) 
# defines proportion of data above the  cutoff rangeForCdfFunction => 1 - cdfFunction (rangeForCdfFunction)
cdfValuesAbove <- 1-(sapply(rangeForCdfFunction,cdfFunction))
plot(rangeForCdfFunction,cdfValuesAbove) 
# defines proportion of data between the  cutoff rangeForCdfFunction => 1 - cdfFunction (rangeForCdfFunction)
rangeForCdfFunctionQ <- seq(quantile(dfAlc$employrt,0.50), quantile(dfAlc$employrt,0.75),length = 100)
cdfValuesBellowQ <- sapply(rangeForCdfFunctionQ,cdfFunction) - (sapply(rangeForCdfFunction,cdfFunction))# define proportion of values between rangeForCdfFunction and rangeForCdfFunctionQ
plot(rangeForCdfFunctionQ,cdfValuesBellowQ)
# smooth density plot are similar to histograms
# smooth density plot lacks sharp edges and the scale of y-axis changes from counts to densities 
# smooth density is understood through estimates 
# estimates -> assumed is that the list of observed values come from much larger list of unobserved values (population)
# smooth = many small bins 
# problem: to not make the curve dependent on the hypothetical size of the hypothetical list -> compute the curve on the frequency scale rather than count scale
# problem: no millions measurements => cannot make a histogram with small bins => how we can estimate the hypothetical smooth curve that will be seen if we have all measurements? => 
# histogram with available data -> compute frequencies -> use bins appropriate for the data -> mark with points the top of each bin -> remove bins -> drow curve accross the points 
# NB! smooth density is based on relative assumptions and choices -> control smoothness of teh curve  that defines a smooth density through an option in the function, ggplot option that computes the smooth density 
# NB! select degree of smoothness that can be defend is representative of the underlying data -> example: do have reson to believe that proportion of people with similar heights should be teh same =  proportion that is 72 inches should be more similar to the proportion of 71 rather than 78, or 65
# interpreting the y - axis of smooth density plot 
# y - axis is scaled so that the area under the density curve adds up to 1 => if a bin has a base 1, then we can say that the value on the y -axis tells us the exact proportion of values in this bin 
# NB! for other bin sizes -> compute the proportion of the total area contained in that interval 
# normal distribution = Gaussian distribution = bell curve = probability to have x value between value a and value b using parameters mean and standard deviation
# why using the normal distribution -> rather than using data, the normal distribution is defined with a mathematical formula 
# normal distribution <- 95% are within 2SD from the average 
# => if a dataset is approximated by a normal distribution, then to describe the distribution only the average and the standard deviation are needed 
 mean1 <- sum(heights$height)/ length(heights$height)
 sd1 <- sqrt(sum((heights$height-mean)^2)/ length(heights$height))
 index1 <- heights$sex == "Male"
 xMHeight <- heights$height[index1]
 mean2 <- mean(xMHeight)
 sd2 <- sd(xMHeight)
 c(mean1=mean1,sd1=sd1)
 #standard units = z scores 
 #The normal distribution:
 #Is centered around one value, the mean
 #Is symmetric around the mean
 #Is defined completely by its mean (μ) and standard deviation ( σ )
 #Always has the same proportion of observations within a given distance of the mean (for example, 95% within 2 σ)
 #The standard deviation is the average distance between a value and the mean value.
 #Calculate the mean using the mean() function.
 #Calculate the standard deviation using the sd() function or manually. 
 #Standard units describe how many standard deviations a value is away from the mean. The z-score, or number of standard deviations an observation x is away from the mean μ:
 # z= x-μ/σ
 #Compute standard units with the scale() function.
 #Important: to calculate the proportion of values that meet a certain condition, use the mean() function on a logical vector. Because TRUE is converted to 1 and FALSE is converted to 0, taking the mean of this vector yields the proportion of TRUE.
 #The built-in R function sd() calculates the standard deviation, but it divides by length(x)-1 instead of length(x). When the length of the list is large, this difference is negligible and you can use the built-in sd() function. Otherwise, you should compute σ by hand. For this course series, assume that you should use the sd() function unless you are told not to do so.
 zMHeight = scale(xMHeight)
 mean(abs(zMHeight)<2) # count the number of z scores less than 2 and bigger than negative 2 and divide by total ; take the mean 
 # 68-95-99.7 rule = probability of observing events within a certain number of standard deviations of the mean 
 # above 68% observations will be within 1sd of the mean = |z|</= 1 (μ +/- 1σ )
 # about 95% observations  will be within 2sd of the mean  =  |z|</= 2  (μ +/- 2σ )
 # about 99.7% observations will be within 3sd of the mean =  |z|</= 3  (μ +/- 3σ )
 #The normal distribution has a mathematically defined CDF which can be computed in R with the function pnorm().
 #pnorm(a, avg, s) gives the value of the cumulative distribution function  for the normal distribution defined by average avg and standard deviation s.
 #We say that a random quantity is normally distributed with average avg and standard deviation s if the approximation pnorm(a, avg, s) holds for all values of a.
 #If we are willing to use the normal approximation for height, we can estimate the distribution simply from the mean and standard deviation of our values.
 #If we treat the height data as discrete rather than categorical, we see that the data are not very useful because integer values are more common than expected due to rounding. This is called discretization.
 # With rounded data, the normal approximation is particularly useful when computing probabilities of intervals of length 1 that include exactly one integer.
 # when willing to use normal approximation when not entire data set is needed to answer whether the probability that a randomly selected student is taller than 70.5
 1 - pnorm(70.5, mean(xMHeight),sd(xMHeight))
 1 - pnorm(70.5, mean(heights$height),sd(heights$height))
 #NB: the normal distribution is defined for continuous variables, not described  for discrete variables 
 # with continuous distribution the probability of a singualr values is not even defined 
 # no sense asking the probability  that a normally distributed value is 70, instead probabilities for intervals (69.99 and 70.1) but in case where the data is roudnded, the normal approximation is useful for approximating if an interval includes one round number (69.5 and 70.5)
 # 1st 3 are using teh data and almost the same as 2nd 3 which using teh pnorm
 # plot distribution of exact heights in data
 plot(prop.table(table(xMHeight)), xlab = "a = Height in inches", ylab = "Pr(x = a)")
 # probabilities in actual data over length 1 ranges containing an integer
 mean(xMHeight <=68.5) -  mean(xMHeight <=67.5)
 mean(xMHeight <=69.5) -  mean(xMHeight <=68.5)
 mean(xMHeight <=70.5) -  mean(xMHeight <=69.5)
 # probabilities in normal approximation match well
 pnorm(68.5, mean(xMHeight),sd(xMHeight)) - pnorm(67.5, mean(xMHeight),sd(xMHeight)) 
 pnorm(69.5, mean(xMHeight),sd(xMHeight)) - pnorm(68.5, mean(xMHeight),sd(xMHeight)) 
 pnorm(70.5, mean(xMHeight),sd(xMHeight)) - pnorm(69.5, mean(xMHeight),sd(xMHeight)) 
 #no access to data, can you approximate the proportion of the data that is between 69 and 72 inches?
 mean(xMHeight>69 & xMHeight<=72)
 pnorm(72, mean(xMHeight),sd(xMHeight))- pnorm(69,  mean(xMHeight),sd(xMHeight)) 
 # approximation is not useful for intervals that not include integer (70.9 and 70.1)
 #discretazation = although a true height distribution is continuous, the reported heights tend to be more common at discrete values (here, due to rounding)
 # probabilities in actual data over other ranges don't match normal approx as well
 mean(xMHeight <= 70.9) - mean(xMHeight <= 70.1)
 pnorm(70.9, mean(xMHeight), sd(xMHeight)) - pnorm(70.1, mean(xMHeight), sd(xMHeight)) 
 #the approximation is not always useful; example is for the more extreme values, often called the "tails" of the distribution
 exact <- mean(xMHeight > 79 & xMHeight <= 81) # calculate the proportion of heights between 79 and 81 
 avg <- mean(xMHeight)
 sd <- sd(xMHeight)
 approx <- pnorm(81, avg, sd) - pnorm(79, avg, sd)# estimate the proportion of heights between 79 and 81 
 exact/approx # report how many times bigger the actual proportion is compared to the approximation
 #what percent of seven footers(1 feet = 12 inches) are in the National Basketball Association (NBA);an estimate? 
 p<- 1-pnorm(7*12,69,3)#an approximation for the proportion, call it p, of men that are 7 feet tall or taller
 N <- round(p*10^9) #use the normal distribution to estimate how many of these 1 billion men are at least seven feet tall
 10/N #calculate the proportion of the world's 18 to 40 year old seven footers that are in the NBA (10)
# Percentiles are the quantiles that divide a dataset into 100 intervals each with 1% probability
 p <- seq(0.01, 0.99, 0.01)
 percentiles <- quantile(heights$height, p)
 percentiles[names(percentiles) == "25%"]
 percentiles[names(percentiles) == "50%"]
 percentiles[names(percentiles) == "75%"]
 #Quartiles divide a dataset into 4 parts each with 25% probability. They are equal to the 25th, 50th and 75th percentiles. The 25th percentile is also known as the 1st quartile, the 50th percentile is also known as the median, and the 75th percentile is also known as the 3rd quartile
 summary(heights$height)
 # Quantiles are cutoff points that divide a dataset into intervals with set probabilities. The th quantile is the value at which % of the observations are equal to or less than that value.
 # qnorm() function gives the theoretical value of a quantile with probability p of observing a value equal to or less than that quantile value given a normal distribution with mean mu and standard deviation sigma
 qnorm(p, mu, sigma)
 #By default, mu=0 and sigma=1. Therefore, calling qnorm() with no arguments gives quantiles for the standard normal distribution
 quantile(p) #p is the probability of a random observation less than or equal to the quantile
 #The result of pnorm() is the quantile => inverse functions
 #pnorm() function gives the probability that a value from a standard normal distribution will be less than or equal to a z-score value z
 pnorm(-1.96)
 qnorm(0.025)
 # determine the theoretical quantiles of a dataset: that is, the theoretical value of quantiles assuming that a dataset follows a normal distribution
 p <- seq(0.01, 0.99, 0.01)
 theoreticalQuantiles <- qnorm(p, mean(heights$height), sd(heights$height)) #qnorm() function with the desired probabilities p, mean mu and standard deviation sigma
 # check whether a distribution is approximating a normal one 
 # define series of proportions
 # for each proportion  determine the value q
 mean(heights$height <= 68.5) # 50% are below 68.5 
 # if p = 0.5, then q = 68.5 
 # for series of p's, if the quantiles for teh data match the quantiles for the normal distribution, then data is approximated by a normal distribution 
 library(tidyverse)
 library(dslabs)
 z <- scale(heights$height)
 # proportion of data below 69.5
 mean(heights$height <= 68.5)
 p <- seq(0.05,0.95,0.05)
observedQuantiles <- quantile(heights$height,p)
theoreticalQuantiles <- qnorm(p, mean = mean(heights$height), sd = sd(heights$height))
plot(theoreticalQuantiles, observedQuantiles)
abline(0,1)
observedQuantiles <- quantile(z,p)
theoreticalQuantiles <- qnorm(p)
plot(theoreticalQuantiles, observedQuantiles)
abline(0,1)
# Percentiles are special cases of quantiles -> The percentiles are the quantiles you obtain when you define p as 0.01, 0.02, up to 0.99, 1%, 2%, 3%, et cetera
# p = 0.25 is called the 25th percentile
#50th percentile is also known as the median
#quartiles are the 25th, 50th and 75th percentiles
# when the distribution is not normal -> boxplots 
# range, quartiles and 25th, 50th and 75th percentiles; ignore outliers when computing the range and plot the outliers as independent points
library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
percentiles_male <- quantile(male, seq(.01, 0.99, 0.01))
percentiles_female<- quantile(female, seq(.01, 0.99, 0.01))
male_percentiles <- c(percentiles_male[names(percentiles_male) == "10%"],percentiles_male[names(percentiles_male) == "30%"],percentiles_male[names(percentiles_male) == "50%"],percentiles_male[names(percentiles_male) == "70%"],percentiles_male[names(percentiles_male) == "90%"])
female_percentiles <- c(percentiles_female[names(percentiles_female) == "10%"],percentiles_female[names(percentiles_female) == "30%"],percentiles_female[names(percentiles_female) == "50%"],percentiles_female[names(percentiles_female) == "70%"],percentiles_female[names(percentiles_female) == "90%"])
df <- data.frame(female = female_percentiles, male = male_percentiles)
plot(theoreticalQuantilesMale, percentiles_male)
abline(0,1)
plot(theoreticalQuantilesFemale, percentiles_female)
abline(0,1)
zMale <- scale(male)
zFemale <- scale(female)
theoreticalQuantilesMale <- quantile(zMale, seq(.01, 0.99, 0.01))
theoreticalQuantilesFemale <- quantile(zFemale, seq(.01, 0.99, 0.01))
theoreticalQuantilesMale <- qnorm(seq(.01, 0.99, 0.01))
theoreticalQuantilesFemale <- qnorm(seq(.01, 0.99, 0.01))
plot(theoreticalQuantilesMale, percentiles_male)
abline(0,1)
plot(theoreticalQuantilesFemale, percentiles_female)
abline(0,1)
mad(dfAlc$employrt) # median absolute deviation
#error check outlier
error_avg <- function(k){
  dfAlc$employrt[1]<-k
  mean(dfAlc$employrt)
}
error_avg (10000)
error_avg (-10000)




  