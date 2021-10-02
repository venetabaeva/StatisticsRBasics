# cumulative distribution function
# Do not assign a very small probability to every single value-> same for probability distributions
# Do:  define a function that operates  on intervals rather than single values
# F(a) = Pr (x<=a) 
# Pr(x>70.5) = 1- Pr(x<=70.5) = 1-F(70.5)
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% 
  filter(sex=="Male") %>% 
  pull(height)
# theoretical distribution 
# the cumulative distribution for the normal distribution
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches

#Using pnorm() to calculate probabilities ; no need of data, just mean and sd
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
1 - pnorm(70.5, mean(x), sd(x)) # The cumulative distribution for the normal distribution is defined by a mathematical formula,  
# Note: Also the normal distribution is defined for continuous variables. It is not described for discrete variables
# theoretical distribution
# For example, we could consider our adult data categorical  with each specific height a unique category. The probability distribution would then be defined by the proportion of students reporting each of those unique heights
# plot distribution of exact heights in data 
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)") #This would be the distribution function for those categories
# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)
# probabilities in normal approximation match well 
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x)) 
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))
# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))
# discretization.Although the true height distribution is continuous, the reported heights tend to be more common at discrete values, in this case, due to rounding
# Probability density  
# Pr(x)-> Pr(x=4)=1/6 # die (categorical data) 
#CDF define -> add up probability -> F(4) = Pr(x<=4)-> Pr(x=4)+ Pr(x=3)+Pr(x=2)+Pr(x=1)
#Note: In contrast, for continuous distributions, the probability of a single value is not defined
#atheoretical definition that has a similar interpretation; the probability density 
#The quantity with the most similar interpretation to the probability of a single value is the probability density function f(x)
#The probability density  f(x) is defined such that the integral of f(x) over a range gives the CDF of that range 
#In R you get the probability density function for the normal distribution using the function dnorm
#Note: dnorm -> will be essential to those wanting to fit models to data for which predefined functions are not available
#Note that dnorm() gives the density function and pnorm() gives the distribution function, which is the integral of the density function.
# probability density for the normal distribution
library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()
#Note: We can use dnorm() to plot the density curve for the normal distribution. dnorm(z) gives the probability density  f(z) of a certain z-score, so we can draw a curve by calculating the density over a range of possible values of z.
#First, we generate a series of z-scores covering the typical range of the normal distribution. Since we know 99.7% of observations will be within , we can use a value of  z slightly larger than 3 and this will cover most likely values of the normal distribution. Then, we calculate f(z), which is dnorm() of the series of z-scores. Last, we plot z  against f(z) .
#Note that dnorm() gives densities for the standard normal distribution by default. Probabilities for alternative normal distributions with mean mu and standard deviation sigma can be evaluated with
dnorm(z, mu, sigma) 
#Monte Carlo Simulations
#rnorm(n, avg, s) generates n random numbers from the normal distribution with average avg and standard deviation s
#Generating normally distributed random numbers
# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% 
  filter(sex=="Male") %>% 
  pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)
# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)
# as it will permit us to generate data that mimics naturally occurring events, and it'll let us answer questions related to what could happen by chance by running Monte Carlo simulations
# Monte Carlo simulation of tallest person over 7 feet 
B <- 10000  
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
}) 
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)
# Other Continuous Distributions
# d to plot the density function of a continuous distribution. Here is the density function for the normal distribution (abbreviation norm())
x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()
#Each distribution has a matching abbreviation (for example, norm() or t()) that is paired with the related function abbreviations (d, p, q, r) to create appropriate functions
#distribution of female heights is approximated by a normal distribution with a mean of 64 inches and a standard deviation of 3 inches. If we pick a female at random, what is the probability that she is 5 feet or shorter?
female_avg <- 64
female_sd <- 3
pnorm(5*12,female_avg,female_sd)
#Use pnorm to define the probability that a height will take a value of 6 feet or taller
#If we pick a female at random, what is the probability that she is 6 feet or taller?
female_avg <- 64
female_sd <- 3
1 - pnorm(6*12,female_avg,female_sd)
#If we pick a female at random, what is the probability that she is between 61 and 67 inches?
#Use pnorm to define the probability that a randomly chosen woman will be shorter than 67 inches.
#Subtract the probability that a randomly chosen will be shorter than 61 inches
pnorm(67,female_avg,female_sd) - pnorm(61,female_avg,female_sd)
#Repeat the previous exercise, but convert everything to centimeters. That is, multiply every height, including the standard deviation, by 2.54
#pnorm to define the probability that a randomly chosen woman will have a height between 61 and 67 inches, converted to centimeters by multiplying each value by 2.54.
female_avg <- 64*2.54
female_sd <- 3*2.54
pnorm(67*2.54,female_avg,female_sd) - pnorm(61*2.54,female_avg,female_sd)
#Compute the probability that the height of a randomly chosen female is within 1 SD from the average height
#Calculate the values for heights one standard deviation taller and shorter than the average.
taller <- female_avg + female_sd
shorter <- female_avg - female_sd
#Calculate the probability that a randomly chosen woman will be within 1 SD from the average height
pnorm(taller,female_avg,female_sd) - pnorm(shorter,female_avg,female_sd)
#Imagine the distribution of male adults is approximately normal with an average of 69 inches and a standard deviation of 3 inches. How tall is a male in the 99th percentile?
male_avg <- 69
male_sd <- 3
#Determine the height of a man in the 99th percentile, given an average height of 69 inches and a standard deviation of 3 inches
qnorm(0.99,male_avg,male_sd)
#The distribution of IQ scores is approximately normally distributed. The average is 100 and the standard deviation is 15. Suppose you want to know the distribution of the person with the highest IQ in your school district, where 10,000 people are born each year.
#Generate 10,000 IQ scores 1,000 times using a Monte Carlo simulation. Make a histogram of the highest IQ scores
B <- 1000 #number of times we want the simulation to run
set.seed(1) #make sure your answer matches the expected result after random number generation
highestIQ<- replicate(B, {
  simulated_data <- rnorm(10000, 100, 15)    
  max(simulated_data)    
})
hist(highestIQ)
#approximately normally distributed with a mean of 20.9 and standard deviation of 5.7 ACT
set.seed(16,sample.kind = "Rounding")
act_scores<- rnorm(10000, 20.9 , 5.7)
mean(act_scores) #What is the mean of act_scores?
sd(act_scores)#What is the mean of act_scores?
#A perfect score is 36 or greater (the maximum reported score is 36).
#In act_scores, how many perfect scores are there out of 10,000 simulated tests?
sum(act_scores>=36)
#In act_scores, what is the probability of an ACT score less than or equal to 30?
mean(act_scores>=30)
#In act_scores, what is the probability of an ACT score less than or equal to 10?
mean(act_scores <=10)
#Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value of the probability density function over x given a mean of 20.9 and standard deviation of 5.7; save the result as f_x. Plot x against f_x. Which of the following plots is correct?
x <- seq(1, 36)
f_x <- dnorm(x, mean = 20.9, sd = 5.7)
plot(x, f_x)
#What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
zscores <- (act_scores - mean(act_scores)) / sd(act_scores)
mean(zscores > 2)
#What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
2*sd(act_scores) + mean(act_scores)
#A Z-score of 2 corresponds roughly to the 97.5th percentile. Use qnorm() to determine the 97.5th percentile of normally distributed data with the mean and standard deviation observed in act_scores. What is the 97.5th percentile of act_scores?
qnorm(.975, mean(act_scores), sd(act_scores))
#Write a function that takes a value and produces the probability of an ACT score less than or equal to that value (the CDF). Apply this function to the range 1 to 36
#What is the minimum integer score such that the probability of that score or lower is at least .95?
cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
min(which(cdf >= .95))
#Use qnorm() to determine the expected 95th percentile, the value for which the probability of receiving that score or lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7. What is the expected 95th percentile of ACT scores?
qnorm(.95, 20.9, 5.7)
#As discussed in the Data Visualization course, we can use quantile() to determine sample quantiles from the data. Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th percentiles of the act_scores data. Save these as sample_quantiles. In what percentile is a score of 26? Your answer should be an integer (i.e. 60), not a percent or fraction. Note that a score between the 98th and 99th percentile should be considered the 98th percentile, for example, and that quantile numbers are used as names for the vector sample_quantiles.
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles < 26))])
#Make a corresponding set of theoretical quantiles using qnorm() over the interval p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard deviation 5.7. Save these as theoretical_quantiles. Make a QQ-plot graphing sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis
library(tidyverse)
library(ggplot2)
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()
