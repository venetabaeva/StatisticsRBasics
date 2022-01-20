#poll
#define estimates througgh  margines of errors -> use this to forecast final results  and provide an estimate  of the precisio n=confidence interval and p value 
#poll and probability theory 
# use urn 
# task of statistical inference is to estimate an unknown population parameter using observed data from a sample.
# scope = guess the spread between the proportion of blue and red balli in an urn 
# the collection of elements in the urn is called the population
# take a sample ,with replacement, with sample size 250 
# parameter is a number that summarizes data for an entire population
# sample is observed data from a subset of the population
# estimate is a summary of the observed data about a parameter that we believe is informative
# want to predict the proportion of the blue beads in the urn, the parameter . The proportion of red beads in the urn is  1-p  and the spread is 2p -1 
# sample proportion is a random variable. Sampling gives random results drawn from the population distribution
# sampling model for polls
library(tidyverse)
library(dslabs)
take_poll(25) # sample proportion is a random variable 
# sample average 
# proposing to use the proportion of blue beads in a sample as and estimate of the parameter p 
# use probability theory to defend the sample proportion  and the confidence interval 
# x = 1 = blue and x = 0 = red 
# assume the beads are 0s and 1s 
# to estimate p  and later estimate the spread 2p-1 
# the proportion of blue beads in N draws is the average of the draws  of beads  X1, X2...Xn/ N = X bar
# X bar is a arandom variable, cause average of random draws 
# NXbar = number of blue beads drawn in N draws = N times the proportion of values in the urn 
# polling vs forecasting
# poll taken in advance of an election estimates  p for that moment
# forecasting is polling in time, use estimates of p to predict p on the last day 
# properties of estimate
# Xbar is a random variable with an expected value and standard error that represents the sample proportion 
# the expected value of Xbar is the parameter of interest p ; why?; because X bar is the sum of independent draws of a random variable times a constant 1/N 
# E(Xbar)= p
# If we increse N , then the standard error of the E(Xbar) decreases
# the standard error of the average of X bar over N draws  is SE(Xbar) = root (p(1-p)/N)
# with a large enough poll, the estimate converges to p 
# how large the poll should be? we don't have the p so to calculate the error 
# even with large polls, Xbar could be missleading if w don't realize is a random variable 
#can be used also other random variable equation to determine the expected value of teh sum of draws E(S) and standard error of draws SE(S), E(S) =Np and SE(S)=root(Np(1-p))
# Solution: how close are we to p 
# CLT -> the distirbution function for a sum of draws is approximatly normal; when dividing  a normally distributed  random variable by a nonrandom constant , the resulting random variable is also normally distributed 
# CLT -> distribution  of Xbar is approximatley normal, so X bar has an approximatley normal distirbution ; the expected value is p ; the standard error is  square root of p times 1 minus p 
# problem: want to know the p we are within 1%  from p  = what is the probability that te distance between  Xbar - p = 0.01 equas to Xbar being less or equal to p - 0.01
# CLT -> X bar will substitute p 
# use estimate of SE 
# compute the estimate of SE for the sample 
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
se
# compute the probability of being as close to p as we want 
pnorm(0.01/se) - pnorm(-0.01/se) 
# margin of error is 2 times the standard error of teh etimate X bar 
# 95% chance that X bar will be within 2SE of the actual parameter p 
# check the higher sample size 
library(tidyverse)
library(dslabs)
take_poll(2000)
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/2000)
se
pnorm(0.01/se) - pnorm(-0.01/se) 
# Se versus p ; calculate SE of a sample average when polled 25 people in the population; generate a sequence of 100 proportions that vary from 0 (no blue beans) to 1(all blue beans)
# `N` represents the number of people polled
N <- 25
# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0,1, length =100)
# Create a variable `se` that contains the standard error of each sample average
se <- sqrt(p*(1-p)/N)
# Plot `p` on the x-axis and `se` on the y-axis
plot(p,se)
# multiple plots of SE versus p 
# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)
# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)
# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.
for(N in sample_sizes){
  se <- sqrt(p*(1-p)/N)
  plot(p,se,ylim = c(0,0.5/sqrt(25)))
}
# SE of spread 
# `N` represents the number of people polled
N <- 25
# `p` represents the proportion of Democratic voters
p <- 0.45
# Calculate the standard error of the spread. Print this value to the console.
2*sqrt(p*(1-p)/N)
# run Monte Carlo simulations to compare with theoretical results assuming a value of p
# p is unknown; to check the theoretical results assuming a value of p, then run Monte Carlo 
# create a Monte Carlo simulation using a set value of p
p <- 0.45    # unknown p to estimate
N <- 1000
# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)
# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x) # Monte Carlo 
})
mean(x_hat)
sd(x_hat)
# histogram 
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)
# CLT 
# sample average 
# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.
take_sample <- function(p,N){
  X <- sample(c(0,1), size = N,replace = TRUE, prob =c(1-p,p))
  mean(X)
}
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45
# Define `N` as the number of people polled
N <- 100
# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p,N)
# distribution of errors 
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45
# Define `N` as the number of people polled
N <- 100
# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B, p- take_sample(p,N))
# Calculate the mean of the errors. Print this value to the console.
mean(errors)
# average size of error 
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45
# Define `N` as the number of people polled
N <- 100
# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))
# Calculate the mean of the absolute value of each simulated error. Print this value to the console.
mean(abs(errors))
# standard deviation of the spread 
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45
# Define `N` as the number of people polled
N <- 100
# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))
# Calculate the standard deviation of `errors`
sqrt(mean(errors^2))
# estimating the standard error 
# Define `p` as the expected value equal to 0.45
p <- 0.45
# Define `N` as the sample size
N <- 100
# Calculate the standard error
sqrt(p*(1-p)/N)
# standars error of the estimate 
# Define `p` as a proportion of Democratic voters to simulate
p <- 0.45
# Define `N` as the sample size
N <- 100
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X <- sample(0:1,N, replace =T,p=c(1-p,p))
# Define `X_bar` as the average sampled proportion
X_bar <- mean(X)
# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar*(1-X_bar)/N)
# plot errors
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45
# Define `N` as the number of people polled
N <- 100
# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Generate `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))
# Generate a qq-plot of `errors` with a qq-line showing a normal distribution
qqnorm(errors)
qqline(errors)
# estimating the probability of a specific value of X bar
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45
# Define `N` as the number of people polled
N <- 100
# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.
1-pnorm(0.5,mean=p,sd  = sqrt(p*(1-p)/N))
# estimating the probability of a specific error size 
# Define `N` as the number of people polled
N <-100
# Define `X_hat` as the sample average
X_hat <- 0.51
# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)
# Calculate the probability that the error is 0.01 or larger
1 - pnorm (0.01,0, se_hat) + pnorm(-0.01,0,se_hat)
# spread = 2p -1 
# predict the spread, not the proportion 
# expected value of the spread 2Xbar -1 
# standard error of spread is 2SEestimated(Xbar)
# margin of errro of the spread is 2 times the margin of error of Xbar
# do not run very large poll 100,000
# the problem is lie, and missing people, we don't know who is the population and not 
# bias = even if margin of error is small, it may not be exactly right that our expected value is p 
# Plotting margin of error in an extremely large poll over a range of values of p
library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()
# forecast 
# confidence interval 
# compute the probability that a given interval contains the true parameter p ; 95% convidences = 95% chance of including p in an interval 
# start and end of confidence intervals are random variables 
# Monte Carlo simulation of confidence intervals
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean
# Solving for z  with qnorm; to calculate any size confidence interval, need to calculate the values z for which Pr(-z<=Z<=z) equals the desired confidence;for a confidence interval of size q, solve for z = 1-(1-q/2); 95 % confidence interval is determined through z <- qnorm(0.975)
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval
# Monte Carlo simulation to confirm that a 95% confidence intervals includes p 95%
#NB -> 95% refers to the probability that the random interval falls on top of p ; it is technically incorrect saying that p has 95% chance of being between two values ; p is not random 
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)
# power = the probability of detecting a spread different from 0 , by increase of the sample size, the SE is lowered and better chance of detecting the direction of the spread 
# confidence interval of spread with sample size 25 
N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)
# p values related to the confidence intervals 
# rather than wanting to estimate the spread of the proportion of blue beans, we are interested in the question whether there are more blue beans than red -> 2p-1>0?
# null hypothesis = the spread is 0 (p= 0.5) even when 52% are red; the alternative hypothesis = the spread is not 0 
# p - value is the probability of detecting an effect of a certain size or larger when the null hypothesis is true 
# If a 95% confidence interval does not include our observed value, then the p-value must be smaller than 0.05
# It is preferable to report confidence intervals instead of p-values, as confidence intervals give information about the size of the estimate and p-values do not
#  Computing a p-value for observed spread of 0.02
N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))
# pollster results for p 
# The `polls` object that filtered all the data by date and nation has already been loaded. Examine it using the `head` function.
head(polls)
# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
polls <- mutate(polls, X_hat = polls$rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/polls$samplesize), lower = X_hat - qnorm(0.975)*se_hat, upper = X_hat + qnorm(0.975)*se_hat)
pollster_results <- select(polls, pollster, enddate, X_hat, se_hat, lower, upper)
# comparing to actual results
# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)
# Add a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% 
  mutate(hit=(lower<0.482 & upper>0.482)) %>% 
  summarize(mean(hit))
avg_hit 
# confidence interval for d
# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.")  %>%
  mutate(d_hat = rawpoll_clinton/100 - rawpoll_trump/100)
# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
N
# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- polls$d_hat[1]
d_hat
# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat + 1) /2
# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(d_hat - qnorm(0.975)*se_hat, d_hat + qnorm(0.975)*se_hat)
# polsters results of d
# The subset `polls` data with 'd_hat' already calculated has been loaded. Examine it using the `head` function.
head(polls)
# Create a new object called `pollster_results` that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
pollster_results <- polls %>% mutate(X_hat = (d_hat + 1) / 2) %>% mutate(se_hat = 2 * sqrt(X_hat * (1 - X_hat) / samplesize)) %>% mutate(lower = d_hat - qnorm(0.975) * se_hat) %>% mutate(upper = d_hat + qnorm(0.975) * se_hat) %>% select(pollster, enddate, d_hat, lower, upper)
pollster_results
# compare to actual d
# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)
# Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% mutate(hit=lower <= 0.021 & upper >= 0.021) %>% summarize(mean(hit))
# comparing to actual results by pollsters 
# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)
# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster.
polls %>% mutate(error = d_hat - 0.021) %>% ggplot(aes(x = pollster, y = error)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# comparing to actual results by myltiple pollsters 
# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)
# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster, but only for pollsters who took 5 or more polls.
polls %>% mutate(error = d_hat - 0.021) %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>%
  ggplot(aes(pollster, error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# poll aggregators aggregate the results of many polls to simulate polls with a large sample size and therefore generate more precise estimates than individual polls
# Polls can be simulated with a Monte Carlo simulation and used to construct an estimate of the spread and confidence intervals
# The actual data science exercise of forecasting elections involves more complex statistical modeling, but these underlying ideas still apply.
# Simulating polls
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls
# Calculating the spread of combined polls
d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)
# statistical modeling 
# pollsters and multilevel models 
# poll data and pollster bias 
# p = proportion voting for CLinton;  1-p = proportion voting for Trump 
# Generating simulated poll data
library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)
# keep only national polls from week before election with a grade considered reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))
# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
# compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat
# compute margin of error
p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
# histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)
# Investigating poll data and pollster bias
# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())
# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))
# Pollster bias reflects the fact that repeated polls by a given pollster have an expected value different from the actual spread and different from other pollsters. Each pollster has a different bias
# data - driven model 
# Instead of using an urn model where each poll is a random draw from the same distribution of voters, we instead define a model using an urn that contains poll results from all possible pollsters
# assume the expected value of the model is the actual spread d = 2p -1 
# rather than zeros and ones the un is now containing continuous numbers between minus 1 and 1 
# the standard deviation is not the sqrt of p times 1 minus p ; teh SE includes polster to pollster variability 
# CLT is still valid to estimate the sample average of many polls X1, ...Xn, because the average of the sum of many random variables is a normally distributed random variable with expected value d and standard error sigma/rootN 
# estimate sigma with the sample standard deviation
# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()
# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)
# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)
# heights revisited 
# used urn models to motivate probability models
# most data comes from individuals, not from urns 
# probability plays a role because the data come from a random sample
# random sample is taken from a population and the urn serves as an analogy for the population
# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)
# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height
# Calculate the population average. Print this value to the console.
mean(x)
# Calculate the population standard deviation. Print this value to the console.
sd(x)
# sample the population of heights 
# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `N` as the number of people measured
N <- 50
# Define `X` as a random sample from our population `x`
X <- sample(x,N,replace =TRUE)
# Calculate the sample average. Print this value to the console.
mean(X)
# Calculate the sample standard deviation. Print this value to the console.
sd(X)
# confidence interval calculation 
# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `N` as the number of people measured
N <- 50
# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)
# Define `se` as the standard error of the estimate. Print this value to the console.
X_hat <- mean(X)
se_hat<- sd(X)
se <- se_hat/sqrt(N)
se
# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(qnorm(0.025,mean(X),se),qnorm(0.975,mean(X),se))
# Monte Carlo simulation of heights
# Define `mu` as the population average
mu <- mean(x)
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `N` as the number of people measured
N <- 50
# Define `B` as the number of times to run the model
B <- 10000
# Define an object `res` that contains a logical vector for simulated intervals that contain mu
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  X_hat <- mean(X)
  se_hat <- sd(X)
  se <- se_hat / sqrt(N)
  interval <- c(qnorm(0.025, mean(X), se) , qnorm(0.975, mean(X), se))
  between(mu, interval[1], interval[2])
})
# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)
# visuallizing polling bias
# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")
# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 
# Make a boxplot with points of the spread for each pollster
polls%>% ggplot(aes(pollster,spread))+geom_boxplot()+geom_point()
# compute estimates
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)
# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
polls %>% group_by(pollster)
sigma <- polls %>% group_by(pollster) %>% summarize(s = sd(spread))
# Print the contents of sigma to the console
sigma
# calculate 95% confidence interval of the spreads
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)
# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% summarize(avg=mean(spread), s = sd(spread), N=n())
res
# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate <- max(res$avg) - min(res$avg)
estimate
# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
se_hat
# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(estimate - qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)
# calculate the p - value 
# We made an object `res` to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 
# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
# Calculate the p-value
2* (1-pnorm(estimate/se_hat,0,1))
# comparing within poll and between poll variability 
# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()
# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.
var <- polls %>% group_by(pollster) %>% summarize(avg = mean(spread), s = sd(spread))
var
# bayesian statistics
# forecasters 
# 90% chance of meaning = the probability p being bigger than 50% is 90% -> p is fixed parameter and there is no sense of talking about p being this or that 
# with Bayesian it is assumed p is a random variable => allows us to calculate probabilities related to p 
# hierarchical models through bayesian statistics -> describe variability at different levels and incorporate all these levels into a model for estimating 
# Bayesian theorem 
# Prob( D = 1 | + test) of having disease given a positive test
# 99% test accuracy ( D = 1 | + test) = 0.99
# 99% test accuracy ( D = 0 | - test) = 0.99
# disease rate is  1 in 3900 => Pr (D=1) = 0.00025
# Pr (D=1|+)0.99 * 0.00025/0.99*0.00025 +0.01*0.99975 =0.02 <- despite the test having accuracy 99%, the probability of having the disease given a positive test is only 2%
# Monte Carlo simulation < - have to factor in the very rare possibility that a person chosen at random has the disease
prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))
N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy
# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))
table(outcome, test)
# bayesian in practice
# The techniques we have used up until now are referred to as frequentist statistics as they consider only the frequency of outcomes in a dataset and do not include any outside information. Frequentist statistics allow us to compute confidence intervals and p-values
# Frequentist statistics can have problems when sample sizes are small and when the data are extreme compared to historical results
# Bayesian statistics allows prior knowledge to modify observed results, which alters our conclusions about event probabilities
# hierarchical model 
# statistics in the courtroom
# claimed the cause of death was sudden infant death syndrome (SIDS).
# assumed the second death was independent of the first son being affected, thereby ignoring possible genetic causes.
# Let’s assume that there is in fact a genetic component to SIDS and the the probability of Pr(second case of SIDS∣first case of SIDS)=1/100, is much higher than 1 in 8,500.
p_1 <- 1/8500
p_2<- 1/100
p_1*p_2
# bcalculate probabolity 
# missused probability 
# Assume that the probability of a murderer finding a way to kill her two children without leaving evidence of physical harm is:
# Pr(two children found dead with no evidence of harm∣mother is a murderer)=0.50
# Assume that the murder rate among mothers is 1 in 1,000,000.
# Pr(mother is a murderer)=1/1,000,000
# According to Bayes’ rule, what is the probability of:
# Pr(mother is a murderer∣two children found dead with no evidence of harm)
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500
# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100
# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2
# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000
# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50
# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
Pr_AB <- Pr_BA*Pr_A/Pr_B
Pr_AB
# spread, average of spread, estimate of standard deviation 
# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)
# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
# Examine the `polls` object using the `head` function
head(polls)
# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
results <- polls %>% summarize(avg = mean(spread),  se = sd(spread)/sqrt(n()))
results
# posterior  distribution
# CLT tells estimate of spread has a normal distribution  with expected value d and sd sigma
# calculate the expected value of the posterior distribution ; mu = 0, tau = 0.01
# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results
# Define `mu` and `tau`
mu <- 0
tau <- 0.01
# Define a variable called `sigma` that contains the standard error in the object `results`
sigma <- results$se
# Define a variable called `Y` that contains the average in the object `results`
Y <- results$avg
# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
tau <- 0.01
miu <- 0
B <- sigma^2 / (sigma^2 + tau^2)
B
# Calculate the expected value of the posterior distribution
miu + (1 - B) * (Y - miu)
# SE posterior distribution 
# Here are the variables we have defined
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
# Compute the standard error of the posterior distribution. Print this value to the console.
sqrt(1 / (1 / sigma ^2 + 1 / tau ^2))
# constructing a credible interval 
# Here are the variables we have defined in previous exercises
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
est <- B * mu + (1 - B) * Y
est
ci <- c(est - qnorm(0.975) * se, est + qnorm(0.975) * se)
ci
# probability that the spread was less than 0 
# Assign the expected value of the posterior distribution to the variable `exp_value`
exp_value <- B*mu + (1-B)*Y 
# Assign the standard error of the posterior distribution to the variable `se`
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0, exp_value, se)
# change variance
# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg
# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)
# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc <- function(tau) {
  B <- sigma ^ 2 / (sigma^2 + tau^2)
  se <- sqrt(1 / (1/sigma^2 + 1/tau^2))
  exp_value <- B * mu + (1 - B) * Y
  pnorm(0, exp_value, se)
}
# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps <- p_calc(taus)
# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus, ps)
# forecasting 
# calculating probability of d>0 with general bias 
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))
1 - pnorm(0, posterior_mean, posterior_se)
# prediction electoral college 
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)
results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)
# Computing the average and standard deviation for each state
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))
# 10 closest races = battleground states
results %>% arrange(abs(avg))
# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")
# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)
# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))
# Calculating the posterior mean and posterior standard error
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))
# Monte Carlo simulation of Election Night results (no general bias)
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election
# histogram of outcomes
data.frame(clintonEV) %>%
  ggplot(aes(clintonEV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)
# Monte Carlo simulation including general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election
# forecaster issue: how informative polls taken several weeks before the election 
# Variability across one pollster ; In poll results,  is not fixed over time. Variability within a single pollster comes from time variation
# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se
# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")
# trend across time for several pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)
# Plotting raw percentages across time
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))
# confidence intervals for polling data 
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
# Create an object called `cis` that has the columns indicated in the instructions
cis <- polls %>% mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize), 
                        lower = spread - qnorm(0.975)*se, upper = spread + qnorm(0.975)*se) %>%
select(state, startdate, enddate, pollster, grade, spread, lower, upper)
# compare to actual results 
# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% summarize(proportion_hits = mean(hit))
p_hits
# stratify by pollster and grade 
# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(pollster) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>%
  arrange(desc(proportion_hits))
p_hits
# Stratify by State
# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.
p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(state) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n()) %>%
  arrange(desc(proportion_hits)) 
p_hits
# plot prediction 
# The `p_hits` data have already been loaded for you. Use the `head` function to examine it.
head(p_hits)
# Make a barplot of the proportion of hits for each state
p_hits %>% mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat = "identity") +
  coord_flip()
# predict 
# The `cis` data have already been loaded. Examine it using the `head` function.
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
head(cis)
# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% dplyr::mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
# Examine the last 6 rows of `errors`
tail(errors)
# plot prediciton 
# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
# Create an object called `p_hits` that summarizes the proportion of hits for each state that has 5 or more polls
p_hits <- errors %>%  group_by(state) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n())
# Make a barplot of the proportion of hits for each state
p_hits %>% mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat = "identity") +
  coord_flip()
# plot errors
# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)
# Generate a histogram of the error
hist(errors$error)
# Calculate the median of the errors. Print this value to the console.
median(errors$error)
# plot bias by state 
# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)
# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c("A+","A","A-","B+") | is.na(grade)) %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(state, error)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_boxplot() + 
  geom_point()

# filter error plot 
errors %>% filter(grade %in% c("A+","A","A-","B+") | is.na(grade)) %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(state, error)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_boxplot() + 
  geom_point()
# In models where we must estimate two parameters,  p and sigma , the Central Limit Theorem can result in overconfident confidence intervals for sample sizes smaller than approximately 30
# Calculating 95% confidence intervals with the t-distribution
z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)
# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)
# t - distribution ; The t-distribution takes the variability into account and generates larger confidence intervals.
# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.
1 - pt(2, 3) + pt(-2, 3)
# plot t -distribution 
# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- seq(3,50)
# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func <- function(n) {
  1 - pt(2, n) + pt(-2, n)
}
# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs <- sapply(df, pt_func)
# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df, probs)
# sample from normal distribution 
# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)
# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height
# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qnorm(0.975)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})
# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
  mean(res)
# sampling from the t -distribution
  # The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
  mu <- mean(x)
  # Use the same sampling parameters as in the previous exercise.
  set.seed(1)
  N <- 15
  B <- 10000
  res <- replicate(B, {
    s <- sample(x, N, replace = TRUE)
    interval <- c(mean(s) - qt(0.975, N - 1) * sd(s) / sqrt(N), mean(s) + qt(0.975, N - 1) * sd(s) / sqrt(N))
    between(mu, interval[1], interval[2])
  })
  # Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
  mean(res)
  
# association test 
# determine the probability that an observation is due to random variability given categorical, binary or ordinal data
# Fisher's exact test determines the p-value as the probability of observing an outcome as extreme or more extreme than the observed outcome given the null distribution
# Research funding rates example
  # load and inspect research funding rates object
  library(tidyverse)
  library(dslabs)
  data(research_funding_rates)
  research_funding_rates
  
  # compute totals that were successful or not successful
  totals <- research_funding_rates %>%
    select(-discipline) %>%
    summarize_all(funs(sum)) %>%
    summarize(yes_men = awards_men,
              no_men = applications_men - awards_men,
              yes_women = awards_women,
              no_women = applications_women - awards_women)
  
  # compare percentage of men/women with awards
  totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                       percent_women = yes_women/(yes_women + no_women))
  # p-value calculation with Fisher's Exact Test
  fisher.test(tab, alternative = "greater")
# chi test 
  # compute overall funding rate
  funding_rate <- totals %>%
    summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
    .$percent_total
  funding_rate
  
  # construct two-by-two table for observed data
  two_by_two <- tibble(awarded = c("no", "yes"),
                       men = c(totals$no_men, totals$yes_men),
                       women = c(totals$no_women, totals$yes_women))
  two_by_two
  
  # compute null hypothesis two-by-two table
  tibble(awarded = c("no", "yes"),
         men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
         women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))
  
  # chi-squared test
  chisq_test <- two_by_two %>%
    select(-awarded) %>%
    nbsp;   chisq.test()
  chisq_test$p.value
  
 # Odds ratio
  # odds of getting funding for men
  odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
    (two_by_two$men[1] / sum(two_by_two$men))
  
  # odds of getting funding for women
  odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
    (two_by_two$women[1] / sum(two_by_two$women))
  
  # odds ratio - how many times larger odds are for men than women
  odds_men/odds_women
  # p-value and odds ratio responses to increasing sample size
  
  # multiplying all observations by 10 decreases p-value without changing odds ratio
  two_by_two %>%
    select(-awarded) %>%
    mutate(men = men*10, women = women*10) %>%
    chisq.test()
  
# compare proportion of hits
  # The 'errors' data have already been loaded. Examine them using the `head` function.
  head(errors)
  totals <- errors %>%
    filter(grade %in% c("A-", "C-")) %>%
    group_by(grade,hit) %>%
    summarize(num = n()) %>%
    spread(grade, num)
  
  # Print the proportion of hits for grade A- polls to the console
  totals[[2,3]]/sum(totals[[3]])
  # Print the proportion of hits for grade C- polls to the console
  totals[[2,2]]/sum(totals[[2]])
# chi - square 
  # The 'totals' data have already been loaded. Examine them using the `head` function.
  head(totals)
  # Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
  chisq_test <- totals %>% 
    select(-hit) %>%
    chisq.test()
  chisq_test
  # Print the p-value of the chi-squared test to the console
  chisq_test$p.value
#odds ratio calculation 
  # The 'totals' data have already been loaded. Examine them using the `head` function.
  head(totals)
  # Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
  odds_C <- (totals[[2,2]] / sum(totals[[2]])) / 
    (totals[[1,2]] / sum(totals[[2]]))
  # Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
  odds_A <- (totals[[2,3]] / sum(totals[[3]])) / 
    (totals[[1,3]] / sum(totals[[3]]))
  # Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
  odds_A/odds_C
# significance
  