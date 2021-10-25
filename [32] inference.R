# Inference
# sampling model parameters and estimates
library(tidyverse)
library(dslabs)
take_poll(25)    # draw 25 beads - observation # shows a random draw
# predict the proportion  of blue p; red = 1-p; spread = p-(1-p)= 2p - 1
# urn = population
# proportion of blue beads in the population, p, = parameter
# inference task ->p redict the parameter, p, using the observed data in the sample
#  estimate = a summary of the observed data that one thinks is informative about the parameter of interest
# sample proportion ->random variable
# taking an oppinion poll = as taking a random sample from an urn
# stat inference for estimating p
# to defend our use of the sample proportion, and quantify how close we think it is from the population proportion p
# for simplicity , draws are independent
# distirbution of the sum of draws 
# the expected value of the sum of draws is N times the average of the values in the urn
# the average of the 0s(red) and 1s(blue) in the urn must be the proportion p, the value we want to estimate
# polling vs. forecast 
# If a poll is conducted 4 months before the election, it is estimating the p for that moment, not for election day
# But, note that the p for election night might be different since people's opinions fluctuate through time. he polls provided the night before the election tend to be the most accurate
# properties of our estimate
# CLT ->  tells that the distribution function for a sum of draws is approximately normal
# want to know -> we are within one percentage point from p =   we made a very, very good estimate
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)
# margin of error 
pnorm(2)-pnorm(-2)
2*se
# Monte Carlo Simulation 
# Note: p is unknown;  one thing we can do to corroborate theoretical results is to pick a value of p or several values of p and then run simulations using those

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})
# Histogram and QQ-plot of Monte Carlo results
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
# the spread
# the competition is to predict the spread not the proportion
# poll a population in which a proportion  p of voters are Democrats and 1− p are Republicans; N = 25 , sample size; consider the random variable S, which is the total number of Democrats in your sample.
# What is the expected value of this random variable S ?
expectedS <- 25* p
# Polling - standard error of S
# random variable S, which is the total number of Democrats in the sample of 25 votere; the variable p describes the proportion of Democrats in the sample, whereas 1−p describes the proportion of Republicans.
# What is the standard error of S ?
seS <- sqrt(25*p(1-5))
# Polling - expected value of X-bar
# random variable S/N, which is equivalent to the sample average denoted with X¯; The variable N represents the sample size and p is the proportion of Democrats in the population.
# What is the expected value of X¯?
XS <= p 
# sample average
# function that takes the p of Democrats and the sample size N and returns the sample average of 1, Democrats and 0, Republicans
# p = 0.45; sample size = 100
# function called take_sample that takes p and N as arguements and returns the average value of a randomly sampled population.
take_sample <- function(p,N){
  X <- sample(c(0,1), size = N,replace = TRUE, prob =c(1-p,p))
  mean(X)
}
#  set.seed function to make sure the answer matches the expected result after random sampling
set.seed(1)
#  p as the proportion of Democrats in the population being polled
p <- 0.45
#  N as the number of people polled
N <- 100
# take_sample function to determine the sample average of N randomly selected people from a population containing a proportion of Democrats equal to p
take_sample(p,N)
# distirbution of errors
# p of democrats = 0.45; N = 100 sample size of poll voters;  replicate the sampling, R = 10000; calculate p-X¯ for each random sample; save the differences in a vector; calculate average of the vector 
#  p as the proportion of Democrats in the population being polled
p <- 0.45
#  N as the number of people polled
N <- 100
# The variable B specifies the number of times the sample to be replicated
B <- 10000
#  the set.seed function to make sure the answer matches the expected result after random sampling
set.seed(1)
#  an objected called errors that replicates subtracting the result of the take_sample function from p for B replications
errors <- replicate(B, p- take_sample(p,N))
#  the mean of the errors
mean(errors)










