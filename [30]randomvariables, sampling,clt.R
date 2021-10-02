#random variables = resulting from a random process.
# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
#Being able to quantify the uncertainty introduced by randomness
#Statistical inference offers a framework for doing this,
#sampling models
#A sampling model models the random behavior of a process as the sampling of draws from an urn.
#The probability distribution of a random variable is the probability of the observed value falling in any given interval
#average of many draws of a random variable is called its expected value
#standard deviation of many draws of a random variable is called its standard error
#Monte Carlo simulation: Chance of casino losing money on roulette

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S
n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})
mean(S < 0)    # probability of the casino losing money
library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")
# Distributions versus Probability Distributions
# A random variable x has a distribution function.
# It's a theoretical concept
# we define capital F of a as a function that answers the question,what is the probability that x is less than or equal to a
# Notation for Random Variables
# Capital letters denote random variables () and lowercase letters denote observed values ()
# CLT for short tells us that when the number of independent draws-- also called sample size-- is large, the probability distribution of the sum of these draws is approximately normal.
# if we know that the distribution of a list of numbers is approximated by the normal distribution, all we need to describe the list are the average and the standard deviation
#We also know that the same applies to probability distributions. If a random variable has a probability distribution that is approximated with the normal distribution, then all we need to describe that probability distribution arethe average and the standard deviation
##Referred to as the expected value and the standard error
#expected value. In statistics books, it is common to use the letter capital E, like this, E of Xequals mu, to denote that the expected value of the random variable X is mu
#A random variable will vary around an expected value in a way that if you take the average of many, many draws, the average of the draws will approximate the expected value
#The first useful fact is that the expected value of the sum of draws is the number of draws times the average of the numbers in the urn
#How different can one observation be from the expected value ?
#The standard error, or SE for short, gives us an idea of the size of the variation around the expected value
#X If our draws are independent--That's an important assumption-- then the standard error of the sum is given by the equation, the square root of the number of draws, times the standard deviation of the numbers in the urn
#The standard error tells us the typical difference between a random variable and its expectation ;The central limit theorem tells us that the distribution of the sum of S is approximated by a normal distribution
#Using the Central Limit Theorem, we can skip the Monte Carlo simulation
#An American roulette wheel has 18 red, 18 black, and 2 green pockets. Each red and black pocket is associated with a number from 1 to 36. The two remaining green slots feature “0” and “00”. Players place bets on which pocket they think a ball will land in after the wheel is spun. Players can bet on a specific number (0, 00, 1-36) or color (red, black, or green).
#What are the chances that the ball lands in a green pocket?
# The variables `green`, `black`, and `red` contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green = green/(green+black+red)

# Print the variable `p_green` to the console
p_green
#In American roulette, the payout for winning on green is $17. This means that if you bet $1 and it lands on green, you get $17 as a prize.
#Create a model to predict your winnings from betting on green one time
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green
#Create a model to predict the random variable `X`, your winnings from betting on green.
X <- sample(c(17,-1), 1, replace = TRUE, prob=c(p_green, p_not_green))
#> Error in sample.int(x, size, replace, prob): incorrect number of probabilities
# Print the value of `X` to the console
X
#In American roulette, the payout for winning on green is $17. This means that if you bet $1 and it lands on green, you get $17 as a prize.In the previous exercise, you created a model to predict your winnings from betting on green.
#Now, compute the expected value of X, the random variable you generated previously.
#Using the chances of winning $17 (p_green) and the chances of losing $1 (p_not_green), calculate the expected outcome of a bet that the ball will land in a green pocket
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Calculate the expected outcome if you win $17 if the ball lands on green and you lose $1 if the ball doesn't land on green
p_green * 17 + p_not_green * (-1)
#The standard error of a random variable X tells us the difference between a random variable and its expected value. You calculated a random variable X in exercise 2 and the expected value of that random variable in exercise 3.
#Now, compute the standard error of that random variable, which represents a single outcome after one spin of the roulette wheel.
#Compute the standard error of the random variable you generated in exercise 2, or the outcome of any one spin of the roulette wheel.
#Recall that the payout for winning on green is $17 for a $1 bet.
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Compute the standard error of the random variable
abs((17 - -1))*sqrt(p_green*p_not_green)
#You modeled the outcome of a single spin of the roulette wheel, X, in exercise 2.
#Now create a random variable S that sums your winnings after betting on green 1,000 times.
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 1000

# Create a vector called 'X' that contains the outcomes of 1000 samples
X <- sample(c(17,-1), size = n, replace=TRUE, prob=c(p_green, p_not_green))

# Assign the sum of all 1000 outcomes to the variable 'S'
S <- sum(X)

# Print the value of 'S' to the console
S
#In the previous exercise, you generated a vector of random outcomes, S, after betting on green 1,000 times.
#What is the expected value of S?
# Using the chances of winning $17 (p_green) and the chances of losing $1 (p_not_green), calculate the expected outcome of a bet that the ball will land in a green pocket over 1,000 bets.
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 1000

# Calculate the expected outcome of 1,000 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
n * (p_green * 17 + p_not_green * (-1))
#You generated the expected value of S, the outcomes of 1,000 bets that the ball lands in the green pocket, in the previous exercise.
#What is the standard error of S?
#Compute the standard error of the random variable you generated in exercise 5, or the outcomes of 1,000 spins of the roulette wheel.

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 1000

# Compute the standard error of the sum of 1,000 outcomes
sqrt(n) * abs((17 + 1))*sqrt(p_green*p_not_green)
#averages and proportions
# the expected value of a sum of random variables is the sum of the expected values of the individual random variables
#If the x are drawn from the same urn, then they all have the same expected value = mu ;  therefore, the expected value of the sum is n times mu
#the expected value of random variables times a non-random constant is the expected value times that non-random constant
#A consequence of these two facts ->  the expected value of the average of draws from the same urn and  the expected value of the urn, call it mu again
# the square of the standard error of the sum of independent random variables is the sum of the square of the standard error of each random variable
#the standard error of random variables times a non-random constant is the standard error times a non-random constant
#A consequence ->   the standard error of the average of independent draws from the same urn is the standard deviation of the urn-- let's call it sigma, this is the Greek letter for s. --divided by the square root of n.
# if x is a normally distributed random variable, then if a and b are non-random constants, then a times X plus b is also a normally distributed random variable
#law of large numbers -> as  n increases, the standard error of the average of a random variable decreases. In other words, when  n is large, the average of the draws converges to the average of the urn;When n is very large, then the standard error is practically 0
#Note that the law of averages is sometimes misinterpreted.
##For example, if you toss a coin five times and you see heads each time, you might hear someone argue that the next toss is probably a tail because of the law of averages
##These events are independent. So the chance of a coin landing heads is 50%, regardless of the previous five
#The law of averages applies only when the number of draws is very, very large, not in small samples
# how large is large CLT
#Note for example, that when the probability of success is very small, we need larger sample sizes -> Poisson distribution is needed 
#American Roulette probability of winning money
#What is the probability that you end up winning money if you bet on green 100 times?
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38
# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green
# Define the number of bets using the variable 'n'
n <- 100
# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)
# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)
# Using the expected value 'avg' and standard error 'se', compute the probability that you win money betting on green 100 times.
1-pnorm(0,avg,se)
#American Roulette Monte Carlo simulation
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38
# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green
# Define the number of bets using the variable 'n'
n <- 100
# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000 #replicate the sample code for B <- 10000 simulations
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)
# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
#use the sample function to simulate n <- 100 outcomes of either a win (17) or a loss (-1) for the bet. Use the order c(17, -1) and corresponding probabilities. Then, use the sum function to add up the winnings over all iterations of the model. Make sure to include sum or DataCamp may crash with a "Session Expired" error.
S <- replicate(B,{
  X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(p_green, p_not_green))
  sum(X)
})
# Compute the average value for 'S'
mean(S)
# Calculate the standard deviation of 'S'
sd(S)
#American Roulette Monte Carlo vs CLT
#calculate the probability of winning money from the Monte Carlo simulation
#mean function to calculate the probability of winning money from the Monte Carlo simulation, S
# Calculate the proportion of outcomes in the vector `S` that exceed $0
mean(S>0)
#American Roulette Monte Carlo vs CLT comparison
#American Roulette average winnings per bet
#create a random variable Y that contains your average winnings per bet after betting on green 10,000 times
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)
# Define the number of bets using the variable 'n'
n <- 10000
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38
# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green
# Create a vector called `X` that contains the outcomes of `n` bets
X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(p_green, p_not_green))
# Define a variable `Y` that contains the mean outcome per bet. Print this mean to the console.
Y <- mean(X)
Y
#American Roulette per bet expected value
#What is the expected value of  Y, the average outcome per bet after betting on green 10,000 times?
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38
# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green
# Calculate the expected outcome of `Y`, the mean outcome per bet in 10,000 bets
Y <- p_green * 17 + p_not_green * (-1)
Y
#American Roulette per bet standard error
#What is the standard error of  Y , the average result of 10,000 spins?
# Define the number of bets using the variable 'n'
n <- 10000
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38
# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green
# Compute the standard error of 'Y', the mean outcome per bet from 10,000 bets.
abs((17 - (-1))*sqrt(p_green*p_not_green) / sqrt(n))
#American Roulette winnings per game are positive
#What is the probability that your winnings are positive after betting on green 10,000 times?
# We defined the average using the following code
avg <- 17*p_green + -1*p_not_green
# We defined standard error using this equation
se <- 1/sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)
# Given this average and standard error, determine the probability of winning more than $0. Print the result to the console.
1 - pnorm(0, avg, se)
#American Roulette Monte Carlo again
# Create a Monte Carlo simulation that generates 10,000 outcomes of S, the average outcome from 10,000 bets on green.
#Compute the average and standard deviation of the resulting list to confirm the results from previous exercises using the Central Limit Theorem.
## Make sure you fully follow instructions, including printing values to the console and correctly running the `replicate` loop. If not, you may encounter "Session Expired" errors.
# The variable `n` specifies the number of independent bets on green
n <- 10000
# The variable `B` specifies the number of times we want the simulation to run
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random number generation
set.seed(1)
# Generate a vector `S` that contains the the average outcomes of 10,000 bets modeled 10,000 times
S <- replicate(B,{  
  X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(p_green, p_not_green))
  mean(X)
})
# Compute the average of `S`
mean(S)
# Compute the standard deviation of `S`
sd(S)
#found the probability of winning more than $0 after betting on green 10,000 times using the Central Limit Theorem
#used a Monte Carlo simulation to model the average result of betting on green 10,000 times over 10,000 simulated series of bets
#American Roulette comparison
#What is the probability of winning more than $0 as estimated by your Monte Carlo simulation?
# Compute the proportion of outcomes in the vector 'S' where you won more than $0
mean(S>0)
#SAT 
#An old version of the SAT college entrance exam had a -0.25 point penalty for every incorrect answer and awarded 1 point for a correct answer. The quantitative test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose a student chooses answers by guessing for all questions on the test.
#What is the probability of guessing correctly for one question?
p<-1/5
p
#What is the expected value of points for guessing on one question?
cr <- 1
mtk <- -0.25
mu <- cr*p+mtk*(1-p)
mu
#What is the expected score of guessing on all 44 questions?
totNQues <- 44
totNQues*mu
#What is the standard error of guessing on all 44 questions?
sigma <- sqrt(totNQues) * abs(mtk-cr) * sqrt(p*(1-p))
sigma
#Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.
1-pnorm(8,mu,sigma) 
#What is the probability that a guessing student scores 8 points or higher?
set.seed(21,sample.kind = "Rounding")
B <- 10000
n <- 44
# Monte Carlo simulation
examScores <- replicate(B,{
  X <- sample(c(1, -0.25), n, replace = TRUE, prob=c(p, 1-p))
  sum(X)
})
# Probability of 8 or higher
mean(examScores>=8)
#Suppose that the number of multiple choice options is 4 and that there is no penalty for guessing - that is, an incorrect question gives a score of 0.
#What is the expected value of the score when guessing on this new test?
# New conditions
cr <- 1
mtk <- 0    # no penalty for incorrect answer
p <- 0.25 # there are 4 questions
n <- 44
# Expected value of 1 question
mu <- (cr*p)+(mtk*(1-p))
# Expected value of test
mu*n 
#Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a range of student skills.
#What is the lowest p such that the probability of scoring over 35 exceeds 80%?
# mu = n * (a*p + b*(1-p))
# sigma = sqrt(n) * abs(b-a) * sqrt(p*(1-p))
# probability of scoring over 35 is 1 - pnorm(35, mu, sigma)
p <- seq(0.25, 0.95, 0.05)
expectedValue <- sapply(p, function(x) {
  mu <- n* (cr*x + mtk*(1-x))
  sigma <- sqrt(n) * abs(mtk-cr) * sqrt(x*(1-x))
  1 - pnorm(35, mu, sigma)
})
# each p  have a different probability
plot(p, expectedValue)
p[which(expectedValue > 0.8)]
min(p[which(expectedValue > 0.8)])
# Betting on Roulette
#A casino offers a House Special bet on roulette, which is a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other words, a losing bet yields -$1 and a successful bet yields $6. A gambler wants to know the chance of losing money if he places 500 bets on the roulette House Special.
#What is the expected value of the payout for one bet?
p <- 5/38
a <- 6
b <- -1
mu <- a*p + b*(1-p)
mu
#What is the standard error of the payout for one bet?
sigma <- abs(b-a) * sqrt(p*(1-p))
sigma
#What is the expected value of the average payout over 500 bets?
n <- 500
mu
#What is the standard error of the average payout over 500 bets?
sigma/sqrt(n)
#What is the expected value of the sum of 500 bets?
mu500<- n*mu
mu500
#What is the standard error of the sum of 500 bets?
sd500 <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
sd500
#Use pnorm() with the expected value of the sum and standard error of the sum to calculate the probability of losing money over 500 bets, Pr(x<=0)
pnorm(0, mu500, sd500)
