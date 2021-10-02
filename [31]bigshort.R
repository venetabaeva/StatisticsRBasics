#the big short: Interest Rates Explained
#Interest rates for loans are set using the probability of loan defaults to calculate a rate that minimizes the probability of losing money.
#We can define the outcome of loans as a random variable. We can also define the sum of outcomes of many loans as a random variable
#The Central Limit Theorem can be applied to fit a normal distribution to the sum of profits over many loans. We can use properties of the normal distribution to calculate the interest rate needed to ensure a certain probability of losing money for a given probability of default
#to decide what interest rates we should charge
#Suppose your bank will give out 1,000 loans for 180,000 this year.
#Also suppose that your bank loses, after adding up all the costs, $200,000 per foreclosure
#2% are not paying, hence loses for teh bank 
#Interest rate sampling model
#probability of defaulting, not sure 
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure) #random variable 
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure) #random variable 
#construct a Monte Carlo simulation to get an idea of the distribution of this random variable
#Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})
#plot expected loses(values)
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")
#no need of Monte Carlo 
#CLT tells that because our losses are a sum of independent draws, its distribution is approximately normal with expected value and standard deviation given by the following formula
#Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error
#We can now set an interest rate to guarantee that on average, we break even -> add quantity x to each loan, represented by draws so that teh expected values equals zero 
x = - loss_per_foreclosure*p/(1-p) # x= -(lp/1-p)
x # about 2% interest rate
x/180000 #On a $180,000 loan, this equals an interest rate of:
#Although this interest rate guarantees that on average we break even, there's a 50% chance that we will lose money
#So we need to pick an interest rate that makes it unlikely for this to happen.At the same time, if the interest rate is too high our clients will go to another bank.
#So, let's they say that we want our chances of losing money to be one in 100.
# what does x have to be now?
#We want the sum-- let's call it capital S-- to have the probability of S less than zero to be 0.01.
# {lp+x(1-p)}n <- expected value of S; n = number of draws= number of lawns
# Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))\x
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans
#Monte Carlo simulation for 1% probability of losing money
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money
#Big short
#One of our employees points out that since the bank is making about $2,000 per loan, that you should give out more loans.Why just n?You explain that finding those n clients was hard. You need a group that is predictable, and that keeps the chances of defaults low.
#He then points out that even if the probability of default is higher, as long as your expected value is positive, you can minimize your chances of losing money by increasing n, the number of loans, and relyingon the law of large numbers.
#He claims that even if the default rate is twice as high, say 4%, if we set the rate just a bit higher so that this happens, you will get a positive expected value.
#Expected value with higher default rate and interest rate
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)
#Calculating number of loans for desired probability of losing money
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans
#Monte Carlo simulation with known default probability
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)
#Your friend's scheme was based in part on the following mathematical formula--by making n large, we minimize the standard error of our per-loan profit.
#However, for this rule to hold, the X's must be independent draws. The fact that one person defaults must be independent of other people defaulting.
#To construct a more realistic simulation than the original one your friend ran
#let's assume there could be a global event that affects everybody with high-risk mortgages, and changes their probability
#We will assume that with a 50-50 chance, all the probabilities go up or down slightly to somewhere between 0.03 and 0.05
#But it happens to everybody at once, not just one person -> these draws are not independent
#Monte Carlo simulation with unknown default probability
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million
#The financial meltdown of 2007 was due, among other things, to financial experts assuming independence when there was none
#Bank earnings
#Say you manage a bank that gives out 10,000 loans. The default rate is 0.03 and you lose $200,000 in each foreclosure.Create a random variable S that contains the earnings of your bank. Calculate the total amount of money lost in this scenario
# Assign the number of loans to the variable `n`
n <- 10000
# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000
# Assign the probability of default to the variable `p_default`
p_default <- 0.03
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Generate a vector called `defaults` that contains the default outcomes of `n` loans
defaults <- sample( c(0,1), n, replace = TRUE, prob=c(1-p_default, p_default))
# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
S <- sum(defaults * loss_per_foreclosure)
S
#Bank earnings Monte Carlo
#Run a Monte Carlo simulation with 10,000 outcomes for  S, the sum of losses over 10,000 loans. Make a histogram of the results.
# Assign the number of loans to the variable `n`
n <- 10000
# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000
# Assign the probability of default to the variable `p_default`
p_default <- 0.03
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# The variable `B` specifies the number of times we want the simulation to run
B <- 10000
# Generate a list of summed losses 'S'. Replicate the code from the previous exercise over 'B' iterations to generate a list of summed losses for 'n' loans.  Ignore any warnings for now.
S <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p_default, p_default), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})
# Plot a histogram of 'S'.  Ignore any warnings for now.
hist(S)
#Bank earnings expected value
#What is the expected value of  S, the sum of losses over 10,000 loans? For now, assume a bank makes no money if the loan is paid
# Assign the number of loans to the variable `n`
n <- 10000
# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000
# Assign the probability of default to the variable `p_default`
p_default <- 0.03
# Calculate the expected loss due to default out of 10,000 loans
n*(p_default*loss_per_foreclosure + (1-p_default)*0)
#Bank earnings standard error
#What is the standard error of S?
# Assign the number of loans to the variable `n`
n <- 10000
# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000
# Assign the probability of default to the variable `p_default`
p_default <- 0.03
# Compute the standard error of the sum of 10,000 loans
sqrt(n) * abs(loss_per_foreclosure) * sqrt(p_default*(1 - p_default))
#Bank earnings interest rate - 1
#So far, we've been assuming that we make no money when people pay their loans and we lose a lot of money when people default on their loans. Assume we give out loans for $180,000. How much money do we need to make when people pay their loans so that our net loss is $0?
#In other words, what interest rate do we need to charge in order to not lose money?
# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000
# Assign the probability of default to the variable `p_default`
p_default <- 0.03
# Assign a variable `x` as the total amount necessary to have an expected outcome of $0
x <- -(loss_per_foreclosure*p_default) / (1 - p_default)
# Convert `x` to a rate, given that the loan amount is $180,000. Print this value to the console.
x/180000
#Bank earnings interest rate - 2
#With the interest rate calculated in the last example, we still lose money 50% of the time. What should the interest rate be so that the chance of losing money is 1 in 20?
# Assign the number of loans to the variable `n`
n <- 10000
# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000
# Assign the probability of default to the variable `p_default`
p_default <- 0.03
# Generate a variable `z` using the `qnorm` function
z <- qnorm(0.05)
# Generate a variable `x` using `z`, `p_default`, `loss_per_foreclosure`, and `n`
x <- -loss_per_foreclosure*( n*p_default - z*sqrt(n*p_default*(1 - p_default)))/ ( n*(1 - p_default) + z*sqrt(n*p_default*(1 - p_default)))
# Convert `x` to an interest rate, given that the loan amount is $180,000. Print this value to the console.
x / 180000
#Big Short
#An insurance company offers a one-year term life insurance policy that pays $150,000 in the event of death within one year. The premium (annual cost) for this policy for a 50 year old female is $1,150. Suppose that in the event of a claim, the company forfeits the premium and loses a total of $150,000, and if there is no claim the company gains the premium amount of $1,150. The company plans to sell 1,000 policies to this demographic
library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)
#Insurance rates, part 1
#The death_prob data frame from the dslabs package contains information about the estimated probability of death within 1 year (prob) for different ages and sexes.
#Use death_prob to determine the death probability of a 50 year old female, p
p <- death_prob %>% 
  filter(age==50, sex=="Female")%>%
  pull(prob)
p
plot_prob <- ggplot(death_prob, aes(x=age, y=prob)) +
  geom_line(aes(col=sex)) +
  theme(panel.background = element_blank()) +
  xlab("age") +
  ylab("probability") +
  ggtitle("Death probability male vs. female by age") 
plot_prob <- ggplotly(plot_prob)
plot_prob
#filter records whose age <= 25
age_25 <- death_prob %>%
  filter(age <= 25)
#filter records whose age above 25 and  <=50
age_50 <- death_prob %>%
  filter(age > 25 & age <=50)
#filter records whose age above 50 and <=75
age_75 <- death_prob %>%
  filter(age > 50 & age <=75)
#filter records whose age above 75 but <=100
age_100 <- death_prob %>%
  filter(age > 75 & age <=100)
library(gridExtra)
manual_color <- c("Male" = "#e50914", "Female" = "#34a853")
plot_prob1 <- ggplot(age_25, aes(x=age, y=prob)) +
  geom_line(aes(col=sex)) + 
  theme(axis.text = element_text(face = "bold"), panel.background = element_blank()) +
  scale_color_manual(values = manual_color) +
  xlab("age") +
  ylab("probability") +
  ggtitle("Death probability - age<=25") 
plot_prob2 <- ggplot(age_50, aes(x=age, y=prob)) +
  geom_line(aes(col=sex)) +
  theme(axis.text = element_text(face = "bold"), panel.background = element_blank()) +
  scale_color_manual(values = manual_color) +
  xlab("age") +
  ylab("probability") +
  ggtitle("Death probability - age: 26 to 50") 
#subplot(plot_prob1, plot_prob2, nrows = 2, margin = 0.08, heights = c(0.5, 0.5))
grid.arrange(plot_prob1, plot_prob2, ncol=1)
plot_prob3 <- ggplot(age_75, aes(x=age, y=prob)) +
  geom_line(aes(col=sex)) +
  scale_color_manual(values = manual_color) +
  theme(axis.text = element_text(face = "bold"), panel.background = element_blank()) +
  xlab("age") +
  ylab("probability") +
  ggtitle("Death probability - age: 51 to 75") 
#plot_prob3 <- ggplotly(plot_prob3)
plot_prob4 <- ggplot(age_100, aes(x=age, y=prob)) +
  geom_line(aes(col=sex)) +
  scale_color_manual(values = manual_color) +
  theme(axis.text = element_text(face = "bold"), panel.background = element_blank()) +
  # theme_minimal(base_size = 12) +
  xlab("age") +
  ylab("probability") +
  ggtitle("Death probability - age: 76 to 100") 
#plot_prob4 <- ggplotly(plot_prob4)
grid.arrange(plot_prob3, plot_prob4, ncol=1)
#The loss in the event of the policy holder's death is -$150,000 and the gain if the policy holder remains alive is the premium $1,150.
#What is the expected value of the company's net profit on one policy for a 50 year old female?
-150000*p+1150*(1-p)
#Calculate the standard error of the profit on one policy for a 50 year old female.
abs(-150000-1150)*sqrt(p*(1-p))
#What is the expected value of the company's profit over all 1,000 policies for 50 year old females?
1000*(-150000*p+1150*(1-p))
#What is the standard error of the sum of the expected value over all 1,000 policies for 50 year old females?
sqrt(1000)*(abs(-150000-1150)*sqrt(p*(1-p)))
#Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of 1,000 policies.
pnorm(0,1000*(-150000*p+1150*(1-p)), sqrt(1000)*(abs(-150000-1150)*sqrt(p*(1-p))))
#Use death_prob to determine the probability of death within one year for a 50 year old male.
p<- death_prob%>%
  filter(age==50,sex=="Male")%>%pull(prob)
p
#Suppose the company wants its expected profits from 1,000 50 year old males with $150,000 life insurance policies to be $700,000. Use the formula for expected value of the sum of draws with the following values and solve for the premium b
# E[S] = mu_S = 700000
# n = 1000
# p = death prob of 50y Males
# a = 150000 loss
# b = premium to solve
# E[S] = n*(ap +b(1-p))
#b = ((E[S]/n)-ap/(1-p
#What premium should be charged?
b<- ((700000/1000)- - 150000*p)/(1-p)
b
#Using the new 50 year old male premium rate, calculate the standard error of the sum of 1,000 premiums.
serr <- sqrt(1000)*abs(b--150000)*sqrt(p*(1-p))
serr
#What is the probability of losing money on a series of 1,000 policies to 50 year old males? Use the Central Limit Theorem.
pnorm(0,1000*(-150000*p+b*(1-p)),serr)    
#Life insurance rates are calculated using mortality statistics from the recent past. They are priced such that companies are almost assured to profit as long as the probability of death remains similar. If an event occurs that changes the probability of death in a given age group, the company risks significant losses.
#In this 6-part question, we'll look at a scenario in which a lethal pandemic disease increases the probability of death within 1 year for a 50 year old to .015. Unable to predict the outbreak, the company has sold 1,000 $150,000 life insurance policies for $1,150.
#What is the expected value of the company's profits over 1,000 policies?
mu <- 1000 * (-150000*0.015+ 1150*(1-0.015))
mu
#What is the standard error of the expected value of the company's profits over 1,000 policies?
serr <- sqrt(1000)*abs(-150000 - 1150)*sqrt(0.015*(1-0.015))
serr
#What is the probability of the company losing money?
 pnorm(0,mu,serr)
#Suppose the company can afford to sustain one-time losses of $1 million, but larger losses will force it to go out of business.
#What is the probability of losing more than $1 million?
 pnorm(-1000000,mu, serr)
#Investigate death probabilities p <- seq(.01, .03, .001).
#What is the lowest death probability for which the chance of losing money exceeds 90%?
 p <- seq(.01, .03, .001)
 a <- -150000    # loss per claim
 b <- 1150    # premium - profit when no claim
 n <- 1000
 
 p_lose_money <- sapply(p, function(p){
   exp_val <- n*(a*p + b*(1-p))
   se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
   pnorm(0, exp_val, se)
 })
 
 data.frame(p, p_lose_money) %>%
   filter(p_lose_money > 0.9) %>%
   pull(p) %>%
   min()
#Investigate death probabilities p <- seq(.01, .03, .0025).
#What is the lowest death probability for which the chance of losing over $1 million exceeds 90%?
 p_lose_million <- sapply(p, function(p){
   exp_val <- n*(a*p + b*(1-p))
   se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
   pnorm(-1*10^6, exp_val, se)
 })
 
 data.frame(p, p_lose_million) %>%
   filter(p_lose_million > 0.9) %>%
   pull(p) %>%
   min()
 #Define a sampling model for simulating the total profit over 1,000 loans with probability of claim p_loss = .015, loss of -$150,000 on a claim, and profit of $1,150 when there is no claim. Set the seed to 25, then run the model once.
 #What is the reported profit (or loss) in millions (that is, divided by 10^6)?
set.seed(25)
n <- 1000
p_loss <- 0.015

X <- sample(c(0,1),n, replace = TRUE, prob=c((1-p_loss),p_loss)) 
loss<- -150000*sum(X==1)/10^6
profit<- 1150*sum(X==0)/10^6
loss+profit

#Set the seed to 27, then run a Monte Carlo simulation of your sampling model with 10,000 replicates to simulate the range of profits/losses over 1,000 loans
#What is the observed probability of losing $1 million or more?
set.seed(27)
S <- replicate(10000,
               {
                 X <- sample(c(0,1),1000, replace = TRUE, prob=c((1-0.015),0.015)) 
                 loss<- -150000*sum(X==1)/10^6
                 profit<- 1150*sum(X==0)/10^6
                 loss+profit
                 
               })
sum(S<=-1)/10000
#Suppose that there is a massive demand for life insurance due to the pandemic, and the company wants to find a premium cost for which the probability of losing money is under 5%, assuming the death rate stays stable at  p= 0.015
#Calculate the premium required for a 5% chance of losing money given n = 1000 loans probability of death p = 0.015, and loss per claim l=-150000. Save this premium as x for use in further questions.
p <- 0.015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*(n*p-z*sqrt(n*p*(1-p)))/(n*(1-p)+z*sqrt(n*p*(1-p)))
x
#What is the expected profit per policy at this rate?
l*p + x*(1-p)
#What is the expected profit over 1,000 policies?
n*(l*p + x*(1-p))
#Monte Carlo simulation run to determine the probability of losing money on 
#Set the seed to 28 before running your simulation
 # B = 10000 
 # n = 1000
 # x
 # l = 150000
 # p = .015
set.seed(28)
S <- replicate(10000, {
  X <- sample(c(0,1), n, replace = TRUE, prob=c((1-p), p))
  loss <- l*sum(X==1)/10^6 
  profit <- x*sum(X==0)/10^6
  loss+profit
})
sum(S<0)/10000
#The company cannot predict whether the pandemic death rate will stay stable. 
#Set the seed to 29
#randomly changes p by adding a value between -0.01 and 0.01 with sample(seq(-0.01,0.01,length = 100),1)
# uses the new random p to generate a sample of n = 1000 policies with premium x and loss per claim  l = -150000
#returns profit over n policies(sum of random variable )
# the outcome should be a vector of B total profits 
# use the results of the Monte Carlo simulaiton to answer 
set.seed(29,sample.kind="Rounding")
p <- 0.015
n <- 1000
l <- -150000
B <- 10000
x <- 3268
X <- replicate(B,{
  next_p <- p+sample(seq(-0.01, 0.01, length=100),1)
  Y <- sample(c(x,l),n,replace=TRUE,prob=c(1-next_p,next_p))
  sum(Y)
})
#What is the expected value over 1,000 policies?
mean(X)
#What is the probability of losing money?
sum(X<0)/B
#What is the probability of losing more than $1 million?
mean(X < -1000000)
 