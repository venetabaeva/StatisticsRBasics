# moneyball
#  goal of a baseball game is to score more runs (points) than the other team.
# Each team has 9 batters who have an opportunity to hit a ball with a bat in a predetermined order.
# Each time a batter has an opportunity to bat, we call it a plate appearance (PA)
# PA ends with a binary outcome: the batter either makes an out (failure) and returns to the bench or the batter doesn’t (success) and can run around the bases, and potentially score a run (reach all 4 bases)
# five ways a batter can succeed (not make an out):
#  Base on balls (BB): the pitcher fails to throw the ball through a predefined area considered to be hittable (the strike zone), so the batter is permitted to go to first base.
## Single: the batter hits the ball and gets to first base.
## Double (2B): the batter hits the ball and gets to second base.
## Triple (3B): the batter hits the ball and gets to third base.
## Home Run (HR): the batter hits the ball and goes all the way home and scores a run
##  batting average = define a hit (H) and an at bat (AB). Singles, doubles, triples, and home runs are hits; H/AB and is considered the main measure of a success rate
## a walk (BB), is not a hit; walk (or base on balls) occurs when a pitcher throws four pitches out of the strike zone, none of which are swung at by the hitter; refraining from swinging at four pitches out of the zone, the batter is awarded first base
## measure two of the game's most important skills: a pitcher's control and a hitter's eye (meaning his ability to tell whether a pitch is a strike or a ball and swing -- or not swing -- accordingly).

# base on ball or stolen bases 

# First one, do teams that hit more home runs score more runs?
# Scatterplot of the relationship between home runs and runs per game (win)
# teams with more home runs tended to score more runs = strong association 
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()
View(Teams)
# We know that, by definition, home runs cause runs, because when you hit a home run, at least one run will score
# Now it could be that home runs also cause the bases on balls
# counfounding = So it might appear that a base on ball is causing runs, when in fact, it's home runs that's causing both
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
# Scatterplot of the relationship between stolen bases and runs per game (win) 
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
# Scatterplot of the relationship between bases on balls and runs per game (win) 
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
# Scatterplot of the relationship between at- bat  and runs per game (win) 
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
# a scatterplot of win rate (number of wins per game) versus number of fielding errors (E) per game
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(E_per_game = E/G, W_per_game = W/G) %>%
  ggplot(aes(E_per_game, W_per_game)) + 
  geom_point(alpha = 0.5)
# a scatterplot of triples (X3B) per game versus doubles (X2B) per game
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X2B_per_game = E/G, X3B_per_game = X3B/G) %>%
  ggplot(aes(X2B_per_game, X3B_per_game)) + 
  geom_point(alpha = 0.5)
# What does the variable “SOA” stand for in the Teams table? = The trend between two variables
# strikeouts by pitchers = stimuli onset asincrony 
# try to predict how many more runs will the team score if we increase the number of bases on balls but keep the home runs fixed.
# Why do we consider team statistics as well as individual player statistics? The success of any individual player also depends on the strength of their team.

# Correlation   -> The correlation coefficient is a summary of what? = 
# Galton tried to predict sons' heights based on fathers' heights
# mean and standard errors are insufficient for describing an important characteristic of the data: the trend that the taller the father, the taller the son.
# The trend that the taller the father, the taller the son -> correlation coefficient is an informative summary of how two variables move together that can be used to predict one variable using the other
# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))
# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)
# correlation coeefficient (x1,y1 .... xn,yn) ; rho denotes correlation which is the greek first letter for r = regression 
# correlation coefficient
# The correlation coefficient is always between -1 and 1. = how two variables move togehter 
library(HistData)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
  filter(gender == "male" & childNum == 1) %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
galton_heights %>% summarize(r = cor(father, son)) %>% 
  pull(r)
# regression 
rho <- mean(scale(x)*scale(y))
# sample correlation is a random variable 
# derived from samples are estimates containing uncertainty
# the sample correlation is an average of independent draws, the central limit theorem applies
# assume the father - son data is the population data
# compute sample correlation
R <- sample_n(galton_heights, 25, replace = TRUE) %>% # R = random variable 
  summarize(r = cor(father, son))
R
# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 50
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
# expected value and standard error
mean(R)
sd(R) # large 
# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2))) # not normal, hence, not good approhimation 
# The expected value we know is the population correlation
# correlation coefficient of the relationship between at- bat  and runs per game (win) 
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()
teams_cc <- Teams %>% 
  filter(yearID %in% 1961:2001 )
cor(teams_cc$R/teams_cc$G, teams_cc$AB/teams_cc$G)
# correlation coefficient of the relationship between win and errors  per game (win) 
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()
teams_cc <- Teams %>% 
  filter(yearID %in% 1961:2001 )
cor(teams_cc$E/teams_cc$G, teams_cc$W/teams_cc$G)
# correlation coefficient of the relationship between doubles (X2B) per game and triples (X3B) per game
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()
teams_cc <- Teams %>% 
  filter(yearID %in% 1961:2001 )
cor(teams_cc$X2B/teams_cc$G, teams_cc$X3B/teams_cc$G)
# intro to regression , stratification  and variance 
# Correlation is not always a good summary of the relationship between two variables
# general idea of conditional expectation is that we stratify a population into groups and compute summaries in each group
# way to improve the estimates of the conditional expectations is to define strata of with similar values of x
# If there is perfect correlation, the regression line predicts an increase that is the same number of SDs for both variables.
# If there is 0 correlation, then we don’t use x at all for the prediction and simply predict the average mu
# values between 0 and 1, the prediction is somewhere in between. If the correlation is negative, we predict a reduction instead of an increase
# NB, teh same correllation coefficient can expell different correlation graphics 
# motivate correlation 
# motivate and define linear regression 
# guess the hight of randomly selected son 
# the average height of son is value with highest proportion, hence high chances of minimizing the error 
# If we say that the father is taller taht the average height, spec, his sd is large than the average father 
# So shall we predict that the son is also 1.14 standard deviations taller than the average son? It turns out that this would be an overestimate
# look at all sons with fathers about 72 inches= stratify  father's height  = conditional average 
# are computing the average son height conditioned on the father being 72 inches tall
# issue , low number of heights with 72 exactly, hence, take around 72 ; Specifically, we will round fathers' heights to the nearest inch.
# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)
# Anscombe’s example =  fallacy sets in which summarizing with a correlation would be a mistake, regression lines shows it 
# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg
# stratify fathers' heights to make a boxplot of son heights ; us see the distribution of each group.; We can see that the centers of these groups are increasing with height; follow linear relationship 
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()
# center of each boxplot ; See the plot and notice that this appears to follow a line; The slope of this line appears to be about 0.5
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()
# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x # Slope = (correlation coefficient of son and father heights) * (standard deviation of sons’ heights / standard deviation of fathers’ heights)
b <- mu_y - m*mu_x
# The slope of this line appears to be about 0.5, which happens to be the correlation between father and son heights.
# This is not a coincidence.
# To see this connection, let's plot the standardized heights against each other, son versus father, with a line that has a slope equal to the correlation
# regression line
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son = mean(son)) %>%
  mutate(z_father = scale(father),z_son = scale(son))%>%
  ggplot(aes(z_father, z_son)) +
  geom_point()+
  geom_abline(intercept = 0, slope = r) # regression line = a line that has a slope equal to the correlation on standartized correlation 
# The regression line for two variables, x and y, tells us that for every standard deviation sigma x increase above the average mu x.
# For x, y grows rho standard deviations sigma y above the average mu y.
# add regression line to plot; 
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)
# in standard units 
galton_heights %>%
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)
# But then we realized that there were very few data points in each strata.
# When we did this approximation of rounding off the height of the fathers, we found that these conditional means appear to follow a line.
# And we ended up with the regression line.
# So the regression line gives us the prediction.
 # An advantage of using the regression line is that we used all the data to estimate just two parameters, the slope and the intercept
# justify correlaiton 
# bivariate normal distribution 
# When a pair of random variables is approximated by a bivariate normal distribution, the scatterplot looks like ovals
# another way 
# as the conditional distribution of Y given X = x, conditional distribution and stratification 
# If we think the height data is well-approximated by the bivariate normal distribution, then we should see the normal approximation hold for each grouping.
# Here, we stratify the son height by the standardized father heights and see that the assumption appears to hold
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)
# So in summary, if our data is approximately bivariate, then the conditional expectation-- which is the best prediction for y given that we know the value of x-- is given by the regression line
# variance of y is sigma square ; correlation and amount of variance are related 
# two regression lines 
# predict father's height based on son's height 
# NB , not an inversion formula 
# We need to compute the expected value of x given y. This gives us another regression function ; regression line comes from computing expectations 
# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x
# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y
# female
set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
#  mean and standard deviation of mothers' heights, the mean and standard deviation of daughters' heights, and the correlaton coefficient between mother and daughter heights
female_heights %>%
  summarize(mean(mother), sd(mother), mean(daughter), sd(daughter))
female_heights <- GaltonFamilies %>%
  filter(gender == "female" & childNum == 1) %>%
  select(mother, childHeight) %>%
  rename(daughter = childHeight)
female_heights %>% summarize(r = cor(mother, daughter)) %>% 
  pull(r)
#  the slope and intercept of the regression line predicting daughters' heights given mothers' heights. Given an increase in mother's height by 1 inch, how many inches is the daughter's height expected to change?
# calculate values to plot regression line on original data
mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
m <- r * s_y/s_x # Slope = (correlation coefficient of son and father heights) * (standard deviation of sons’ heights / standard deviation of fathers’ heights)
b <- mu_y - m*mu_x # intercept 
r * s_y/s_x # change in daughter's height in inches given a 1 inch increase in the mother's height 
# percent of the variability in daughter heights is explained by the mother's height?
var_percent<- (r * r)*100
# conditional expected value 
 x= 60 
 m*x+b 

