# confounding
# So, does this mean that if we go and hire low salary players with many bases on balls that increases the number of walks per game by 2 for our team?
# Our team will score 1.47 more runs per game? We are again reminded that association is not causation
# The data does provide strong evidence that a team with 2 more bases on balls per game than the average team scores 1.47 more runs per game, but this does not mean that bases on balls are the cause
# If we do compute the regression line slope for singles, we get 0.449, a lower value. Note that a single gets you to first base just like a base on balls
# a regression line used to minimize the squared deviations of predictions is called as the regression line
# NB: Those that know a little bit more about baseball will tell you that with a single, runners that are on base have a better chance of scoring than with a base on balls.
# https://www.quora.com/Is-it-true-that-with-a-single-runners-that-are-on-base-have-a-better-chance-of-scoring-than-with-a-base-on-balls-If-so-why
# scatter plot BB averages .... increase the BB number with 2, the slope of teh regression line is getting higher  =  evidence that a team with 2 more bases on balls per game than the average team scores 1.47 more runs per game, but this does not mean that bases on balls are the cause
# if scattered are singles, then regression line slope for singles, we get 0.449, a lower value compared with the BB sctatter
# but, those that know a little bit more about baseball will tell you that with a single, runners that are on base have a better chance of scoring than with a base on balls
# why BB turns to be more predictive ... confouding 

# find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
#It turns out that pitchers, afraid of homeruns,will sometimes avoid throwing strikes to homerun hitters.As a result, homerun hitters tend to have more bases on balls
Teams %>%  
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

# Thus, a team with many homeruns will also have more bases on balls than average, and as a result, it may appear that bases on balls cause runs. But it is actually the homeruns that caused the runs.
# bases on balls are confounded with homeruns
# To find out, we somehow have to adjust for the homerun effect. Regression can help with this
# stratification and multivariate regression 
# A first approach to check confounding is to keep HRs fixed at a certain value and then examine the relationship between BB and runs
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)
# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)
# regression slope for predicting runs with bases on balls when we ignore home runs was 0.735
# calculate slope of regression line after stratifying by HR
# But once we stratify by home runs, these slopes are substantially reduced. Slopes close to the slopes of singles
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))
# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 
# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)
# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 
# Note that if we take random variability into account
# the estimated slopes by strata don't appear to change that much = function is constant
# This model implies that if the number of home runs is fixed,we observe a linear relationship between runs and bases on balls
# And that the slope of that relationship does not depend on the number of home runs
# Only the slope changes as the home runs increase.
# linear model
# When we're not able to randomly assign each individual to a treatment or control group, confounding is particularly prevalent
# adjust for confound in practice
# use regression 
# We have described how, if data is bivariate normal, then the conditional expectation follow a regression line
#  model =  the conditional expectation is a linear combination of known quantities = Any combination that multiplies them by a constant and then adds them up with, perhaps, a shift.
 
lm(son ~ father, data = galton_heights) # For every inch we increase the father’s height, the predicted son’s height grows by 0.5 inches.
# want the intercept term for our model to be more interpretable
galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))
lm(son ~ father_centered, data = galton_heights)
# the intercept (often labeled the constant) is the expected mean value of Y when all X=0; If X sometimes equals 0, the intercept is simply the expected mean value of Y at that value.
# The height of a son of a father of average height is 70.45 inches.
# Because the fathers’ heights (the independent variable) have been centered on their mean, the intercept represents the height of the son of a father of average height. In this case, that means that the height of a son of a father of average height is 70.45 inches.
# If we had not centered fathers’ heights to its mean, then the intercept would represent the height of a son when a father’s height is zero.
# in linear model, assumed is that the errors are independent 
# E[R|BB = x1,HB=x2] = b0 +b1x1+b2x2 where the intercept is b0+b1x1

# least square estimates 
# estimation of unknown parameters, b - tas
# residual sum of squares 
# find the values that minimize the residual sum of squares  = least square estimates 
# 
# compute RSS for any pair of beta0 and beta1 in Galton's data
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
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}
# plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))
# the lm fucntion -> obtain leats square estimates 
# fit regression line to predict son's height from father's height
fit <- lm(son ~ father, data = galton_heights)
fit
# summary statistics
summary(fit)
# LSE is derived from data, hence random variable 
# Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
# Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)
# summary statistics
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef
lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))
#interpretation is not straight-forward, it is also useful to know that the LSE can be strongly correlated
lse %>% summarize(cor(beta_0, beta_1))
B <- 1000
N <- 50
lse <- replicate(B, { # standardize the father heights
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})
cor(lse[1,], lse[2,]) 
# we can obtain predictions of y by plugging the estimates into the regression model
# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")
# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)
# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>% #can construct confidence intervals; plots confidence intervals around the predicted y hat; The R function predict takes an lm object as input and returns these predictions
  ggplot(aes(father, Y_hat))+
  geom_line()
# LSE 
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)
# The least-squares method finds the optimal parameter values by minimizing the sum of squared residuals,
# linear model 
library(Lahman)
library(broom)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
Teams_small %>% 
  mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game = HR/G) %>% 
  do(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = .)))

# R code(s) below would properly plot the predictions and confidence intervals for our linear model of sons’ heights
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))
# set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits
female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
# slope
fit<- lm(mother ~ daughter, data = female_heights)
fit$coef[2]
# intercept 
fit$coef[1]
# What is the predicted height of the first mother in the dataset?
predict(fit)[1]
# What is the actual height of the first mother in the dataset?
female_heights$mother[1]
# we want to generate two tables: one for 2002 and another for the average of 1999-2001 seasons. We want to define per plate appearance statistics, keeping only players with more than 100 plate appearances. Here is how we create the 2002 table:
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
# How many players had a single rate mean_singles of greater than 0.2 per plate appearance over 1999-2001?
bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))

sum(bat_99_01$mean_singles > 0.2)
# How many players had a BB rate mean_bb of greater than 0.2 per plate appearance over 1999-2001?
sum(bat_99_01$mean_bb > 0.2)
# Use inner_join() to combine the bat_02 table with the table of 1999-2001 rate averages you created in the previous question. 
dat <- inner_join(bat_02, bat_99_01)
# What is the correlation between 2002 singles rates and 1999-2001 average singles rates?
cor(dat$singles, dat$mean_singles)
# What is the correlation between 2002 BB rates and 1999-2001 average BB rates?
cor(dat$bb, dat$mean_bb)
# Make scatterplots of ```mean_singles``` versus ```singles``` and ```mean_bb``` versus ```bb```

dat %>%
  ggplot(aes(singles, mean_singles)) +
  geom_point()
dat %>%
  ggplot(aes(bb, mean_bb)) +
  geom_point()
# Fit a linear model to predict 2002 ```singles``` given 1999-2001 ```mean_singles```.
# What is the coefficient of ```mean_singles```, the slope of the fit?
fit_singles <- lm(singles ~ mean_singles, data = dat)
fit_singles$coef[2]
# Fit a linear model to predict 2002 ```bb``` given 1999-2001 ```mean_bb```.
# What is the coefficient of ```mean_bb```, the slope of the fit?
fit_bb <- lm(bb ~ mean_bb, data = dat)
fit_bb$coef[2]
#Tibbles are more readable than data frames; If you subset a data frame, you may not get a data frame. If you subset a tibble, you always get a tibble; Tibbles can hold more complex objects such as lists or functions.; Tibbles can be grouped.
# inspect data frame and tibble
Teams
as_tibble(Teams)
# Note that the function was formerly called as.tibble()

# subsetting a data frame sometimes generates vectors
class(Teams[,20])

# subsetting a tibble always generates tibbles
class(as_tibble(Teams[,20]))

# pulling a vector out of a tibble
class(as_tibble(Teams)$HR)

# access a non-existing column in a data frame or a tibble
Teams$hr
as_tibble(Teams)$HR

# create a tibble with complex objects
tibble(id = c(1, 2, 3), func = c(mean, median, sd))
# function DO 
# use do to fit a regression line to each HR stratum
dat %>%  
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))

# using do without a column name gives an error
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .))

# define a function to extract slope from lm
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

# return the desired data frame
dat %>%  
  group_by(HR) %>%
  do(get_slope(.))

# not the desired output: a column containing data frames
dat %>%  
  group_by(HR) %>%
  do(slope = get_slope(.))

# data frames with multiple rows will be concatenated appropriately
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             estimate = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}

dat %>%  
  group_by(HR) %>%
  do(get_lse(.))
# broom package
# use tidy to return lm estimates and related information as a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)

# add confidence intervals with tidy
tidy(fit, conf.int = TRUE)

# pipeline with lm, do, tidy
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

# make ggplots
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

# inspect with glance
glance(fit)
# pairs 
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))
galton
galton %>%
  group_by(pair) %>%
  summarize(n = n())
# Calculate the correlation coefficients for fathers and daughters, fathers and sons, mothers and daughters and mothers and sons.
# Which pair has the **strongest** correlation in heights?
galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == max(cor))
# Which pair has the **weakest** correlation in heights?

galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == min(cor))
# What is the estimate of the father-daughter coefficient?
library(broom)
library(tidyverse)
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "father_daughter") %>%
  pull(estimate)
# For every 1-inch increase in mother's height, how many inches does the typical son's height increase?

galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_son") %>%
  pull(estimate)
# Which sets of parent-child heights are significantly correlated at a p-value cut off of .05?
# all
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight" & p.value < .05)
# building a better offensive metric for baseball 
# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()
#   a summary statistic different from batting average to evaluate players. They realized walks were important, and that doubles, triples, and home runs should be weighted much more than singles, and proposed on-base-percentage plus slugging percentage, or OPS. = regression analysis
# regression fallacy = sophmore slump 
#  create a table with player ID, their names, and their most played position:
  library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)
# code to create a table with only the ROY award winners and add their batting statistics:
  ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")
# to keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons:
  ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)
# to use the spread function to have one column for the rookie and sophomore years batting averages:
  ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY
# Measurement Error Models -> considers the external variables 
# to use dslabs function rfalling_object to generate simulations of dropping balls:
  library(dslabs)
falling_object <- rfalling_object()
# to draw the trajectory of the ball:
  falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")
# to use the lm() function to estimate the coefficients:
  fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)
tidy(fit)
#  to check if the estimated parabola fits the data:
  augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")
# to see the summary statistic of the regression:
  tidy(fit, conf.int = TRUE)
# linear model predict runs scored per game?
  lm(R ~ BB + singles + doubles + triples + HR)
# want to estimate runs per game scored by individual players, not just by teams. What summary metric do we calculate to help estimate this?
  pa_per_game <- Batting %>% 
    filter(yearID == 2002) %>% 
    group_by(teamID) %>%
    summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
    .$pa_per_game %>% 
    mean
# Team A is comprised of batters who, on average, get two bases on balls, four singles, one double, no triples, and one home run. 
# Team B is comprised of batters who, on average, get one base on balls, six singles, two doubles, one triple, and no home runs.
# Which team scores more runs, as predicted by our model?
  #What is the estimate for the effect of BB on runs?
  Teams %>%
    filter(yearID == 1971) %>%
    lm(R ~ BB + HR, data = .) %>%
    tidy() %>%
    filter(term == "BB") %>%
    pull(estimate)
  #What is the estimate for the effect of HR on runs?
  Teams %>%
    filter(yearID == 1971) %>%
    lm(R ~ BB + HR, data = .) %>%
    tidy() %>%
    filter(term == "HR") %>%
    pull(estimate)
# Interpret the p-values for the estimates using a cutoff of 0.05.
# Which of the following is the correct interpretation?
  fit <- Teams %>% filter(yearID %in% 1971) %>% 
    mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
    lm(R ~ BB + HR, data = .)
  tidy(fit, conf.int = TRUE)
# Repeat the above exercise to find the effects of BB and HR on runs (```R```) for every year from 1961 to 2018 using ```do()``` and the **broom** package.
# Make a scatterplot of the estimate for the effect of BB on runs over time and add a trend line with confidence intervals.
  res <- Teams %>%
    filter(yearID %in% 1961:2018) %>%
    group_by(yearID) %>%
    do(tidy(lm(R ~ BB + HR, data = .))) %>%
    ungroup() 
  res %>%
    filter(term == "BB") %>%
    ggplot(aes(yearID, estimate)) +
    geom_point() +
    geom_smooth(method = "lm")
# Fit a linear model on the results from Question 10 to determine the effect of year on the impact of BB.
# For each additional year, by what value does the impact of BB on runs change?
  res %>%
    filter(term == "BB") %>%
    lm(estimate ~ yearID, data = .) %>%
    tidy() %>%
    filter(term == "yearID") %>%
    pull(estimate)
  
# What is the p-value for this effect?
  res %>%
    filter(term == "BB") %>%
    lm(estimate ~ yearID, data = .) %>%
    tidy() %>%
    filter(term == "yearID") %>%
    pull(p.value)
# Linear Models 
  library(tidyverse)
  library(broom)
  library(Lahman)
  Teams_small <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(avg_attendance = attendance/G)
 # Use runs  per game to predict average attendance.
 # For every 1 run scored per game, average attendance increases by how much?
 # find regression line predicting attendance from R and take slope
  Teams_small %>% 
    mutate(R_per_game = R/G) %>% 
    lm(avg_attendance ~ R_per_game, data = .) %>% 
    .$coef %>%
    .[2]
  # For every 1 home run hit per game, average attendance increases by how much?
  Teams_small %>% 
    mutate(HR_per_game = HR/G) %>% 
    lm(avg_attendance ~ HR_per_game, data = .) %>% 
    .$coef %>%
    .[2]
# Use number of wins to predict average attendance; do not normalize for number of games.
# For every game won in a season, how much does average attendance increase
  Teams_small %>% 
    lm(avg_attendance ~ W, data = .) %>%
    .$coef %>%
    .[2]
# Suppose a team won zero games in a season.
# Predict the average attendance.
  Teams_small %>% 
    lm(avg_attendance ~ W, data = .) %>% 
    .$coef %>%
    .[1]
# Use year to predict average attendance.
# How much does average attendance increase each year?
  Teams_small %>% 
    lm(avg_attendance ~ yearID, data = .) %>% 
    .$coef %>%
    .[2]
# Game wins, runs per game and home runs per game are positively correlated with attendance. We saw in the course material that runs per game and home runs per game are correlated with each other. Are wins and runs per game or wins and home runs per game correlated?
# What is the correlation coefficient for wins and runs per game?
  cor(Teams_small$W, Teams_small$R/Teams_small$G)
# What is the correlation coefficient for wins and home runs per game?
  cor(Teams_small$W, Teams_small$HR/Teams_small$G)
# How many observations are in the 8 win strata?
# 
  dat <- Teams_small %>%
    mutate(W_strata = round(W/10)) %>%
    filter(W_strata >= 5 & W_strata <= 10)
  sum(dat$W_strata == 8)
# Which win stratum has the largest regression line slope? 5
  # calculate slope of regression line after stratifying by R per game
  dat %>%  
    group_by(W_strata) %>%
    summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(R/G))
# Which win stratum has the largest regression line slope?
  # calculate slope of regression line after stratifying by HR per game
  dat %>%  
    group_by(W_strata) %>%
    summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))
# Fit a multivariate regression determining the effects of runs per game, home runs per game, wins, and year on average attendance. Use the original ```Teams_small``` wins column, not the win strata from question 3.
# What is the estimate of the effect of runs per game on average attendance?
  fit <- Teams_small %>% 
    mutate(R_per_game = R/G,
           HR_per_game = HR/G) %>%
    lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)
  tidy(fit) %>%
    filter(term == "R_per_game") %>%
    pull(estimate)
# What is the estimate of the effect of home runs per game on average attendance?
  tidy(fit) %>%
    filter(term == "HR_per_game") %>%
    pull(estimate)
# What is the estimate of the effect of number of wins in a season on average attendance?
  tidy(fit) %>%
    filter(term == "W") %>%
    pull(estimate)
# Use the multivariate regression model from Question 4. Suppose a team averaged 5 runs per game, 1.2 home runs per game, and won 80 games in a season.
# What would this team's average attendance be in 2002?
predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002))
# What would this team's average attendance be in 1960?
  predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960))
# Use your model from Question 4 to predict average attendance for teams in 2002 in the original Teams data frame.
# What is the correlation between the predicted attendance and actual attendance?
  newdata <- Teams %>%
    filter(yearID == 2002) %>%
    mutate(avg_attendance = attendance/G,
           R_per_game = R/G,
           HR_per_game = HR/G)
  preds <- predict(fit, newdata)
  cor(preds, newdata$avg_attendance)
# correlation is not causation 
  library(dslabs)
  data("research_funding_rates")
  research_funding_rates
# Construct a two-by-two table of gender (men/women) by award status (awarded/not) using the total numbers across all disciplines.
# What is the number of men not awarded?
# What is the number of women not awarded?
  two_by_two <- research_funding_rates %>% 
    select(-discipline) %>% 
    summarize_all(funs(sum)) %>%
    summarize(yes_men = awards_men, 
              no_men = applications_men - awards_men, 
              yes_women = awards_women, 
              no_women = applications_women - awards_women) %>%
    gather %>%
    separate(key, c("awarded", "gender")) %>%
    spread(gender, value)
  two_by_two
#  Use the two-by-two table from Question 1 to compute the percentages of men awarded versus women awarded.
# What is the percentage of men awarded?
  two_by_two %>% 
    mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
    filter(awarded == "yes") %>%
    pull(men)
# What is the percentage of women awarded?
  two_by_two %>% 
    mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
    filter(awarded == "yes") %>%
    pull(women)
# a chi-squared test on the two-by-two table to determine whether the difference in the two success rates is significant. (You can use ```tidy()``` to turn the output of ```chisq.test()``` into a data frame as well.)
# What is the p-value of the difference in funding rate?
  two_by_two %>% select(-awarded) %>% chisq.test() %>% tidy() %>% pull(p.value)
# There may be an association between gender and funding. But can we infer causation here? Is gender bias causing this observed difference? The response to the original paper claims that what we see here is similar to the UC Berkeley admissions example. Specifically they state that this "could be a prime example of Simpson’s paradox; if a higher percentage of women apply for grants in more competitive scientific disciplines, then an analysis across all disciplines could incorrectly show 'evidence' of gender inequality."
# To settle this dispute, use this dataset with number of applications, awards, and success rate for each gender:
  dat <- research_funding_rates %>% 
    mutate(discipline = reorder(discipline, success_rates_total)) %>%
    rename(success_total = success_rates_total,
           success_men = success_rates_men,
           success_women = success_rates_women) %>%
    gather(key, value, -discipline) %>%
    separate(key, c("type", "gender")) %>%
    spread(type, value) %>%
    filter(gender != "total")
  dat
# To check if this is a case of Simpson's paradox, plot the success rates versus disciplines, which have been ordered by overall success, with colors to denote the genders and size to denote the number of applications.
dat %>% 
      ggplot(aes(discipline, success, size = applications, color = gender)) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_point()
