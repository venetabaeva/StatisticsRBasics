#[scatterPlot]
library(dplyr)
library(dslabs)
data("murders")
populationInMilions <-murders$population/10^6
totalGunMurders <- murders$total
plot(populationInMilions,totalGunMurders)
#[scatterPlot]
library(dslabs)# Load the datasets and define some variables
data(murders)
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)
log10_population <- log10(murders$population)# Transform population (not population in millions) using the log10 transformation and save to object log10_population
log10_total_gun_murders <- log10(murders$total)# Transform total gun murders using log10 transformation and save to object log10_total_gun_murders
plot(log10_population,log10_total_gun_murders)# Create a scatterplot with the log scale transformed population and murders 
#[histograms]
hist(murders$rate) 
murders$state[which.max(murders$rate)]
#[histograms]
population_in_millions <- murders$population/10^6# Store the population in millions and save to population_in_millions 
hist(population_in_millions)# Create a histogram of this variable
#[boxplot]
boxplot(rate~region, data = murders)
#[boxplot]
boxplot(population~region, data = murders)# Create a boxplot of state populations by region for the murders dataset
#
library(dslabs)
data(heights)
options(digits = 3)   
str(heights)
ind <- heights$height > mean(heights$height)#How many individuals in the dataset are above average height?
sum(ind)
ind <- heights$height > mean(heights$height) & (heights$sex =="Female")#How many individuals in the dataset are above average height and are female?
sum(ind)
mean(heights$sex == "Female")# proportion of individuals in the dataset are female
minH<- min(heights$height) 
ind <- match(minH, heights$height)#Use the match() function to determine the index of the first individual with the minimum height.
heights$sex[ind]#Subset the sex column of the dataset by the index in 4b to determine the individual’s sex.
maxH <- max(heights$height)#Write code to create a vector x that includes the integers between the minimum and maximum heights (as numbers).
minH <- min(heights$height)
intgr <- c(minH:maxH)
intgr
sum(!(intgr %in% heights$height))#How many of the integers in x are NOT heights in the dataset?
heights <- mutate(heights, ht_cm = height*2.54)#create a new column of heights in centimeters named ht_cm
head(heights)
heights$ht_cm[18]
mean(heights$ht_cm) 
females <- filter(heights, sex == "Female") #females by filtering the heights2 data to contain only female individuals.
head(females)
nrow(females)#How many females are in the heights2 dataset?
mean(females$ht_cm)#What is the mean height of the females in centimeters?
#[scatterplot]
library(dslabs)
data(olive)
head(olive)
plot(olive$palmitic,olive$palmitoleic)
#[histogram]
hist(olive$eicosenoic)
#[boxplot]
boxplot(palmitic~region, data = olive)

