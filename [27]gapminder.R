# load and inspect gapminder data
library(dslabs)
data(gapminder)
head(gapminder)
gapminder %>%
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey"))%>%
  select(country, infant_mortality)
gapminder %>%
  filter(year == 2015 & country %in% c("Malaysia", "Russia"))%>%
  select(country,infant_mortality)
# basic scatterplot of life expectancy versus fertility
ds_theme_set()    # set plot theme
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()
#facet_grid
#Faceting makes multiple side-by-side plots stratified by some variable. This is a way to ease comparisons.
#The facet_grid() function allows faceting by up to two variables, with rows faceted by one variable and columns faceted by the other variable. To facet by only one variable, use the dot operator as the other variable.
#The facet_wrap() function facets by one variable and automatically wraps the series of plots so they have readable dimensions
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent ~ year)
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)
#facet_wrap
# facet by year, plots wrapped onto multiple rows
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)
#time series plot 
#Time series plots have time on the x-axis and a variable of interest on the y-axis
#Single time series
# scatterplot of US fertility by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_point()
# line plot of US fertility by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()
#Multiple time series
# line plot fertility time series for two countries- only one line (incorrect)
countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility)) +
  geom_line()
# line plot fertility time series for two countries - one line per country
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, group = country)) +
  geom_line()
# fertility time series for two countries - lines colored by country
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, col = country)) +
  geom_line()
#Adding text labels to a plot
# life expectancy time series - lines colored by country and labeled, no legend
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")
#transformation
# add dollars per day variable;The GDP per person is often used as a rough summary of how rich a country is.
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)
# histogram of dollars per day
past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") 
#However, the majority of the x-axis is dedicated to the 35 countries with averages above 10.
#log transformation 
#So to get the distribution of the log base 2 transformed values, we simply transform the data and use the same code
# repeat histogram with log2 scaled data
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")
# Note: bimodal
# Other common choices are the natural log in base 10
# In general, we do not recommend using the natural log for data exploration and visualization
# In the dollar per day example, we use base 2 instead of base 10 because the resulting range is easier to interpret.
#With log base 2, we know that a bin width of 1 will translate to bins with range x to 2 to the x
# repeat histogram with log2 scaled x-axis
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")
#There are two ways we can use log transformation in plots
#log the values before plotting them, or we can use log scales in the axis.
# stratifying 
# add dollars per day variable
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)
# number of regions
length(levels(gapminder$region))
# boxplot of GDP by region in 1970
past_year <- 1970
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p + geom_boxplot()
# rotate names on x-axis
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#reorder : This function lets us change the order of the levels of a factor variable based on a summary computed on a numeric vector
# by default, factor order is alphabetical
fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)
# reorder factor by the category means
value <- c(10, 11, 12, 6, 4)
fac <- reorder(fac, value, FUN = mean)
levels(fac)
# Enhanced boxplot ordered by median income, scaled, and showing data
# reorder by median income and color by continent
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
  ggplot(aes(region, dollars_per_day, fill = continent)) +    # color by continent
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")
p
# log2 scale y-axis
p + scale_y_continuous(trans = "log2")
# add data points
p + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)
#comparing distirbution 
#Histogram of income in West versus developing world, 1970 and 2010
# add dollars per day variable and define past year
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)
past_year <- 1970

# define Western countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

# facet by West vs devloping
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ group)

# facet by West/developing and year
present_year <- 2010
gapminder %>%
  filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)
#Income distribution of West versus developing world, only countries with data 
# define countries that have data available in both years
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

# make histogram including only countries with data available in both years
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%    # keep only selected countries
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)
#Boxplots of income in West versus developing world, 1970 and 2010
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") 
+ scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~ .)

# arrange matching boxplots next to each other, colored by year
p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))
# Faceted smooth density plots
# smooth density plots - area under each curve adds to 1
gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
  summarize(n = n()) %>% knitr::kable()
# Note: If we overlay the two densities, the default is to have the area represented by each distribution add up to 1 regardless of the size of each group
# access computed variables
# To have the areas of the densities be proportional to the size of the groups,multiply the y-axis values by the size of the group
# smooth density plots - variable counts on y-axis
p <- gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)# bw for smoothness 
# case_when
# add group as a factor, grouping regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))
#Stacked density plot
# note you must redefine p with the new gapminder object first
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")

# stacked density plot
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)
#Weighted stacked density plot
# weighted stacked density plot
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)
#ecological fallacy = The almost perfect relationship between survival rates and income is only observed for the averages at the regional level.
# define gapminder
library(tidyverse)
library(dslabs)
data(gapminder)

# add additional cases
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))
# logit transformation = The logistic or logit transformation for a proportional rate p is defined as follows.f of p equals the log of p divided by 1 minus p
# define a data frame with group average income and average infant survival rate
# This scale is useful when we want to highlight differences that are near 0 or near 1
surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)
# plot infant survival versus income, with transformed axes
surv_income %>% 
  ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) 
## exercise
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
head(gapminder)

## fill out the missing parts in filter and aes
gapminder %>% 
  filter( year == 2012,continent == 'Africa') %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()
# use color to distinguish the different regions of Africa to see if this explains the clusters
gapminder %>% 
  filter(year ==2012 & continent == "Africa") %>%
  ggplot(aes(fertility, life_expectancy, color = region))+
  geom_point()
# conditions 
df <- gapminder %>%
  filter(year == 2012 & continent =="Africa" & fertility <=3 & life_expectancy >= 70)%>%
  select(country,region)
# time series plot 
tab <- gapminder %>%
  filter(country %in% c("Vietnam","United States") & year>=1960 & year <=2010)
p <- tab %>%
  ggplot(aes(year, life_expectancy,color = country))+
  geom_line()
p
# time series plot
gapminder %>%
  filter(year>=1960 & year <= 2010 & country == "Cambodia")%>%
  ggplot(aes(year, life_expectancy))+
  geom_line()
# frequency 
daydollars <- gapminder %>% mutate(dollars_per_day = gdp/population/365)%>% filter(continent == "Africa", year == 2010,!is.na(gdp)) 
# 
daydollars %>%
  ggplot(aes(dollars_per_day, y = ..count..))+
  geom_density()+
  scale_x_continuous(trans = "log2")
#density plot 
library(dplyr)
library(ggplot2)
library(dslabs)
daydollars <- gapminder %>%
  filter(continent == "Africa", year  %in% c(1970,2010),!is.na(gdp)) %>% mutate(dollars_per_day = gdp/population/365) %>%
  ggplot(aes(dollars_per_day, y =..count..)) +
  geom_density()+
  scale_x_continuous(trans="log2")+
  facet_grid(.~year)
daydollars
#stack density plot 
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
daydollars <- gapminder %>%
  filter(continent == "Africa", year  %in% c(1970,2010),!is.na(gdp)) %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  ggplot(aes(dollars_per_day, y =..count.., fill = region)) +
  geom_density(bw = 0.5, position = "stack")+
  scale_x_continuous(trans="log2")+
  facet_grid(year~.)
daydollars
# scatter plot
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder_Africa_2010 <- gapminder %>%
  filter(continent == "Africa", year  == 2010,!is.na(gdp)) %>% 
  mutate(dollars_per_day = gdp/population/365)
# create the mutated dataset
# now make the scatter plot
gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day,infant_mortality,color = region))+
  geom_point()
# scale
gapminder_Africa_2010 %>% # your plotting code here
  ggplot(aes(dollars_per_day,infant_mortality,color = region))+
  geom_point()+ 
  scale_x_continuous(trans="log2")
# label 
gapminder_Africa_2010 %>% # your plotting code here
  ggplot(aes(dollars_per_day,infant_mortality,color = region, label = country))+
  geom_point()+
  scale_x_continuous(trans="log2")+
  geom_text()
# scatter plot 
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder %>%
  filter(continent == "Africa", year  %in% c(1970,2010),!is.na(gdp),!is.na(infant_mortality)) %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  ggplot(aes(dollars_per_day,infant_mortality, color = region, label = country)) +
  geom_point()+
  scale_x_continuous(trans="log2")+
  geom_text()+
  facet_grid(year~.)
