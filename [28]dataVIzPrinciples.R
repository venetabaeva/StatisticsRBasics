#Encoding Data Using Visual Cues
#position
#aligned lengths
#angles
#area
#brightness
#color hue
##  %in% c(year1, year2) & %in% c("browser1","browser2","browser3","browser4")
### Mistake: pie chart <- reprs quantities with both areas & angles
####demonstrated by perception studies,humans are not good at precisely quantifying angles, and  worse when only area is available
###Mistake: donut chart <- reprs quantities with only areas 
###Solution: plot quantities using length and positions<- humans are better at judging linear measures => bar plot
##bar plot : The bar plot uses this approach by using bars of length proportional to the quantity of interest
#Know When to Include Zero
##bar plot -> By avoiding 0, relatively small differences can be made to look much bigger than they actually are
##When using position rather than length, then it's not necessary to include 0
###This is particularly the case when we want to compare differences between groups relative to the variability seen within the groups
#Do Not Distort Quantities
#The reason for this distortion is that the radius, rather than the area, was made to be proportional to the quantity, which implies that the proportions between the areas is squared. So 2.6 turns into 6.5, and 5.8, turns into 34.1
##Not surprisingly, ggplot defaults to using area rather than the radius
##Solution: since we can use position and length -> bar plot is prefered over bubblechart 
#Order by a Meaningful Value
##reorder by value not alphabetic 
# Note: We have focused on displaying single quantities across categories
# Show the Data
# Note: We now shift our attention to displaying data with a focus on comparing groups
# dot plot showing the data instead of bar plot 
heights %>%
  ggplot(aes(sex, height)) + 
  geom_point()# issue: And many points are plotted above each other -> solution: the distribution is much more informative
# jittered, alpha blended point plot
heights %>% 
  ggplot(aes(sex, height)) + 
  geom_jitter(width = 0.1, alpha = 0.2) #Jitter is adding a small random shift to each point 
# distribution Ease Comparisons: Use Common Axes
# histogram ->  keep the axes the same when comparing data across plots
# histogram -> and it's to align plots vertical to see horizontal changes, and horizontally to see vertical changes.
heights %>% 
  ggplot(aes(sex, height)) + 
  geom_jitter(width = 0.1, alpha = 0.2)+ 
  geom_boxplot(alpha= 0.5) #Jitter is adding a small random shift to each point
#Note: Bar plots are useful for showing one number,but not very useful when wanting to describe distributions
# Consider Transformations
## We have motivated the use of the log transformation in cases where the changes are multiplicative
## consider are the logistic transformation-- useful to better see fold changes in odds--and the square root transformation, useful for count data
# Ease Comparisons: Compared Visual Cues Should Be Adjacent

##reorder 
library(dplyr)
library(ggplot2)
library(dslabs)
usContDiseasesMeasles <- us_contagious_diseases %>%
  filter(year == 1967 & disease=="Measles" & 
           !is.na(population)) %>% 
  mutate(rate = count / population * 10000 * 52 / weeks_reporting)
state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)
state <- reorder(state,rate, FUN = mean)
levels(state)
#numeric order 
library(dplyr)
library(ggplot2)
library(dslabs)
data(us_contagious_diseases)
usContDiseasesMeasles <- us_contagious_diseases %>% 
  filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting)
usContDiseasesMeasles %>% 
  mutate(state = reorder(state, rate, FUN = mean)) %>% 
  ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()
# numeric order  
library(dplyr)
library(ggplot2)
library(dslabs)
data("murders")
murders %>% 
  mutate(rate = total/population*100000) %>%
  mutate(region = reorder(region, rate, FUN = median)) %>% 
  ggplot(aes(region,rate)) + 
  geom_boxplot() + 
  geom_point() 
# slope chart 
# when comparing variables of the same type; at different time points;  for a relatively small number of comparison
library(tidyverse)
library(dslabs) 
data(gapminder)
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder20002015Slope <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)
gapminder20002015Slope %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 
#The Bland-Altman plot = Tukey Mean Different plot
library(ggrepel)
head(dat)
gapminder20002015Slope %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")
# Encoding a Third Variable
# tile plot
# The geom_tile() geometry creates a grid of colored tiles
# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)
# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
usContDisMeasles <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))
# plot disease rates per year in California
usContDisMeasles %>% 
  filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue") 
# tile plot of disease rate by state and year
usContDisMeasles %>% 
  ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")
#line plot
# compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)
# make line plot of measles rate by year by state
usContDisMeasles %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")
# When choosing colors to quantify a numeric variable, we choose between two options, sequential and diverging.
# Sequential palettes are suited for data that goes from high to low
library(RColorBrewer)
display.brewer.all(type = "seq")
#On the other hand, diverging colors are used to represent values that verge from a center
#Avoid Pseudo and Gratuitous 3D Plots
#Avoid Too Many Significant Digits
# tile plot
library(dplyr)
library(ggplot2)
library(dslabs)
the_disease = "Smallpox"
usContDiseasesSmallpox <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))
usContDiseases %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")
# series plot
the_disease = "Smallpox"
usContDiseasesSmallpox <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))
avg <- us_contagious_diseases %>%
  filter(disease==the_disease & weeks_reporting >= 10) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)
usContDiseasesSmallpox %>% 
  ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")
#series plot
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)
us_contagious_diseases %>% 
  filter(state=="California"  & weeks_reporting >= 10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate,color = disease)) + 
  geom_line()
#
us_contagious_diseases %>%
  filter(!is.na(population)) %>%
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color = disease)) +
  geom_line()




#Titanic package

options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
View(titanic)
head(titanic)
typeof(titanic$SibSp)
#Make density plots of age grouped by sex. Try experimenting with combinations of faceting, alpha blending, stacking and using variable counts on the y-axis to answer the following questions. Some questions may be easier to answer with different versions of the density plot
titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2) +
  facet_grid(Sex ~ .)
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack")
titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2)
#QQ-plot of Age Distribution
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
titanic %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()
#barplots
#plot 1 - survival filled by sex
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar()
# plot 2 - survival filled by sex with position_dodge
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())
#plot 3 - sex filled by survival
titanic %>%
  ggplot(aes(Sex, fill = Survived)) +
  geom_bar()
#density plot
#Which age group is the only group more likely to survive than die? 0-8
#Which age group had the most deaths? 18-30
#Which age group had the highest proportion of deaths? 70-80
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)
#boxplot
#Filter the data to remove individuals who paid a fare of 0. Make a boxplot of fare grouped by survival status. Try a log2 transformation of fares. Add the data points with jitter and alpha blending.
titanic %>%
  filter(Fare != 0)%>%
  ggplot(aes(Survived,Fare)) +
  geom_boxplot(alpha = 0.2) +
  scale_y_continuous(trans = "log2")+
  geom_jitter()
#Passengers who survived generally payed higher fares than those who did not survive.
#The median fare was lower for passengers who did not survive.
#Most individuals who paid a fare around $8 did not survive.
# barplot
# basic barplot of passenger class filled by survival
titanic %>%
  ggplot(aes(Pclass, fill = Survived))+
  geom_bar()
# same barplot but use the argument position = position_fill() to show relative proportions in each group instead of counts
titanic %>%
  ggplot(aes(Pclass, fill = Survived))+
  geom_bar(position = position_fill())
# barplot of survival filled by passenger class using position = position_fill()
titanic %>%
  ggplot(aes(Survived, fill = Pclass))+
  geom_bar(position = position_fill())
# There were more third class passengers than passengers in the first two classes combined.
# Survival proportion was highest for first class passengers, followed by second class. Third-class had the lowest survival proportion.
# Most passengers in first class survived. Most passengers in other classes did not survive.
# The majority of those who did not survive were from third class.
# grid of density plots for age, filled by survival status, with count on the y-axis, faceted by sex and passenger class
titanic %>%
  ggplot(aes(Age, fill = Survived, y = ..count..)) +
  geom_density(alpha = 0.2) +
  facet_grid(Pclass~ Sex) 
# The largest group of passengers was third-class males.
# Most first-class and second-class females survived.
# Almost all second-class males did not survive, with the exception of children.
# Properties of starts exercise 
# analyze some actual astronomical data to inspect properties of stars, their absolute magnitude (which relates to a star's luminosity, or brightness), temperature and type (spectral class)
library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)# report 3 significant digits
head(stars)
mean(stars$magnitude)
sd(stars$magnitude)
#  density plot of the magnitude for peaks number check 
stars %>%
  ggplot(aes(magnitude)) +
  geom_density(alpha = 0.2) # two
# distribution of star temperature
stars %>% 
  ggplot(aes(temp)) +
  geom_histogram()# The majority of stars have a low temperature.
#  a scatter plot: temperature x-axis/ magnitude  y-axis/  examine the relationship  Note:  lower magnitude means a more luminous (brighter) star
stars %>% 
  ggplot(aes(temp, magnitude)) +
  geom_point() #decreasing exponential
#astronomers usually transform values of star luminosity and temperature before plotting
stars %>% 
  ggplot(aes(temp, magnitude)) +
  scale_y_reverse()+
  scale_x_continuous(trans = "log10")+
  scale_x_reverse()+
  geom_point()#The brighest, highest temperature stars are in the upper left corner of the plot; For main sequence stars, hotter stars have higher luminosity
#estimate the average temperature of a giant.
estimateGigants<- stars%>%
  filter(temp <10000)
mean(estimateGigants$temp)
# label
stars %>% 
  ggplot(aes(temp, magnitude,label = star)) +
  scale_y_reverse()+
  scale_x_continuous(trans = "log10")+
  scale_x_reverse()+
  geom_label(size = 3, show.legend = TRUE) 
# color point
head(stars)
stars %>%  
  ggplot(aes(temp, magnitude, color = type)) +
  scale_y_reverse()+
  scale_x_continuous(trans = "log10")+
  scale_x_reverse()+
  geom_point()
# Climate Change Exercises
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
head(temp_carbon)
# Which of these code blocks return the latest year for which carbon emissions are reported?
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()
#
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()
#
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()
# What is the first year for which carbon emissions (carbon_emissions) data are available?
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  min()
# How many times larger were carbon emissions in the last year relative to the first year?
lastYear <- temp_carbon %>%
  filter(year == 2014)
firstYear <- temp_carbon %>%
  filter(year == 1751)
proportion<- lastYear$carbon_emissions/firstYear$carbon_emissions
# What is the first year for which global temperature anomaly (temp_anomaly) data are available?
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  pull(year) %>%
  min()
# What is the last year for which global temperature anomaly data are available?
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  pull(year) %>%
  max()
# How many degrees Celsius has temperature increased over the date range? Compare the temperatures in the most recent year versus the oldest year.
lastYear <- temp_carbon %>%
  filter(year == 2018)
firstYear <- temp_carbon %>%
  filter(year == 1880)
difference<- lastYear$temp_anomaly - firstYear$temp_anomaly
# 20th century mean temperature
twCentury <- temp_carbon %>%
  filter(!is.na(temp_anomaly) & year %in% c(1901:2000))
# Create a time series line plot of the temperature anomaly
# Only include years where temperatures are reported. Save this plot to the object p
twCentury %>%
  ggplot(aes(year,temp_anomaly)) +
  geom_line() +
  geom_hline(yintercept = 0, col="blue") +
  ylab("t anomaly in C degrees") +
  ggtitle("t anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue") +
  geom_vline(aes(xintercept=1939),col="red") +
  geom_vline(aes(xintercept=1976),col="green") +
  geom_text(aes(x=1936,y=0.05,label="1939"),col="red") +
  geom_text(aes(x=1978,y=0.05,label="1976"),col="green")
#   line plot
  temp_carbon%>%
    filter(year %in% c(1880:2020)) %>%
    filter(!is.na(temp_anomaly)) %>%
    filter(!is.na(ocean_anomaly))%>%
    filter(!is.na(land_anomaly))%>%
    ggplot() +
    geom_line(aes(x = year,y=temp_anomaly), col = "black")+
    geom_line(aes(x = year,y=ocean_anomaly), col = "blue")+
    geom_line(aes(x = year,y=land_anomaly), col = "green")+ 
    ylab("anomaly")+
    geom_hline(yintercept = 0, col="black",alpha=0.5) +
    geom_vline(xintercept = 2018, col="black",alpha=0.5)
 #line plot
  greenhouse_gases %>%
    ggplot(aes(x= year,y = concentration)) +
    geom_line() +
    facet_grid(gas ~., scales = "free") +
    geom_vline(xintercept = 1850)+ #Add a vertical line with an x-intercept at the year 1850, noting the unofficial start of the industrial revolution and widespread fossil fuel consumption
    ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
    ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")
  #line plot : a time series line plot of carbon emissions
  temp_carbon%>%
    filter(!is.na(carbon_emissions)) %>%
    ggplot() +
    geom_line(aes(x = year,y=carbon_emissions), col = "black")+
    ylab("carbon emissions")
  #a line plot of co2 concentration over time (year), coloring by the measurement source (source)
  co2_time <- historic_co2 %>%
    ggplot(aes(year,co2,color = source))+
    geom_line()
  co2_time_recent <- historic_co2 %>%
    filter(year>1500) %>%
    ggplot(aes(year,co2,color=source)) +
    geom_line()
  # x-axis limits to -800,000 and -775,000. About how many years did it take for co2 to rise from 200 ppmv to its peak near 275 ppmv?
  historic_co2 %>%
    ggplot(aes(year,co2,color=source)) +
    geom_line() +
    xlim(-800000,-775000)
  #Change the x-axis limits to -375,000 and -330,000. About how many years did it take for co2 to rise from the minimum of 180 ppm to its peak of 300 ppmv?
  historic_co2 %>%
    ggplot(aes(year,co2,color=source)) +
    geom_line() +
    xlim(-375000 ,-330000)
  #Change the x-axis limits to -140,000 and -120,000. About how many years did it take for co2 to rise from 200 ppmv to its peak near 280 ppmv?
  historic_co2 %>%
    ggplot(aes(year,co2,color=source)) +
    geom_line() +
    xlim(-140000 ,-120000)
  #Change the x-axis limits to -3000 and 2018 to investigate modern changes in co2. About how many years did it take for co2 to rise from its stable level around 275 ppmv to the current level of over 400 ppmv?
  historic_co2 %>%
    ggplot(aes(year,co2,color=source)) +
    geom_line() +
    xlim(1700,2018)
 