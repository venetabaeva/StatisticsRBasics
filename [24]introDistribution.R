library(dslabs)
#data() #check all available data sets 
data(heights)
names(heights)#extract the variable names from a dataset
head(heights)
#discrete numeric data can be considered ordinal
#for variables belonging to a small number of different groups, with each group having many members
unique(heights$height) #how many unique values are used by the heights variable
length(heights$height)
length(unique(heights$height)) # how many unique (numeric ordinal) variables are there 
tab <- table(heights$height) #compute the frequencies # of each unique value 
sum(tab==1) #count the number of times a unique value appears
#with categorical data, the distribution describes the proportions of each unique category 
prop.table(table(heights$sex)) # proportion of each unique value = frequency table = distribution 
#to define a distribution for numerical data is more effective to define a function that reports the proportion of the data below a value A for all possible values of A
# cumulative distribution function (CDF)  F (a) = Pr (x</=a)  <- proportion of data below a given value a 
# any continuous dataset has a CDF, not only normal distribution 
# NB: male heights(continuous data type) on x and proportion on y axis 
# calculating probability related to continuous dataset not equal to calculate probability of a specific exact value -> not informative because each value is unique
# note: histograms are preferred rather than ECDF
# histogram = divide span of data into non - overlapping bins of the same size; for each bin count the number of values that fall in that interval; plot these counts as bars with the base of teh bar the interval 
# Note: histogram is an approximation -> all values in each interval are treated as the same when computing the bin heights
rangeForCdfFunction <- seq(min(heights$height), max(heights$height),length = 100) #define range of values spaning the dataset
cdfFunction <- function(f){ # computes probability for a single values 
  mean(heights$height<=f) # cdfFunction (rangeForCdfFunction) = Pr (f</=rangeForCdfFunction) 
}
#CDF defines proportion of data below a cutoff - rangeForCdfFunction
cdfValues <- sapply(rangeForCdfFunction,cdfFunction ) #  CDF defines proportion of data below the  cutoff rangeForCdfFunction
plot(rangeForCdfFunction,cdfValues) 
# defines proportion of data above the  cutoff rangeForCdfFunction => 1 - cdfFunction (rangeForCdfFunction)
cdfValuesAbove <- 1-(sapply(rangeForCdfFunction,cdfFunction))
plot(rangeForCdfFunction,cdfValuesAbove) 
# defines proportion of data between the  cutoff rangeForCdfFunction => 1 - cdfFunction (rangeForCdfFunction)
rangeForCdfFunctionQ <- seq(quantile(dfAlc$employrt,0.50), quantile(dfAlc$employrt,0.75),length = 100)
cdfValuesBellowQ <- sapply(rangeForCdfFunctionQ,cdfFunction) - (sapply(rangeForCdfFunction,cdfFunction))# define proportion of values between rangeForCdfFunction and rangeForCdfFunctionQ
plot(rangeForCdfFunctionQ,cdfValuesBellowQ)
# smooth density plot are similar to histograms
# smooth density plot lacks sharp edges and the scale of y-axis changes from counts to densities 
# smooth density is understood through estimates 
# estimates -> assumed is that the list of observed values come from much larger list of unobserved values (population)
# smooth = many small bins 
# problem: to not make the curve dependent on the hypothetical size of the hypothetical list -> compute the curve on the frequency scale rather than count scale
# problem: no millions measurements => cannot make a histogram with small bins => how we can estimate the hypothetical smooth curve that will be seen if we have all measurements? => 
# histogram with available data -> compute frequencies -> use bins appropriate for the data -> mark with points the top of each bin -> remove bins -> drow curve accross the points 
# NB! smooth density is based on relative assumptions and choices -> control smoothness of teh curve  that defines a smooth density through an option in the function, ggplot option that computes the smooth density 
# NB! select degree of smoothness that can be defend is representative of the underlying data -> example: do have reson to believe that proportion of people with similar heights should be teh same =  proportion that is 72 inches should be more similar to the proportion of 71 rather than 78, or 65
# interpreting the y - axis of smooth density plot 
# y - axis is scaled so that the area under the density curve adds up to 1 => if a bin has a base 1, then we can say that the value on the y -axis tells us the exact proportion of values in this bin 
# NB! for other bin sizes -> compute the proportion of the total area contained in that interval 
# normal distribution = Gaussian distribution = bell curve = probability to have x value between value a and value b using parameters mean and standard deviation
# why using the normal distribution -> rather than using data, the normal distribution is defined with a mathematical formula 
# normal distribution <- 95% are within 2SD from the average 
# => if a dataset is approximated by a normal distribution, then to describe the distribution only the average and the standard deviation are needed 
 mean1 <- sum(heights$height)/ length(heights$height)
 sd1 <- sqrt(sum((heights$height-mean)^2)/ length(heights$height))
 index1 <- heights$sex == "Male"
 xMHeight <- heights$height[index1]
 mean2 <- mean(xMHeight)
 sd2 <- sd(xMHeight)
 c(mean1=mean1,sd1=sd1)
 #standard units = z scores 
 zMHeight = scale(xMHeight)
 mean(abs(zMHeight)<2) # count the number of z scores less than 2 and bigger than negative 2 and divide by total ; take the mean 
 #The normal distribution:
#Is centered around one value, the mean
 #Is symmetric around the mean
 #Is defined completely by its mean (μ) and standard deviation ( σ )
 #Always has the same proportion of observations within a given distance of the mean (for example, 95% within 2 σ)
 #The standard deviation is the average distance between a value and the mean value.
 #Calculate the mean using the mean() function.
 #Calculate the standard deviation using the sd() function or manually. 
 #Standard units describe how many standard deviations a value is away from the mean. The z-score, or number of standard deviations an observation x is away from the mean μ:
 # z= x-μ/σ
 #Compute standard units with the scale() function.
 #Important: to calculate the proportion of values that meet a certain condition, use the mean() function on a logical vector. Because TRUE is converted to 1 and FALSE is converted to 0, taking the mean of this vector yields the proportion of TRUE.
 #The built-in R function sd() calculates the standard deviation, but it divides by length(x)-1 instead of length(x). When the length of the list is large, this difference is negligible and you can use the built-in sd() function. Otherwise, you should compute σ by hand. For this course series, assume that you should use the sd() function unless you are told not to do so.




