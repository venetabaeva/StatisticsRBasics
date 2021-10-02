murders$state[which.max(murders$population)]
max(murders$population)
#murders per capita
#[vtrArthmc] operations on vectors occur element-wise 
murder_rate <- murders$total/murders$population*100000 #murder rate = murders for every 100 000 people element - wise (one by one)
murders$state[order(murder_rate,decreasing=TRUE)]#states ordered by murder rate
#[vtrArthmc] operations on vectors occur element-wise 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")# Assign city names to `city` 
temp <- c(35, 88, 42, 84, 81, 30)# Store temperature values in `temp`
temp <- 5/9 * (temp-32)# Convert temperature into Celsius and overwrite the original values of 'temp' with these Celsius values
city_temps <- data.frame(name = city, temperature = temp)# Create a data frame `city_temps` 
str(city_temps) 
#[vtrArthmc] operations on vectors occur element-wise 
x <- c(1:100)# Define an object `x` with the numbers 1 through 100
sum(1/x^2)# Compute the sum 
#[vtrArthmc] operations on vectors occur element-wise 
x <- c(2, 43, 27, 96, 18) 
sort(x)
order(x)
rank(x)#gives the ranks from lowest to highest
rank(-x)#gives the ranks from highest to lowest
min(x)
which.min(x)
max(x)
which.max(x)
#[vtrArthmc] operations on vectors occur element-wise 
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
timeConverted <- time/60
speed <- distance/timeConverted
print(speed)
df <- data.frame(name = name, time = timeConverted,distance = distance, speed =speed )
str(df)




