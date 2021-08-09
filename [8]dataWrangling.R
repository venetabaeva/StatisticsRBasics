#[P] package
#[F] function
#[mtu] mutate
#[DT]data table
#[fltr] filter
#[slct] select
#[dplyrP] works with tables 
library(dplyr)
#[mtuF] change DT ; add e new clmn, or change an existing clmn; takes  the DF as 1st arg  and the name  and value of vrb in the 2nd arg
murders <- mutate(murders,rate=total/population*100000)
head(murders)
#[mtuF] change DT
x <- c(88, 100, 83, 92, 94)# Note that if you want ranks from highest to lowest you can take the negative and then compute the ranks 
rank(-x)
rate <-  murders$total/ murders$population * 100000# Defining rate
murders <- mutate(murders,rank=rank(-rate))# Redefine murders to include a column named rank# with the ranks of rate from highest to lowest
#[fltr] filter data by subsetting rw; takes  the DF as 1st arg  and the conditional statement as the 2nd 
filter(murders,rate <= 0.71)
#[fltr] filter data by subsetting rw;
murders <- mutate(murders, rate = total/population * 100000, rank = rank(-rate))# Add the necessary columns
filter(murders,rate,rank <=5)# Filter to show the top 5 states with the highest murder rates
#[fltr] filter data by subsetting rw;
no_south <- data.frame(filter(murders,region!="South"))# Use filter to create a new data frame no_south
nrow( no_south) # Use nrow() to calculate the number of rows
#[fltr] filter data by subsetting rw;
murders_nw <- data.frame(filter(murders,region %in% c("Northeast","West")))# Create a new data frame called murders_nw with only the states from the northeast and the west
nrow(murders_nw)# Number of states (rows) in this category 
#[fltr] filter data by subsetting rw;
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))# add the rate column
my_states <- filter(murders,region %in% c("Northeast","West") & rate < 1)# Create a table, call it my_states, that satisfies both the conditions 
select(my_states,state,rate,rank)# Use select to show only the state name, the murder rate and the rank
#[slct] subset the data; select specific clmn; 
newTable <- select(murders,state,region,rate) 
filter(newTable,rate <= 0.71)
#[%>%] pipe operator  
murders %>% select(state,region,rate) %>% filter(rate <= 0.71)
#[%>%] pipe operator  
library(dplyr)# Load library
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))## Define the rate column
filter(murders, region %in% c("Northeast", "West") & rate < 1 )%>% # show the result and only include the state, rate, and rank columns, all in one line, in that order
  select(state, rate, rank)
#[%>%] pipe operator  
library(dplyr)# Loading the libraries # Create new data frame called my_states (with specifications in the instructions)
data(murders)
my_states <- murders %>% mutate(rate=total/murders$population*100000, rank=rank(-rate)) %>% filter(region %in% c('Northeast','West') & rate <1) %>% select(state,rate,rank)