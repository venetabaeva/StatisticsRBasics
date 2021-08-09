#use logical operators to index vtr
murder_rate <- murders$total/murders$population*100000 
#[T/F] check values' index 
ind <- murder_rate < 0.71
ind <- murder_rate <=0.71
ind
murders$state[ind]
#[T/F] count how many
sum(ind)
#[&] filter
west<- murders&region == "West"
safe <- murder_rate <= 1 #Store the `murder_rate <= 1` in `safe` 
ind <- safe&west
murders$state[ind]
#[which] give entries of a logical vtr that are TRUE 
x <- c(FALSE,TRUE,FALSE,TRUE)
which(x)
#[which] give entries of a logical vtr that are TRUE 
ind<-which(murders$state == "Massachusetts")
ind
murder_rate[ind]
##optional 
ind<-murders$state == "Massachusetts"
murder_rate[ind]
#[match] look for entries in a vtr; return the ind needed to access the entries 
ind <- match(c("New York","Florida","Texas"),murders$state)
ind
murders$state[ind]
murder_rate[ind]
#[%in%] check whether  each element of a 1st vtr is in a 2nd vtr 
x<- c("a","b","c")
y <- c("a","b","f")
y %in% x
#[%in%] check whether  each element of a 1st vtr is in a 2nd vtr 
c("Boston","Dakota","Washington") %in% murders$state
#[%in%] check whether  each element of a 1st vtr is in a 2nd vtr 
abbs <- c("MA", "ME", "MI", "MO", "MU") # Store the 5 abbreviations in abbs. (remember that they are character vectors)
ind <- which(!abbs%in%murders$abb)# Use the `which` command and `!` operator to find out which index abbreviations are not actually part of the dataset and store in `ind`
abbs[ind] # Names of abbreviations in `ind`



