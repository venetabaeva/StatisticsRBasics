#[DF] data frame
#[DS] data set
#[col] column
#[var] variable
#[rw] row
#[obsrv] observation
#[obj] object 
#[objTp] object type
#[dTp] data type
#[objStr] object structure
#[vctr] vector
#[vctrLgc] vector logic 
#[fctrTp] factor data type = categorical data type; 
#[lvlCtDT] levels of categorical data type
#[relOpr] relational operator
#[idnOpr] identical operator
#[lvlLnght] levels lenght
#[tbl] table
#[c] concatenate
#[lnght] lenght
#[lvlNum] levels number 
#[intgr] integer
#[num] numeric

library(tidyverse)
#[DS] access
library(dslabs)
#[DS] load
data(murders)
#[available DF] call; DF =? [different DSs] combined; DF = [DSs = col(var) + rw(obsrv)] stored 
  data()
#[DF] plot
murders %>%
  ggplot(aes(population, total, label=abb, color=region)) +
  geom_label()

#[objTp] determine = [daTp] check 
class(murders)
class(murders$state)
#[objStr] show 
str(murders)
#[DF 1st 6 obsv] show
head(murders)
#[var = col] get  
names(murders)
#[var = col = vctr] access; preserved is the row order in the DF
murders$population
murders[["population"]]
#[var = DF] access; subset of the original DF containing just this col
murders["population"]
#[obj = vctr] -> obsrv(entry)
#[lnght] check
pop<-murders$population
length(pop)
#[vctrLgc] define & check; [objTp] determine 
#[relOpr] checktable
z<- 3==2
z
class(z)
#[idnOpr] check
identical(a,b)
#[fctrTp] show data type 
class(murders$region)
#[lvl] show
levels(murders$region)
#[lvlLnght] show
length(levels(murders$region))
#[lvlNum] show
nlevels(murders$region)
#[tbl]takes a input - vctr; returns frq of each unique element in the vctr
#[c] 
x <- c("a", "a", "b", "b", "b", "c")
table(x)
#[tbl]show number of states per region
table(murders$region)
#[tbl]show number of states per region
numState<- murders$state
catRegion<-murders$region
table(numState,catRegion)
#[num] check it is numeric
class(3)
#[intgr] confirm it is an integer
class(3L)
#[vtr]create
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
temp <- c(35, 88, 42, 84, 81, 30)
#[DF] create 
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)
#[help on]call  
help("+")