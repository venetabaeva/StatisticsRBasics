#A vector is a series of values, all of the same type. They are the most basic data type in R and can hold numeric data, character data, or logical data. 
#[cVtr] create vtr through concatenate
codes<-c(380,124,818)
#[chrtVtr]create character vtr
country<-c("italy","canada","egypt")
#[assctVtr] associate vtrs
codesC<-c(italy=380,canada=124,egypt=818)
codesC<-c("italy"=380,"canada"=124,"egypt"=818)
#[nmsVtr]assign names to entries of vtr
names(codesC)<-country 
#[seq] show sequence
seq(1,10)
seq(1,10,2)
1:10
#[sbstVtr]access[] specific part (element) of a vtr through subset
codesC[2]
#[sbstVtrC]access[] specific part (element) of a vtr through subset
codesC[c(1,3)]
#[sbstVtrC]access[] specific part (element) of a vtr through subset
codesC[1:2]
#[sbstVtrC]access[] specific part (element) of a vtr through names
codesC["canada"]
codesC[c("egypt","italy")]
#[lngthSeqVtr]
length(seq(6,55,4/7))
#[lngthSeqVtr] generate seq increasing by the same amount but  of the prespecified lngth
seq(1, 10, length.out = 100)
#[srtVtr] sort vtr in increasing order 
data(murders)
sort(murders$total)
#[ordrVtr] return the indices that sort the vtr parameter Note: sort(x) = x[order(x)]
x <- c(31, 4, 15, 92, 65)
sort(x)    # puts elements in order
index <- order(x)   # returns index that will put x in order
x[index]# rearranging by this index puts elements in order
order(x)
#[ordrVtr] return the indices that sort the vtr parameter
index<-order(murders$total)
murders$abb[index]
#[maxOrdrVtr] return the indices that sort the vtr parameter
max(murders$total)
iMax<-which.max(murders$total)
murders$state[iMax]
#[rankOrderVtr]
x <- c(31, 4, 15, 92, 65)
rank(x)
#[rankOrderVtr]
states <- murders$state # Define a variable states to be the state names from the murders data frame
ranks <- rank(murders$population)# Define a variable ranks to determine the population size ranks 
ind <- order(murders$population)# Define a variable ind to store the indexes needed to order the population values
my_df <- data.frame(states = states[ind], ranks = ranks[ind])# Create a data frame my_df with the state name and its rank and ordered from least populous to most 
