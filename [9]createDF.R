grades <- data.frame(names=c("John","Juan","Jean","Yao"),
                     exam1 = c(95,80,90,85),
                     exam2 = c(90,85,85,90))
grades
#[DF] data.frame turns by default characters into factors
class(grades$names)
#[DF] define special argument 
grades <- data.frame(names=c("John","Juan","Jean","Yao"),
                     exam1 = c(95,80,90,85),
                     exam2 = c(90,85,85,90),
                     stringsAsFactors = FALSE)
grades
class(grades$names)

