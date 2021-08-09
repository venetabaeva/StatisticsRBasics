#Scope:  find the solutions to an equation ax^2 + bx + c 
#Info: a <-2 ; b<- 1; c<- 4
#Use: quadratic equation[qdrEq]:  x= (-b+-sqrt(b^2-4ac))/2a 

#[qdrEq] create
#1st
a<-2
b<-1
c<-4
qdrEq<-function(a,b,c){
  print(paste0("You have chosen the quadratic equation ", a, "x^2 + ", b, "x + ", c, "."))
  discriminant <- (b^2) - (4*a*c)

if(discriminant<0){
  return(paste0("This quadratic equation has no real numbered roots."))
}else if (discriminant>0){
  xPos<- (-b+sqrt(discriminant))/(2*a)
  xNeg<- (-b-sqrt(discriminant))/(2*a)
    return(paste0("This two x-intercepts for the quadratic equation are",
                  format(round(xPos,5),nsmall = 5)," and",
                  format(round(xNeg,5),nsmall = 5)," ."))
}else 
  xZero<- (-b)/(2*a)
  return(paste0("This quadratic equation has only one root.This root is",
                xZero ))
}
qdrEq(a,b,c)

#2nd

quadratic_function <- function(a, b, c){ 
  pos_root <- ((-b) + sqrt((b^2) - 4*a*c)) / (2*a) 
  neg_root <- ((-b) - sqrt((b^2) - 4*a*c)) / (2*a) 
  print(pos_root) 
  print(neg_root) 
} 
quadratic_function(2,1,4)

#3rd

factorial <- function(a, b, c){
  pos_x <- (-1 * b + sqrt(as.complex(b^2 - 4 * a * c))) / (2 * a)
  neg_x <- (-1 * b - sqrt(as.complex(b^2 - 4 * a * c))) / (2 * a)
  c(pos_x, neg_x)
}

factorial(2,1,4)