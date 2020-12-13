getwd()

setwd("C:/Users/Pc/Desktop/R")

install.packages("seqinr")

library("seqinr")

aux1 <- 10

aux2 <- 15

aux3 <- 5

ls()

rm(list=ls())

exp(1)
exp(log(5))
sin(pi/2)
cos(pi/2)
max(2,9,3,6)
min(4,8,3,2)
sum(4,2,3,5)
prod(5,6,8,2)
sqrt(25)
factorial(5)

help(mean)#documentação
?mean #documentação
example(mean)#exemplos
help.search("mean")#documentação
help.start()#documentação

X <- 133
Y <- 36
Z <- X + Y
round(log(sqrt(Z)), digits =3)

log(7)

cos(2)

factorial(10)

c = 300000000
m = 10
E=m*c^2

v<-c(2,5,7,8,9)

func = function (x) {
  
  return (x^3 + x^2 + x)
  
}

func(v)
