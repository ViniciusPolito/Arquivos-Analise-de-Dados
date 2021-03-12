qnorm(0.9, mean = 90, sd = 21)
  
pnorm(116.9126, mean = 90, sd = 21)

x <- 30.2
d <- 3.8
nc <- (1-0.95)/2
n <- 100
error <- d/sqrt(n)
left <- x - (qnorm(nc, lower.tail = F)*error)
right <- x + (qnorm(nc, lower.tail = F)*error)
cat("[", left, "-", right, "]")



d <- sd(c(4.37, 3.63, 2.78, 5.46, 2.18, 6.07, 3.24, 5.89, 4.86,4.64))
x <- mean(c(4.37, 3.63, 2.78, 5.46, 2.18, 6.07, 3.24, 5.89, 4.86,4.64))
n <- 10
nc <- (1-0.9)/2
error <- d/sqrt(n)
left <- x - (qnorm(nc, lower.tail = F)*error)
right <- x + (qnorm(nc, lower.tail = F)*error)
cat("[", left, "-", right, "]")