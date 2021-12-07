data06 <- read.table("Input/day06.txt", sep = ",")[1,]
x <- as.numeric(sapply(0:8, \(i) sum(data06 == i)))

A <- diag(9)[, c(2:9, 1)]
A[1,7] <- 1

#part 1---------
sum(x %*% Reduce(`%*%`, rep(list(A), 80)))

#part 2--------
print(sum(x %*% Reduce(`%*%`, rep(list(A), 256))), 12)
#x %*% A is exactly the timings after one unit of time passes
