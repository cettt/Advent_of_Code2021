data06 <- read.table("Input/day06.txt", sep = ",")[1,]
x <- as.numeric(sapply(0:8, \(i) sum(data06 == i)))

A <- diag(9)[c(2:9, 1), ]
A[7,1] <- 1

#part 1---------
sum(Reduce(`%*%`, rep(list(A), 80)) %*% x)

#part 2--------
sprintf("%.f", sum(Reduce(`%*%`, rep(list(A), 256)) %*% x))

#Reduce(`%*%`, rep(list(A), 80)) is the base-R version of calculating matrix powers
#Alternatively we could use the %^% from the expm package: A %^% 80
#x contains the number of fish with remaining time of 0 to 8 days
#x %*% A is exactly the new number of fish in each cycle after one day.
# the solution therefore is only sum(x %*% A^n)
