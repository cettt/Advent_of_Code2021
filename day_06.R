data06 <- read.table("Input/day06.txt", sep = ",")[1,]
x <- as.numeric(sapply(0:8, \(i) sum(data06 == i)))

#part 1---------
for (i in seq_len(80)) x <- c(x[-1] + c(rep(0, 6), x[1], 0), x[1])
sum(x)

#part 2--------
for (i in seq_len(256 - 80)) x <- c(x[-1] + c(rep(0, 6), x[1], 0), x[1])
print(sum(x), 12)
