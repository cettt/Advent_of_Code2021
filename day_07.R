data07 <- as.integer(read.table("Input/day07.txt", sep = ",")[1,])

#part1------
sum(abs(data07 - median(data07)))

#part2----------
x <- floor(mean(data07)) + 0:1
min(sapply(x, \(z) sum(abs((data07 - z)) * (abs(data07 - z) + 1) / 2)))
