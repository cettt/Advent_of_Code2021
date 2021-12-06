data02 <- read.table("Input/day02.txt", sep = " ")
x <- data02[,2] * c(up = -1i, down = 1i, forward = 1)[data02[,1]]

#part1----------
sum(Re(x)) * sum(Im(x))

#part2--------
sum(Re(x)) * sum(cumsum(Im(x)) * Re(x))
