#part 1 and 2---------
sapply(c(1, 3), \(k) sum(diff(read.table("Input/day01.txt")[,1], lag = k) > 0))
