data14 <- read.table("Input/day14.txt", sep = ".")[,1]

.a <- sapply(strsplit(data14[-1], ""), \(y) paste0(y[c(1, 1, 7)], y[c(2, 7, 2)]))
A <- apply(.a, 2, \(y) ifelse(.a[1,] %in% y[-1], 1L, 0L))
x <- sapply(.a[1,], \(y) sum(gregexpr(y, data14[1])[[1]] > 0))

y <- sapply(c(10, 40), \(n) c(1, Reduce(`%*%`, rep(list(A), n)) %*% x)) #add 1 for the first letter
res <- aggregate(y, list(c(substr(data14[1], 1, 1), sub(".", "", .a[1,]))), sum)

#part1--------
diff(range(res[,2]))

#part2----
sprintf("%.f", diff(range(res[,3])))


#the idea is not to consider the polymer as a whole but to model the evolution of each pair individually
# x is a list with the number of all initial pairs
# after each step the new number of pairs is given by A %*% x
# in order to count the elements in the result we only consider the last letter of each pair.
# In order not to miss the very first letter of the final string we add it to res
