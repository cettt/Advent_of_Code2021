data08 <- strsplit(readLines("Input/day08.txt"), "\\W+")

#part1---------
sum(sapply(data08, \(z) sum(nchar(z[-(1:10)]) %in% c(2:4, 7))))

#part2-----------
find_digits <- function(x) {
  y <- paste0("[^", sapply(c(2,4,7), \(k) x[sapply(x, nchar) == k][1]), "]")
  tmp <- apply(sapply(y, \(z) nchar(gsub(z, "", x[11:14]))), 1, \(z) sum(z * 3^(2:0)))
  map_vec <- c(33, 26, 20, 32, 34, 23, 24, 27, 37, 36)
  sum(sapply(tmp, \(k) which(map_vec == k) - 1) * 10^(3:0))
}

sum(sapply(data08, find_digits))

#Explanation part 2------
#credit to /u/4HbQ
#we first find 1,4 and 8 by counting letters (y)
#next for the last four digits we see how many segments it has in common with 1,4 and 8.
# For each digit these three numbers are converted using sum(z * 3^(2:0))
# the vector map_vec contains these mappings:
#   e.g for 0 we get that 2 intersections with 1, 3 intersections with 4 and 6 intersections with 8
#   transforming these values gets 2*3^2 + 3*3^1 + 6 = 33 which is exactly map_vec[1]
#We use the mapping to get the actual value of the digit and produce the output.
