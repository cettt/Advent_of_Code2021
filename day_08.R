data08 <- strsplit(readLines("Input/day08.txt"), "\\W+")

#part1---------
sum(sapply(data08, \(z) sum(nchar(z[-(1:10)]) %in% c(2:4, 7))))

#part2-----------
find_digits <- function(x) {
  y <- paste0("[^", sapply(c(2,4,7), \(k) x[sapply(x, nchar) == k][1]), "]")
  tmp <- apply(sapply(y, \(z) nchar(gsub(z, "", x[11:14]))), 1, prod)
  map_vec <- c(36, 8, 10, 30, 32, 15, 18, 12, 56, 48)
  sum(sapply(tmp, \(k) which(map_vec == k) - 1) * 10^(3:0))
}

sum(sapply(data08, find_digits))

#Explanation part 2------
#credit to /u/4HbQ:
#   https://www.reddit.com/r/adventofcode/comments/rbj87a/2021_day_8_solutions/hnp4saz/?utm_source=share&utm_medium=web2x&context=3 asv
#we first find 1,4 and 8 by counting letters and transform into a regex (y)
#next for the last four digits we check how many segments they have in common with 1,4 and 8.
# For each digit these three numbers are multiplied:
# the vector map_vec contains these mappings:
#   e.g for 0 we get 2 intersections with 1, 3 intersections with 4 and 6 intersections with 8
#   transforming these values gets  2*3*6 = 36 which is the first entry of map_vec
#We use the mapping to get the actual value of the digit and produce the output.
### full mapping
## #check with 1 4 and 8 (intersect)
#0 2 3 6
#1 2 2 2
#2 1 2 5
#3 2 3 5
#4 2 4 4
#5 1 3 5
#6 1 3 6
#7 2 2 3
#8 2 4 7
#9 2 4 6
