data10 <- readLines("Input/day10.txt")

p1 <- c(")" = 3L, "]" = 57L, "}" = 1197L, ">" = 25137L)
p2 <- c("(" = 1L, "[" = 2L,  "{" = 3L,    "<" = 4L)

pat <- "<>|\\{\\}|\\[\\]|\\(\\)"
while (any(grepl(pat, data10))) data10 <- gsub(pat, "", data10)
x <- strsplit(data10, "")

#part1---------
sum(sapply(x, \(y) p1[y[y %in% names(p1)][1]]), na.rm = TRUE)

#part2------
median(sapply(x, \(y) max(filter(p2[rev(y)], 5, method = "rec"))), na.rm = TRUE)

# the idea is to remove matching pairs (), [], {}, <> until there are none left
# for part1 we map the first closing parentheses to a score (p1) and take the sum
# for part2 we map all opening brackets to a score (p2) in reverse order.
#  the score is then computed using a linear filter
