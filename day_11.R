data11 <- setNames(unlist(read.fwf("Input/day11.txt", rep(1, 10))), 1:100)

z <- complex(real = rep(1:10, each = 10), imaginary = rep(1:10, 10))
lookup <- lapply(seq_along(data11), \(k) which(abs(z - z[k]) < 2))

n <- 0L
rounds <- 0L

while (any(data11 != 0L)) {
 rounds <- rounds + 1L
 data11 <- data11 + 1L

 while (any(data11 >= 10L)) {
   idx <- table(unlist(lookup[data11 >= 10L]))
   data11[data11 >= 10] <- -Inf
   data11[names(idx)] <- data11[names(idx)] + idx
 }
 n <- n + sum(is.infinite(data11))
 if (rounds == 100L) part1 <- n
 data11[is.infinite(data11)] <- 0L
}

#part1-------
part1

#part2-----
rounds
