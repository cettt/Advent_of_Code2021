data20 <- c("." = 0L, "#" = 1L)[strsplit(readLines("Input/day20.txt", n = 1), "")[[1]]]
map0 <- read.fwf("Input/day20.txt", widths = rep(1, 100), skip = 2, comment.char = "")

map <- matrix(0, 202, 202)
map[52:151, 52:151] <- c("." = 0L, "#" = 1L)[unlist(map0)]

map_k <- function(k, n) {
  res <- k + c(if (k > n) -n, 0L, if (k <= n^2L - n) n)
  c(if (k %% n != 1L) res - 1L, res, if (k %% n != 0L) res + 1L)
}

lookup <- lapply(seq_along(map), map_k, n = 202L)

pow2 <- 2L^(8:0)
for (r in seq_len(50)) {
  i_idx <- as.integer(matrix(seq_len(202^2), 202)[(52 - r):(151 + r), (52 - r):(151 + r)])
  map[i_idx] <- data20[colSums(matrix(map[unlist(lookup[i_idx])]*pow2, nrow = 9)) + 1]
  map[-i_idx] <- data20[map[1] * 511L + 1L]
  if (r == 2) part1 <- sum(map)
}

#part 1----
part1

#part 2----
sum(map)
