data20 <- c("." = 0L, "#" = 1L)[strsplit(readLines("Input/day20.txt")[1], "")[[1]]]
map0 <- read.fwf("Input/day20.txt", widths = rep(1, 100), skip = 2, comment.char = "")

map <- matrix(0, 202, 202)
map[52:151, 52:151] <- c("." = 0L, "#" = 1L)[unlist(map0)]
map <- as.integer(map)

map_k <- function(k, n) {
  res <- k + c(if (k > n) -n, 0L, if (k <= n^2 - n) n)
  c(if (k %% n != 1) res - 1L, res,  if (k %% n != 0L) res + 1L)
}

lookup <- lapply(seq_along(map), map_k, n = 202L)

pix_vec <- integer(50)

for (k in seq_len(50)) {
  i_idx <- as.integer(matrix(seq_along(map), 202)[(52 - k):(151 + k), (52 - k):(151 + k)])

  map[i_idx] <- sapply(i_idx, \(k) data20[sum(map[lookup[[k]]]*2^(8:0)) + 1L])
  map[-i_idx] <- if (map[1] == 0L) data20[1] else data20[512]
  pix_vec[k] <- sum(map)
}

#part 1----
pix_vec[2]

#part 2----
pix_vec[50]
