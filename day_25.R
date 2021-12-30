data25 <- as.character(do.call(rbind, strsplit(readLines("Input/day25.txt"), "")))

neighbor_k <- function(k) {
  m <- k %% 137L
  res <- k + c( -1L + if (m == 1L) 137L else 0L, 1L - if (m == 0) 137 else 0L)
  c(res, (k - 138L) %% (137L * 139L) + 1L, (k + 136L) %% (137L * 139L) + 1L)
}

lu <- sapply(seq_along(data25), neighbor_k)
map0 <- unname(c(">" = 1L, "v" = 2L, "." = 0L)[data25])

update_map <- function(x) {
  x1 <- ifelse(x == 0L & x[lu[3, ]] == 1L, 1L, ifelse(x == 1L & x[lu[4, ]] == 0L, 0L, x))

  ifelse(x1 == 0L & x1[lu[1,]] == 2L, 2L, ifelse(x1 == 2L & x1[lu[2, ]] == 0L, 0L, x1))
}

for (steps in 1:1e3) {
  map1 <- update_map(map0)
  if (all(map1 == map0)) break
  map0 <- map1
}
steps
