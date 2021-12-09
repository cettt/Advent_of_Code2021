data09 <- unlist(read.fwf("Input/day09.txt", widths = rep(1, 100)))

idx_map <- function(k) {
  m <- k %% 100L
  res <- c(k + 100 * c(1, -1), k + if (m > 1) c(1, -1) else if (m == 1) 1 else -1)
  res[res > 0 & res < 10001]
}

#part1-----
basin_idx <- which(data09 < sapply(seq_along(data09), \(k) min(data09[idx_map(k)])))
sum(data09[basin_idx] + 1L)

#part2-----
basin_sign <- function(queue) {
  j <- 1L

  while (j <= length(queue)) {
    x <- idx_map(queue[j]) #neighbor edges
    new_edge <- setdiff(x[data09[x] > data09[queue[j]] & data09[x] < 9L], queue)
    queue <- c(queue, new_edge)
    j <- j + 1L
  }

  return(length(queue))
}

prod(sort(sapply(basin_idx, basin_sign), decreasing = TRUE)[1:3])
