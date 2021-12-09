data09 <- unlist(read.fwf("Input/day09.txt", widths = rep(1, 100)))

map_k <- function(k) {
  m <- k %% 100
  k + c(if (k < 9901) 100, if (k > 100) -100, if (m != 1) -1, if (m != 0) 1)
}

#part1-----
basin_idx <- which(data09 < sapply(seq_along(data09), \(k) min(data09[map_k(k)])))
sum(data09[basin_idx] + 1L)

#part2-----
basin_size <- function(queue) {
  j <- 1L
  while (j <= length(queue)) {
    x <- map_k(queue[j]) #neighbor edges
    queue <- c(queue, setdiff(x[data09[x] < 9L], queue))
    j <- j + 1L
  }

  return(length(queue))
}

prod(sort(sapply(basin_idx, basin_size), decreasing = TRUE)[1:3])

#the function map_k returns for given index k the indices of neighbor cells.
#the function basin size returns the size of a basin given the index of the lowest part.
#   it relies on a BFS-style algorithm: #
#     in each step we determine new edges and check which of these edges we already visited
#     we repeat this procedure until no new edges are found
