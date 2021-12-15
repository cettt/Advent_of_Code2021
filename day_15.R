data15 <- as.integer(unlist(read.fwf("Input/day15.txt", rep(1, 100))))

map_k <- function(k, n = 100L) {
  m <- k %% n
  k + c(if (k <= n^2 - n) n , if (k > n) -n, if (m != 1L) -1L, if (m != 0L) 1L)
}

find_shortest_path <- function(map, n = length(map)) {

  lookup <- lapply(seq_along(map), map_k, n = sqrt(n))
  unvisited <- seq_along(map)
  risk <- c(0L, rep(10000L, length(map) - 1L))

  while (risk[n] >= 10000L) { #find fastest path
    x <- risk[unvisited]
    cur_risk <- min(x)
    idx <- x == cur_risk #position of non-visited vertices with lowest risk
    new_edges <- unlist(lookup[unvisited[idx]]) #find all adjacent vertices
    risk[new_edges] <- pmin(risk[new_edges], cur_risk + map[new_edges]) #update accumulated risk
    unvisited <- unvisited[!idx] #vertices with lowest risk are now visited
  }
  return(risk[n])

}

#part1-------
find_shortest_path(data15)

#part2-----
map5 <- matrix(sapply(0:4, \(k) data15 + k), nrow = 100) #copy horizontally
map5 <- as.integer(Reduce(rbind, lapply(0:4 - 1L, \(k) map5 + k)) %% 9 + 1) # copy vertically
find_shortest_path(map5)


#alternatively we can use the igraph package to speed things up----
# using igraph is about 10 times faster
# find_shortest_path <- function(map, n = sqrt(length(map))) {
#   gr <- igraph::make_lattice(dim = 2, length = n, directed = T, mutual = T)
#   igraph::distances(gr, 1, length(map), "out", weights = map[igraph::get.edgelist(gr)[, 2]])
# }
