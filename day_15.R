data15 <- as.integer(unlist(read.fwf("Input/day15.txt", rep(1, 100))))

map_k <- function(k, n = 100L) {
  m <- k %% n
  k + c(if (k <= n^2L - n) n , if (k > n) -n, if (m != 1L) -1L, if (m != 0L) 1L)
}

find_shortest_path_c <- function(m) {

  map <- matrix(sapply(1:m, \(k) data15 + k), nrow = 100) #copy horizontally
  map <- as.integer(Reduce(rbind, lapply(1:m - 3L, \(k) map + k)) %% 9L + 1L) # copy vertically

  lookup <- lapply(seq_along(map), map_k, n = m*100L)
  q <- collections::priority_queue(1L, priorities = 0L)

  risk <- c(0L, rep.int(10000L, length(map) - 1L))

  while (q$size() > 0) {
    cur <- q$pop()
    cur_risk <- risk[cur]
    for (ne in lookup[[cur]]) {
      nr <- cur_risk + map[ne]
      if (risk[ne] > nr) {
        risk[ne] <- nr
        q$push(ne, priority = -nr)
      }
    }
  }
  return(risk[m^2*1e4])
}

#part1-------
find_shortest_path_c(1L)

#part2-----
find_shortest_path_c(5L)
