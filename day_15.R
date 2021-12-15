data15 <- unlist(read.fwf("Input/day15.txt", rep(1, 100)))

map_k <- function(k, n = 100) {
  m <- k %% n
  k + c(if (k <= n^2 - n) n , if (k > n) -n, if (m != 1) -1, if (m != 0) 1)
}

lookup <- lapply(seq_along(data15), map_k, n = 100)

find_shortest_path <- function(.to, lu = lookup, maze = data15) {
  queue <- 1L
  risk_vec <- 0L
  visited <- integer()
  parent <- integer()

  while (!.to %in% parent) {
    idx <- risk_vec == min(risk_vec)
    parent <- queue[idx]
    cur_risk <- risk_vec[idx][1]
    parent <- unique(parent)
    visited <- c(parent, visited)

    nei <- unlist(unname(lu[parent]))
    nei <- nei[!nei %in% visited]

    risk <- maze[nei]
    idx2 <- queue %in% parent

    risk_vec <-  c(risk_vec[!idx2], cur_risk + risk)
    queue <- c(queue[!idx2], nei)
  }

  return(cur_risk)
}

#part1-------
find_shortest_path(1e4)

#part2-----
x <- (data15 - 1L + rep(0:4, each = length(data15))) %% 9 + 1L
x2 <- as.integer(Reduce(rbind, rep(lapply(0:4, \(k) (matrix(x, 100) - 1 + k) %% 9 + 1))))

lookup2 <- lapply(seq_along(x2), map_k, n = 500)

system.time(find_shortest_path(500^2, lookup2, x2))
