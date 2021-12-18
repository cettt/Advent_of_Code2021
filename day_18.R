unlist_pairs <- function(x) {
  nam <- cumsum(ifelse(x == "[", 1, ifelse(x == "]", -1, 0)))[grepl("\\d", x)]
  setNames(as.integer(x[grepl("\\d", x)]), nam)
}

data18 <- lapply(strsplit(readLines("Input/day18.txt"), ""), unlist_pairs)

add_numbers <- function(x, y) {
  xy <- setNames(c(x, y), as.integer(names(c(x,y))) + 1L)

  while (any(c(names(xy) > 4, xy > 9))) {

    if (any(names(xy) > 4)) { #explode
      n <- which(names(xy) > 4)[1]
      if (n != 1L) xy[n - 1] <- xy[n - 1] + xy[n]
      if (n != length(xy) - 1) xy[n + 2] <- xy[n + 2] + xy[n + 1]
      xy <- c(xy[seq_len(n - 1)], setNames(0, "4"), xy[-seq_len(n + 1)])
    } else { #split
      n <- which(xy > 9)[1]
      new_val <- c(floor(xy[n] / 2), ceiling(xy[n] / 2))
      names(new_val) <- rep(as.integer(names(xy[n])) + 1, 2)
      xy <- c(xy[seq_len(n - 1)], new_val, xy[-seq_len(n)])
    }
  }
  return(xy)

}

magn <- function(xy) {
  while (length(xy) > 1) {
    n <- which.max(names(xy))
    new_val <- setNames(sum(xy[n + 0:1] * 3:2), as.integer(names(xy[n])) - 1L)
    xy <- c(xy[seq_len(n - 1)], new_val, xy[-seq_len(n + 1)])
  }
  return(xy)
}

magn(Reduce(add_numbers, data18))

#part2-------
comp_part2 <- function(idx) magn(Reduce(add_numbers, data18[idx]))

A <- cbind(rep(seq_along(data18), 100), rep(seq_along(data18), each = 100))
max(apply(A[A[,1] != A[,2], ], 1, comp_part2))
