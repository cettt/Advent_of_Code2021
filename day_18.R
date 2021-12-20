unlist_pairs <- function(x) {
  nam <- cumsum(ifelse(x == "[", 1, ifelse(x == "]", -1, 0)))[grepl("\\d", x)]
  setNames(as.integer(x[grepl("\\d", x)]), nam)
}

data18 <- lapply(strsplit(readLines("Input/day18.txt"), ""), unlist_pairs)

add_pair <- function(x, y) {
  xy <- setNames(c(x, y), as.integer(c(names(x), names(y))) + 1L)

  while (TRUE) {

    exp_idx <- .Internal(which(names(xy) == "5"))
    for (k in seq_len(length(exp_idx) / 2)) {
      n <- exp_idx[2*k - 1L] - k + 1L
      if (n > 1L)              xy[n - 1L] <- sum(xy[n - 0:1])
      if (n < length(xy) - 1L) xy[n + 2L] <- sum(xy[n + 1:2])
      xy[n] <- 0L
      names(xy)[n] <- "4"
      xy <- xy[-n - 1L]
    }

    idx <- xy > 9L
    if (any(idx)) {
      n <- .Internal(which(idx))[1L]
      z <- xy[n]
      nam <- as.character(as.integer(names(z)) + 1L)
      new_val <- as.integer((z + 0:1) / 2L)
      names(new_val) <- c(nam, nam)
      xy <- c(xy[seq_len(n - 1L)], new_val, xy[-seq_len(n)])
    } else break
  }
  return(xy)
}

magn <- function(xy) {
  for (i in seq_len(length(xy) - 1L)) {
    nam <- as.integer(names(xy))
    n <- which.max(nam)
    xy[n] <- sum(xy[n + 0:1] * 3:2)
    names(xy)[n] <- as.character(nam[n] - 1L)
    xy <- xy[-n - 1L]
  }
  return(xy)
}

magn(Reduce(add_pair, data18))

#part2-------
A <- cbind(rep(seq_along(data18), 100), rep(seq_along(data18), each = 100))
max(apply(A[A[,1] != A[,2], ], 1, \(k) magn(Reduce(add_pair, data18[k]))))
