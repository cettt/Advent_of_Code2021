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
      new_val <- as.integer((z + 0:1) / 2L)

      if (names(z) == "4") { #if there is an explosion right after the split
        if (n > 1L)         xy[n - 1L] <- xy[n - 1L] + new_val[1]
        if (n < length(xy)) xy[n + 1L] <- xy[n + 1L] + new_val[2]
        xy[n] <- 0L
      } else {
        nam <- as.character(as.integer(names(z)) + 1L)
        names(new_val) <- c(nam, nam)
        xy <- c(xy[seq_len(n - 1L)], new_val, xy[-seq_len(n)])
      }
    } else break
  }
  return(xy)
}

magn <- function(xy) {
  for (nam in as.character(4:1)) {
    idx <- .Internal(which(names(xy) == nam))
    idx1 <- idx[seq_along(idx) %% 2L == 1L]
    xy[idx1] <- xy[idx1]*3L + xy[idx1 + 1L]*2L
    names(xy)[idx1] <- as.character(as.integer(nam) - 1L)
    xy <- xy[-idx1 - 1L]
  }
  return(xy)
}

magn(Reduce(add_pair, data18))

#part2-------
idx <- order(sapply(data18, sum), decreasing = TRUE)[1:40]
A <- subset(expand.grid(x = idx, y = idx), x != y)
max(apply(A, 1, \(k) magn(Reduce(add_pair, data18[k]))))
