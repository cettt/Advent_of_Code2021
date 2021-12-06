data03 <- read.fwf("Input/day03.txt", widths = rep(1, 12))

#part1-----------
tmp <- apply(data03, 2, \(x) names(sort(table(x))))
prod(apply(tmp, 1, \(x) strtoi(paste(x, collapse = ""), base = 2)))

#part2-----------
find_c_o2 <- function(dat = data03, pos = 1L, o2 = 1L) {
  idx <- (if (sum(dat[, pos]) < nrow(dat) / 2) `==` else `!=`)(dat[, pos], o2)
  if (sum(idx) > 1) return(find_c_o2(dat[idx, ], pos + 1L, o2))
  return(strtoi(paste(dat[idx, ], collapse = ""), base = 2))
}

find_c_o2(o2 = 1L) * find_c_o2(o2 = 0L)
