data13 <- readLines("Input/day13.txt")
z <- sapply(strsplit(data13[grepl("^\\d", data13)], ","), as.integer)

fold_paper <- function(z, f) {
  xy <- grepl("y", f) + 1
  z[xy, ] <- pmin(2 * as.integer(sub("\\D+", "", f)) - z[xy, ], z[xy, ])
  return(unique(z, MARGIN = 2))
}

#part1---
ncol(fold_paper(z, data13[grepl("fold", data13)][1]))

#part2 -----
for (f in data13[grepl("fold", data13)]) z <- fold_paper(z, f)
plot(z[1, ], -z[2, ], xlim = c(-5, 45), ylim = c(-30, 30), pch = 15)
