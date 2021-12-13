data13 <- readLines("Input/day13.txt")

z <- sapply(strsplit(data13[grepl("^\\d", data13)], ","), as.integer)
folds <- data13[grepl("fold", data13)]

fold_paper <- function(z, f) {
  xy <- grepl("y", f) + 1
  z[xy, ] <- pmin(2 * as.integer(sub("\\D+", "", f)) - z[xy, ], z[xy, ])
  return(unique(z, MARGIN = 2))
}

#part1---
z <- fold_paper(z, folds[1])
ncol(z)

#part2 -----
for (f in folds[-1]) z <- fold_paper(z, f)
plot(t(z * c(1, -1)), xlim = c(-5, 45), ylim = c(-30, 30), pch = 15)
