numbers <- read.table("Input/day04.txt", sep = ",", nrows = 1)[1, ]
mat     <- read.table("Input/day04.txt", sep = "", skip = 2)

mat_list <- lapply(seq(5, nrow(mat), 5), \(i) as.matrix(mat[i - 4:0, ]))
res <- integer()

for (x in numbers) {

  mat_list <- lapply(mat_list, \(m) {m[m == x] <- -1; return(m)})
  idx <- sapply(mat_list, \(m) any(c(rowSums(m), colSums(m)) == -5))

  if (any(idx)) {
    res <- c(res, sapply(mat_list[idx], \(m) sum(pmax(m, 0)) * x))
    mat_list <- mat_list[!idx]
  }
}

#part 1------------
res[1]

#part 2--------------
tail(res, 1)
