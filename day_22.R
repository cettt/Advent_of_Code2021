data22 <- strsplit(gsub("[^0-9onf-]+", " ", readLines("Input/day22.txt")), " ")
cubo <- lapply(data22, \(x) c(grepl("n", x[1]), as.numeric(x[-1])))

n <- which(sapply(cubo, \(z) abs(z[2])) > 50)[1] - 1L

arr_3d <- array(0L, rep(101L, 3L))

for (i in seq_len(n)) {
  z <- cubo[[i]][-1] + 51
  arr_3d[z[1]:z[2], z[3]:z[4], z[5]:z[6]] <- cubo[[i]][1]
}

sum(arr_3d)

#part2--------
#crate a list containing the endpoints of all intersections
make_isct <- function(co1, co2) {
  lo <- pmax(co1[c(2,4,6)], co2[c(2,4,6)]) #lower bound of cuboid
  up <- pmin(co1[c(3,5,7)], co2[c(3,5,7)]) #upper bound of cuboid

  if (any(lo > up)) return(NULL)
  c(co1[1], c(lo, up)[c(1,4, 2, 5, 3, 6)], prod(up - lo + 1)*co1[1], co1[-(1:8)], co2[-(1:8)])
}

cubo2 <- lapply(cubo[-seq_len(n)], \(x) c(x, prod(diff(x)[c(2,4,6)] + 1) * x[1]))
cubo2 <- lapply(seq_along(cubo2), \(k) c(cubo2[[k]], k))
n2 <- length(cubo2)


size <- cubo2[[1]][8] # size of first cuboid
for (j in seq_along(cubo2)[-1]) {
  new_isct <- lapply(cubo2[seq_len(j - 1)], \(x) make_isct(x, cubo2[[j]]))
  new_isct <- new_isct[!sapply(new_isct, is.null)]

  if (length(new_isct) > 0) {
    idx <- sapply(new_isct, \(z) z[9])
    idx2 <- sapply(cubo2[-seq_len(n2)], \(x) all(x[-(1:8)] %in% idx))
    if (any(idx2)) {
      new_isct2 <- lapply(cubo2[-seq_len(n2)][idx2], make_isct, co2 = cubo2[[j]])
      new_isct <- c(new_isct, new_isct2[!sapply(new_isct2, is.null)])
    }
  }
  size <- size + sum(sapply(c(cubo2[j], new_isct), \(z) c(z[8] * (-1)^(length(z) + 1))))
  cubo2 <- c(cubo2, new_isct)
}

sprintf("%.f", size + sum(arr_3d))
