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

make_isct <- function(lco1, co2) {

  co1 <- matrix(sapply(lco1, \(x) x[1:7]), nrow = 7)

  lo <- matrix(pmax(co1[c(2,4,6),], co2[c(2,4,6)]), nrow = 3) #lower bound of cuboid
  up <- matrix(pmin(co1[c(3,5,7),], co2[c(3,5,7)]), nrow = 3) #upper bound of cuboid

  idx <- colSums((up - lo) < 0) == 0
  lo <- matrix(lo[,idx], nrow = 3)
  up <- matrix(up[,idx], nrow = 3)

  vol <- exp(colSums(log((up - lo + 1)))) * co1[1, idx]
  res <- rbind(co1[1,idx], matrix(rbind(lo, up)[c(1,4, 2, 5, 3, 6),], nrow = 6), unname(vol))

  lapply(seq_along(res[1,]), \(k) c(res[,k], lco1[idx][[k]][-(1:8)], co2[9]))
}


cubo2 <- lapply(cubo[-seq_len(n)], \(x) c(x, prod(diff(x)[c(2,4,6)] + 1) * x[1]))
cubo2 <- lapply(seq_along(cubo2), \(k) c(cubo2[[k]], k))
n2 <- length(cubo2)


volume <- cubo2[[1]][8] # size of first cuboid

for (j in seq_along(cubo2)[-1]) {
  new_isct <- make_isct(cubo2[seq_len(j - 1)], cubo2[[j]])

  if (length(new_isct) > 0) {
    idx <- sapply(new_isct, \(z) z[9])
    idx2 <- sapply(cubo2[-seq_len(n2)], \(x) all(x[-(1:8)] %in% idx))
    if (any(idx2)) {
      new_isct <- c(new_isct, make_isct(cubo2[-seq_len(n2)][idx2], cubo2[[j]]))
    }
  }
  volume <- volume + sum(sapply(c(cubo2[j], new_isct), \(z) c(z[8] * (-1)^(length(z) + 1))))
  cubo2 <- c(cubo2, new_isct)
}
sprintf("%.f", volume + sum(arr_3d))
