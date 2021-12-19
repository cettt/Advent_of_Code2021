data19 <- read.table("Input/day19.txt", sep = ".")[,1]

#parse data----
li <- aggregate(data19, list(cumsum(grepl("s", data19))), \(x) paste(x[-1], collapse = " "))
li <- lapply(strsplit(li[,2], " "), \(x) t(do.call(rbind, strsplit(x, ","))))
li <- lapply(li, \(x) matrix(as.integer(x), nrow = 3))

#create rotation matrices--------
one_vec <- as.matrix(expand.grid(c(-1,1), c(-1,1), c(-1,1))) #helper matrix
perm_list <- list(1:3, c(1,3,2), c(2,1,3), c(2,3,1), c(3:1), c(3,1,2)) #helper list

make_mat <- function(perm_i, one_vec_j) {
  m <- matrix(0, 3, 3)
  m[perm_list[[perm_i]] + 0:2*3] <- 1 * one_vec[one_vec_j, ]
  return(m)
}

R <- unlist(lapply(1:6, \(i) lapply(1:8, \(j) make_mat(i, j))), recursive = FALSE)
R <- R[sapply(R, \(m) det(m) == 1)] #correct matrices have determinant == 1

#for each scanner compute the inner group Manhattan distances
find_rel_dist <- function(x) {
  A <- expand.grid(seq_len(ncol(x)), seq_len(ncol(x)))
  apply(A[A[,1] > A[,2], ], 1, \(i) sum(abs(x[,i[1]] - x[,i[2]])))
}

li2 <- lapply(li, find_rel_dist)

#create a lookup table lu, which contains pair of scanners with 12 intersections----
lu <- expand.grid(seq_along(li), seq_along(li))
lu$intsct <- apply(lu, 1, \(i) length(intersect(li2[[i[1]]], li2[[i[2]]])))
lu <- lu[lu[, 3] >= 66, ]

#function to intersect to scanners and get scanner position
intersect_scan <- function(i, j) {
  a <- li3[[i]]$beacon
  b <- li3[[j]]

  idx_ab <- integer(ncol(a))
  for (k in 1:ncol(a)) {
    xa <- colSums(abs(a - a[,k]))
    for (l in seq_len(ncol(b))) {
      xb <- colSums(abs(b - b[,l]))
      if (length(intersect(xa, xb)) >= 12) idx_ab[k] <- l
    }
  }

  resa <- a[, which(idx_ab != 0)]
  resb <- b[, idx_ab[idx_ab != 0]]

  .rot <- sapply(R, \(m) ncol(unique(m  %*% resb - resa, MARGIN = 2)))
  rot_idx <- which(.rot == 1)
  pos <- -as.integer(R[[rot_idx]]  %*% resb[,1] - resa[,1])
  beac <- (R[[rot_idx]] %*% b + pos)

  return(list(pos = pos, beacon = beac))
}

#part1-----
li3 <- li
li3[[1]] <- list(scan_pos = c(0,0,0), beacon = li[[1]])
scaned <- 1L

while (length(scaned) < length(li)) {
  k <- as.integer(subset(lu, Var2 %in% scaned & !Var1 %in% scaned)[1,1:2])
  scaned <- c(scaned, k[1])
  li3[[k[1]]] <- intersect_scan(k[2], k[1])
}

ncol(unique(Reduce(cbind, lapply(li3, \(z) z[[2]])), MARGIN = 2))


#part2------
a <- sapply(li3, \(x) x[[1]])
max(colSums(abs(a[, rep(seq_along(li), ncol(a))] - a[, rep(1:ncol(a), each = ncol(a))])))

