data19 <- read.table("Input/day19.txt", sep = ".")[,1]

#parse data----
li <- aggregate(data19, list(cumsum(grepl("s", data19))), \(x) paste(x[-1], collapse = " "))
li <- lapply(strsplit(li[,2], " "), \(x) t(do.call(rbind, strsplit(x, ","))))
li <- lapply(li, \(x) list(beacon = matrix(as.integer(x), nrow = 3), pos = c(0, 0, 0)))

#create rotation matrices--------
one_vec <- as.matrix(expand.grid(c(-1,1), c(-1,1), c(-1,1))) #helper matrix
perm_list <- list(1:3, c(1,3,2), c(2,1,3), c(2,3,1), c(3:1), c(3,1,2)) #helper list

make_mat <- function(i, j) {
  m <- matrix(0, 3, 3)
  m[perm_list[[i]] + 0:2*3] <- one_vec[j, ]
  return(m)
}

R <- unlist(lapply(1:6, \(i) lapply(1:8, \(j) make_mat(i, j))), recursive = FALSE)
R <- R[sapply(R, \(m) det(m) == 1)] #correct matrices have determinant == 1

#for each scanner compute the inner group Manhattan distances
find_rel_dist <- function(x) {
  res <- lapply(seq_along(x[[1]][1,]), \(k) colSums(abs(x[[1]][,k] - x[[1]][,-k])))
  c(x, rd = list(unlist(res)), rd2 = list(res))
}

li <- lapply(li, find_rel_dist)

#create a lookup table lu, which contains pair of scanners with 12 intersections----
lu <- expand.grid(k1 = seq_along(li), k2 = seq_along(li))
lu$intsct <- apply(lu, 1, \(i) length(intersect(li[[i[1]]]$rd, li[[i[2]]]$rd)))
lu <- subset(lu, intsct >= 66L & k1 != k2)

#function to intersect to scanners and get scanner position
intersect_scan <- function(a, b) {

  idx_ab <- integer(length(a$rd2))
  for (k in seq_along(a$rd2)) {
    for (l in setdiff(seq_along(b$rd2), idx_ab)) {
      if (length(intersect(a$rd2[[k]], b$rd2[[l]])) >= 11) idx_ab[k] <- l
    }
  }

  resa <- a$beacon[, which(idx_ab != 0)]
  resb <- b$beacon[, idx_ab[idx_ab != 0]]

  R2 <- R[sapply(R, \(m) ncol(unique(m  %*% resb - resa, MARGIN = 2)) == 1)][[1]]
  pos <- as.integer(resa[,1] - R2 %*% resb[,1])
  beac <- R2 %*% b$beacon + pos

  return(list(beacon = beac, pos = pos, rd = b$rd, rd2 = b$rd2))
}

#part1-----
scaned <- 1L

while (length(scaned) < length(li)) {
  k <- as.integer(subset(lu, k2 %in% scaned & !k1 %in% scaned)[1,1:2])
  scaned <- c(scaned, k[1])
  li[[k[1]]] <- intersect_scan(li[[k[2]]], li[[k[1]]])
}

ncol(unique(Reduce(cbind, lapply(li, \(z) z$beacon)), MARGIN = 2))

#part2------
max(find_rel_dist(list(sapply(li, \(x) x$pos)))$rd)


#explanation:
# first we parse the data: we create a list li of the same length as scanners
#   initially, each component of li has two components
#    - beacon are the beacon coordinates
#    - pos is the initial (unknown) position of the scanner which will be updated later
# Then, we create a list of all valid rotation matrices
# Next, for each scanner we compute all relative distances between the beacons (find_rel_dist)
#   this information is added to every component of li.
#  two scanners which overlap (12 beacons) must have at least 66 = (12*11 / 2) distances in common
#    note that in general this is only necessary but not sufficient
# the list lu contains all pairs of overlapping scanners.
# finally  for each matching pair we use the function intersect_scan:
#   this function first finds the 12 common beacons (idx_ab)
#    then the appropriate rotation matrix is found by trying all 24 possibilities (R2)
#    given R2 we can compute the new position of the scanner and update all beacon positions.
#
