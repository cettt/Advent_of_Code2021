library(tidyverse)
data19 <- readLines("Input/day19.txt")

scan_list <- tibble(x = data19) %>%
  mutate(gr = cumsum(grepl("scan", x))) %>%
  filter(x != "") %>%
  filter(!grepl("scan", x)) %>%
  separate(x, into = c("x", "y", "z"), sep = ",", convert = TRUE) %>%
  group_by(gr) %>%
  nest(scan = c(x,y,z)) %>%
  mutate(scan = map(scan, ~ t(as.matrix(.)))) %>%
  pull(scan)

make_mat <- function(idx){
  m <- rep(0L, 9)
  m[idx[idx > 0]] <- 1L
  m[-idx[idx < 0]] <- -1L
  matrix(m, 3, 3)
}

R <- list(
  make_mat(c(1,5,9)), # facing pos x
  make_mat(c(1,6,-8)),
  make_mat(c(1,-5,-9)),
  make_mat(c(1,-6, 8)),
  make_mat(c(-1,-5,9)), # facing neg x
  make_mat(c(-1, -6, -8)),
  make_mat(c(-1,5,-9)),
  make_mat(c(-1, 6,8)),
  make_mat(c(2, -4, 9)),#facing pos y
  make_mat(c(2, 6, 7)),
  make_mat(c(2, 4, -9)),
  make_mat(c(2, -6, -7)),
  make_mat(c(-2, -4, -9)),#facing neg y
  make_mat(c(-2, -6, 7)),
  make_mat(c(-2, 4, 9)),
  make_mat(c(-2, 6, -7)),
  make_mat(c(3, 5, -7)),#facing pos z
  make_mat(c(3, 4, 8)),
  make_mat(c(3, -5, 7)),
  make_mat(c(3, -4, -8)),
  make_mat(c(-3, -5, -7)),#facing neg z
  make_mat(c(-3, -4, 8)),
  make_mat(c(-3, 5, 7)),
  make_mat(c(-3, 4, -8))
)

check_intersect <- function(a, b) {
  a_con <- integer(ncol(a))
  for (k in 1:ncol(a)) {
    xa <- apply(a[,-k], 2, \(x) sort(abs(x - a[,k]), decreasing = TRUE))

    for (l in seq_len(ncol(b))) {
      xb <- apply(b[,-l], 2, \(x) sort(abs(x - b[,l]), decreasing = TRUE))
      if (ncol(xa) + ncol(xb) - ncol(unique(cbind(xa, xb), MARGIN = 2)) >= 11) {
        a_con[k] <- l
      }
    }
  }
  return(sum(a_con != 0) >= 12)
}

intersect_scan <- function(i, j) {
   a <- new_scan[[i]]$beacon
   b <- new_scan[[j]]
   Ri <- new_scan[[i]]$rot
   posi <- new_scan[[i]]$scan_pos

  a_con <- integer(ncol(a))
  for (k in 1:ncol(a)) {
  xa <- apply(a[,-k], 2, \(x) sort(abs(x - a[,k]), decreasing = TRUE))

    for (l in seq_len(ncol(b))) {
      xb <- apply(b[,-l], 2, \(x) sort(abs(x - b[,l]), decreasing = TRUE))
      if (ncol(xa) + ncol(xb) - ncol(unique(cbind(xa, xb), MARGIN = 2)) >= 11) {
        a_con[k] <- l
      }
    }
  }
  if (sum(a_con != 0) < 12) return(list())
  resa <- a[,which(a_con != 0)]
  resb <- b[, a_con[a_con != 0]]

  .rot <- sapply(R, \(m) ncol(unique(m  %*% resb - resa, MARGIN = 2)))
  rot_idx <- which(.rot == 1)
  sca <- -as.integer(R[[rot_idx]]  %*% resb[,1] - resa[,1])
  beac <- (R[[rot_idx]] %*% b + sca)

  return(list(scan_pos = sca, rot = R[[rot_idx]], beacon = beac))
}

#part1-----
intmat <- matrix(0, length(scan_list), length(scan_list))

for (i in 1:(length(scan_list) - 1)) {
 for (j in (i + 1):length(scan_list)) {
  print(c(i, j))
   intmat[i, j] <- check_intersect(scan_list[[i]], scan_list[[j]])
   intmat[j, i] <- intmat[i,j]
 }
}
new_scan <- scan_list
new_scan[[1]] <- list(scan_pos = c(0,0,0), rot = diag(3), beacon = scan_list[[1]])
scaned <- 1L

while (length(scaned) < length(scan_list)) {
  next_idx <- which(intmat == 1 & row(intmat) %in% scaned & ! col(intmat) %in% scaned, arr.ind = TRUE)[1,]
  print(next_idx)
  res <- intersect_scan(next_idx[1], next_idx[2])
  scaned <- c(scaned, next_idx[2])
  new_scan[[next_idx[2]]] <- res
}


new_scan[[4]]
ncol(unique(Reduce(cbind, lapply(new_scan, \(z) z[[3]])), MARGIN = 2))


#part2------
cur <- 0
for (i in 1:(length(new_scan) - 1)) {
  for (j in i:length(new_scan)) {
    cur <- max(cur, sum(abs(new_scan[[i]][[1]] - new_scan[[j]][[1]])))
  }
}
cur
