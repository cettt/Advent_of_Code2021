data21 <- read.table("input/day21.txt", sep = ":")[,2]

#part1------
die <- rep.int(seq_len(10L) %% 10L, 300L) #each player gets 500 turns
mo <- aggregate(die, list(rep(seq_len(1e3), each = 3L)), \(x) sum(x) %% 10L)[,2]
mo[1:2] <- (mo[1:2] + data21 - 1L) %% 10L + 1L
score <- aggregate(mo, list(rep.int(1:2, 500L)), \(x) cumsum((cumsum(x) - 1L) %% 10L + 1L))[,2]

n <- which(score >= 1000L)[1]
score[n - 1L] * n * 3L

#part2------
A <- array(0L, dim = c(10,21,10,21))
A[data21[2],1,data21[1], 1] <- 1L
oc <- rep(c(0L, 0L, 0L, table(rowSums(expand.grid(1:3, 1:3, 1:3)))), 10)
n <- c(0, 0)
B <- sapply(0:9, \(k) oc[k * 9L + 1:10])

update2x2 <- function(a, pl) {
  C <- B %*% a
  n[pl] <<- n[pl] + sum(C[row(C) + col(C) >= 22])
  t(sapply(1:10, \(k) c(rep.int(0L, k), C[k,])[seq_len(21)]))
}

for (r in 1:30) {
  A <- array(apply(A, c(1,2), update2x2, pl = 2 - r %% 2), dim = dim(A))
  if (all(A == 0)) break
}

sprintf("%.f", max(n))
