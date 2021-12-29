data21 <- read.table("input/day21.txt", sep = ":")[,2]

#part1------
mo <- rep_len(9:0, 1003)[-(1:3)] #first player rolls 6, second rolls 15 = 5 mod 10, then 24 = 4 mod 10 etc
mo[1:2] <- (mo[1:2] + data21 - 1L) %% 10L + 1L
score <- aggregate(mo, list(rep.int(1:2, 500L)), \(x) cumsum((cumsum(x) - 1L) %% 10L + 1L))[,2]

n <- which(score >= 1000L)[1]
score[n - 1L] * n * 3L

#part2------
A <- array(0L, dim = c(10,21,10,21))
A[data21[2],1,data21[1], 1] <- 1L
oc <- rep(c(0L, 0L, 0L, table(rowSums(expand.grid(1:3, 1:3, 1:3)))), 10)
n <- c(0, 0)
mat_pos <- sapply(0:9, \(k) oc[k * 9L + 1:10])

update_2d <- function(m, pl) {
  new_pos <- mat_pos %*% m #new positions after players pl turn
  n[pl] <<- n[pl] + sum(new_pos[row(new_pos) + col(new_pos) >= 22L]) #positions which will reach score 21
  t(sapply(1:10, \(k) c(rep.int(0L, k), new_pos[k,])[seq_len(21)])) # update score according to positions
}

for (r in rep_len(1:2, 19)) A <- array(apply(A, c(1,2), update_2d, pl = r), dim(A))
sprintf("%.f", max(n))

#the game cannot take longer than 19 turns.
#after player 1's 10th turn the game is over no matter what:
#worst case positions 2,1,4,3,2,1,4,2,1 with score 20
