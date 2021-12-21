data21 <- read.table("input/day21.txt", sep = ":")[,2]

#part1------
die <- rep.int(seq_len(10L) %% 10L, 300L) #each player gets 500 turns
mo <- aggregate(die, list(rep(seq_len(1e3), each = 3L)), \(x) sum(x) %% 10L)[,2]
mo[1:2] <- (mo[1:2] + data21 - 1L) %% 10L + 1L
score <- aggregate(mo, list(rep.int(1:2, 500L)), \(x) cumsum((cumsum(x) - 1L) %% 10L + 1L))[,2]

n <- which(score >= 1000L)[1]
score[n - 1L] * n * 3L

#part2------
oc <- table(rowSums(expand.grid(1:3, 1:3, 1:3)))
p0 <- data.frame(mo = as.integer(names(oc)), n0 = as.integer(oc), .j = 1L)
p <- data.frame(x1 = data21[1], x2 = data21[2], s1 = 0L, s2 = 0L, n = 1, .j = 1L)
n <- c(0, 0)
k <- 1L

while (nrow(p) > 0) {
  p <- dplyr::left_join(p, p0, by = ".j")
  p[, k] <- (p[, k] + p$mo - 1L) %% 10L + 1L # update position x
  p[, k + 2] <- p[, k + 2] + p[, k] # update score s
  p <- dplyr::summarise(dplyr::group_by(p, x1, x2, s1, s2, .j), n = sum(n*n0), .groups = "drop")
  n[k] <- n[k] + sum(p[p[, k + 2] >= 21L, ]$n)
  p <- p[p[, k + 2] < 21L, ]
  k <- 3L - k # from 1 to 2 and from 2 to 1
}

print(sprintf("%.f", max(n)))

