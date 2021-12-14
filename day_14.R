data14 <- readLines("Input/day14.txt")

x <- sapply(seq_len(nchar(data14[1]) - 1), \(k) substring(data14[1], k, k + 1))
pats <- lapply(strsplit(data14[-(1:2)], ""), \(y) c(paste0(y[1], y[2]), paste0(y[1], y[7]), paste0(y[7], y[2])))
pats <- do.call(rbind, pats)

simulate_poly <- function(n) {
  tab <- table(x)
  for (i in seq_len(n)) {
    new_tab <- integer()
    for (k in seq_along(tab)) {
      if (names(tab[k]) %in% pats[,1]) {
        new_tab <- c(new_tab, setNames(rep(tab[k], 2), pats[pats[,1] == names(tab[k]), -1]))
      } else new_tab <- c(new_tab, setNames(tab[k], names(tab[k])))
    }

    pre <- aggregate(new_tab, list(names(new_tab)), sum)
    tab <- setNames(pre[,2], pre[,1])
  }

  az <- setNames(c(1,1), substring(data14[1], c(1, nchar(data14[1])), c(1, nchar(data14[1]))))

  tab <- c(tab, az)

  my_count <- function(a) {
    aa <- paste0(a, a)
    sum(tab[grepl(a, names(tab))]) + sum(tab[grepl(aa, names(tab))])
  }

  fin <- sapply(unique(substr(pats[,1], 1, 1)), my_count) / 2
  max(fin) - min(fin)

}

#part1--------
simulate_poly(10)

#part2----
print(simulate_poly(40), 12)
