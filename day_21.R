data21 <- read.table("input/day21.txt", sep = ":")[,2]

#part1------
die <- rep.int(seq_len(10) %% 10, 300)
mo <- aggregate(die, list(rep(seq_len(1e3), each = 3)), \(x) sum(x) %% 10L)[,2]
mo[1:2] <- (mo[1:2] + data21 - 1) %% 10 + 1L
score <- aggregate(mo, list(rep(1:2, 500)), \(x) cumsum((cumsum(x) - 1L) %% 10L + 1L))[,2]

n <- which(score >= 1000)[1]

 score[n - n %% 2] * n * 3

#part2------
library(tidyverse)
p0 <- tibble(mo = rowSums(expand.grid(d1 = 1:3, d2 = 1:3, d3 = 1:3))) %>%
  group_by(mo) %>%
  summarise(n0 = n(), .groups = "drop") %>%
  mutate(.j = 1)

p <- tibble(x1 = data21[1], score1 = 0, x2 = data21[2], score2 = 0, n = 1, .j = 1)

res_tib <- tibble(p = integer(), n = numeric())

while (nrow(p) > 0) {
  p <- p %>%
  filter(score2 < 21) %>%
  left_join(p0, by = ".j") %>%
  mutate(
    x1 = (x1 + mo - 1L) %% 10 + 1L,
    score1 = score1 + x1,
    n = n * n0
  ) %>%
    group_by(x1, score1, x2, score2, .j) %>%
    summarise(n = sum(n), .groups = "drop")

  res_tib <- p %>%
    filter(score1 >= 21) %>%
    transmute(n = n, p = 1L) %>%
    bind_rows(res_tib)


  p <- p %>%
    filter(score1 < 21) %>%
    left_join(p0, by = ".j") %>%
    mutate(
      x2 = (x2 + mo - 1L) %% 10 + 1L,
      score2 = score2 + x2,
      n = n * n0
    ) %>%
    group_by(x1, score1, x2, score2, .j) %>%
    summarise(n = sum(n), .groups = "drop")

  res_tib <- p %>%
    filter(score2 >= 21) %>%
    transmute(n = n, p = 2L) %>%
    bind_rows(res_tib)

}

res_tib %>%
  group_by(p) %>%
  summarise(n = sum(n)) %>%
  pull(n) %>%
  max() %>%
  print(16)
