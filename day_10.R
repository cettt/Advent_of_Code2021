data10 <- readLines("Input/day10.txt")

points1 <- c(")" = 3L, "]" = 57L, "}" = 1197L, ">" = 25137L)
points2 <- setNames(1:4, names(points1))
completer <- c("(" = ")", "[" = "]", "{" = "}", "<" = ">")

score_string <- function(x, part1 = TRUE) {

  pat <- "<>|\\{\\}|\\[\\]|\\(\\)"
  while (grepl(pat, x)) x <- gsub(pat, "", x)
  y <- strsplit(x, "")[[1]]

  if (part1) return(points1[y[y %in% c(")", "]", "}", ">")][1]])

  tail(filter(points2[completer[rev(y)]], 5L, method = "rec"), 1)
}

#part1----
sum(sapply(data10, score_string), na.rm = TRUE)

#part2------
median(sapply(data10, score_string, part1 = FALSE), na.rm = TRUE)


