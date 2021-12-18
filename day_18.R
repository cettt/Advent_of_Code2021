data18 <- readLines("Input/day18.txt")

mysplit <- function(z) {
  gsub("(\\d)(\\D)", "\\1 \\2", gsub("(\\D)", "\\1 ", z))
}
f <- function(z) paste0(z, collapse = "")

add_numbers <- function(x, y) {
  xy <- mysplit(paste0("[",x, ",", y, "]"))
  xy2 <- strsplit(xy, " ")[[1]]
  ex1 <- cumsum(ifelse(xy2 == "[", 1, ifelse(xy2 == "]", -1, 0)))
  check_expl <- any(ex1 >= 5)
  check_spli <- any(nchar(xy2) > 1)

  if (pr) print(f(xy2))
  while (check_expl | check_spli) {
  # for (i in 1:5) {

    if (any(check_expl)) {
      if (pr) print("explosion")
      ex_pos1 <- which(ex1 >= 5)[1]
      ex_pos2 <- ex_pos1 + 4
      if (pr) print(f(xy2[ex_pos1:ex_pos2]))

      n1 <- as.integer(xy2[ex_pos1 + 1L])
      n2 <- as.integer(xy2[ex_pos1 + 3L])
      if (any(grepl("\\d", xy2[1:ex_pos1])))  {
        pos1 <- max(which(grepl("\\d", xy2[1:ex_pos1])))
        xy2[pos1] <- as.character(as.integer(xy2[pos1]) + n1)
      }
      if (any(grepl("\\d", xy2[-(1:ex_pos2)])))  {
        pos2 <- which(grepl("\\d", xy2[-(1:ex_pos2)]))[1] + ex_pos2
        xy2[pos2] <- as.character(as.integer(xy2[pos2]) + n2)

      }
      xy3 <- c(xy2[1:(ex_pos1 - 1)], "0", xy2[-(1:(ex_pos2))])

    } else { #split
      if (pr) print("split")
     split_pos <- which(nchar(xy2) > 1)[1]
     n3 <- as.integer(xy2[split_pos])
     if (pr) print(n3)
     xy3 <- c(xy2[1:(split_pos - 1)], "[", floor(n3 / 2), ",", ceiling(n3 / 2), "]", xy2[-(1:(split_pos))])
    }

    xy2 <- xy3
    ex1 <- cumsum(ifelse(xy2 == "[", 1, ifelse(xy2 == "]", -1, 0)))
    check_expl <- any(ex1 >= 5)
    check_spli <- any(nchar(xy2) > 1)
    if (pr) print(f(xy2))
  }

  return(f(xy2))

}


compute_magn <- function(xy) {
  xy2 <- strsplit(xy, "")[[1]]
  while (grepl("\\d+,\\d+", xy)) {
    pos3 <- regexpr("\\d+,\\d+", xy)
    n <- attributes(pos3)$match.length
    val <- as.integer(strsplit(paste0(xy2[pos3[1] + 1:n - 1], collapse = ""), ",")[[1]])
    xy <- sub("\\[\\d+,\\d+\\]", val[1]*3 + val[2]*2, xy)
    xy2 <- strsplit(xy, "")[[1]]
  }
  return(as.integer(xy))
}

pr <- FALSE
res <- add_numbers(data18[1], data18[2])
for (z in data18[3:length(data18)]) res <- add_numbers(res, z)

compute_magn(res)


#part2-------
comp_part2 <- function(idx) compute_magn(add_numbers(data18[idx[1]], data18[idx[2]]))


A <- cbind(rep(1:100, 100), rep(1:100, each = 100))
A <- A[A[,1] != A[,2], ]

max(apply(A, 1, comp_part2))
