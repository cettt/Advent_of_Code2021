data08 <- strsplit(readLines("Input/day08.txt"), "\\W+")

#part1---------
sum(sapply(data08, \(z) sum(nchar(z[-(1:10)]) %in% c(2:4, 7))))


#part2-----------
find_digits <- function(x) {

  y <- strsplit(x[1:10], "")

  y_174 <- sapply(2:4, \(k) y[sapply(y, length) == k][[1]]) #1, 7 and 4
  y_069 <- y[sapply(y, length) == 6]

  y_6 <- y_069[sapply(y_069, \(z) !all(y_174[[1]] %in% z))][[1]] #6 does not contain all segments of 1
  y_9 <- y_069[sapply(y_069, \(z)  all(y_174[[3]] %in% z))][[1]] #9 contains all segments of 4

  seg_c <- setdiff(y_174[[1]],   y_6) #segment in 2 but not 6
  seg_e <- setdiff(letters[1:7], y_9) # segment in 8 but not 9

  z <- strsplit(x[11:14], "")
  res <- integer(4)

  for (i in 1:4) {
    if (length(z[[i]]) %in% c(2:4, 7)) {
      res[i] <- c("2" = 1, "3" = 7, "4" = 4, "7" = 8)[as.character(length(z[[i]]))]
    } else if (length(z[[i]]) == 6L) {
      res[i] <- if (!seg_c %in% z[[i]]) 6L else if (!seg_e %in% z[[i]]) 9L else 0L
    } else {
      res[i] <- if (!seg_c %in% z[[i]]) 5L else if (!seg_e %in% z[[i]]) 3L else 2L
    }
  }
  return(sum(res * 10^(3:0)))

}

sum(sapply(data08, find_digits))
