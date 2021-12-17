data17 <- strsplit(gsub("[a-z ;=\\.:,]+", " ", readLines("input/day17.txt")), " ")[[1]]
tar <- as.integer(data17)[-1]

vx <- which(sapply(seq_len(tar[2]), \(x) any(cumsum(x:1) >= tar[1] & cumsum(x:1) <= tar[2]))) #valid x velocities

throw <- function(vx0, vy0) {

  xvec <- cumsum(c(vx0:1))
  yvec <- cumsum(as.numeric(vy0 - seq_along(xvec) + 1))

  if (any(xvec >= tar[1] & xvec <= tar[2] & yvec >= tar[3] & yvec <= tar[4])) {
    return(max(yvec))
  } else if (xvec[vx0] > tar[2] | yvec[vx0] < tar[3] | -vy0 - 1 < tar[3]) {
    return(NA_integer_)
  } #function does continue only if x-velo == 0 and ypos is above target


  max_y <- (vy0 *  (vy0 + 1)) / 2L #highest y position that will be (or was) reached
  #at some point the ball will be at max(xvec) and 0 with a velocity of -(vy0 +1)
  yvec2 <- cumsum(seq(-vy0 - 1, by = -1, length.out = abs(tar[3])))
  return(if (any(yvec2 >= tar[3] & yvec2 <= tar[4])) max_y else NA_integer_ )
}

res <- as.integer(sapply(vx, \(x) sapply(tar[3]:-tar[3], \(y) throw(x,y))))

#part1-----
max(res, na.rm = TRUE) #is the same as sum(1:abs(tar[3]) - 1)

#part2---------
sum(!is.na(res))
