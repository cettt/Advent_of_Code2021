data17 <- strsplit(gsub("[a-z ;=\\.:,]+", " ", readLines("input/day17.txt")), " ")[[1]]
tar <- as.integer(data17)[-1]

vx <- which(sapply(seq_len(tar[2]), \(x) any(cumsum(x:1) >= tar[1] & cumsum(x:1) <= tar[2]))) #valid x velocities

throw <- function(vx0, vy0) {

  xvec <- cumsum(vx0:1)[cumsum(vx0:1) <= tar[2]]
  yvec <- cumsum(vy0 - seq_along(xvec) + 1L)

  if (any(xvec >= tar[1] & xvec <= tar[2] & yvec >= tar[3] & yvec <= tar[4])) {
    return(max(yvec))
  } else if (length(xvec) < vx0 | yvec[length(yvec)] < tar[3]) return(NA_integer_)

  max_y <- (vy0 *  (vy0 + 1L)) / 2L
  yvec2 <- cumsum(seq.int(-vy0 - 1L, by = -1L, length.out = abs(tar[3] / 2)))
  return(if (any(yvec2 >= tar[3] & yvec2 <= tar[4])) max_y else NA_integer_)
}

res <- as.integer(sapply(vx, \(x) sapply(tar[3]:(-tar[3] - 1), \(y) throw(x, y))))

#part1-----
max(res, na.rm = TRUE) #is the same as tar[3] * (tar[3] + 1) / 2)

#part2---------
sum(!is.na(res))

#explanation:
#first we check valid x velocities:
# - first we verify that the maximum x-speed is tar[2]
# - then, for all vx <= tar[2] we check if any component of x-positions (cumsum(vx:1) is inside the target area)
# after the x velocity reached zero the x-coordinate of the probe does not change.
#  if at this point the x-coordinate is not within the target x coordinates we will never reach the target
#   otherwise note that the probe at some point was or will be at y = 0 with velocity -vy0 - 1 where vy0 is the starting velo.
