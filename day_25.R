day25 <- as.character(do.call(rbind, strsplit(readLines("Input/day25.txt"), "")))

z <- complex(real = rep(0:138, each = 137), imaginary = rep(0:136, 139))
east <- z[day25 == ">"]
sout <- z[day25 == "v"]

for (k in seq_len(1e3)) {

  east2 <- (Re(east + 1) %% 139 + Im(east) * 1i)
  idx <- east2 %in% c(east, sout)
  east[!idx] <- east2[!idx]

  sout2 <- Re(sout) + ((Im(sout) + 1) %% (137)) * 1i
  idx2 <- sout2 %in% c(east, sout)
  sout[!idx2] <- sout2[!idx2]

  if (all(c(idx, idx2))) break
}

k
