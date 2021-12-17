data16 <- readLines("Input/day16.txt")

bin2int <- function(y) sum(y * 2^(length(y):1 - 1L))

map <- sort(apply(expand.grid(0:1, 0:1, 0:1, 0:1), 1,  paste, collapse = ""))
map <- setNames(map, c(0:9, LETTERS[1:6]))

bits <- as.integer(unlist(strsplit(map[strsplit(data16, "")[[1]]], "")))
sv <- 0L #sum of versions

find_value <- function() {

  sv <<- sv + bin2int(bits[1:3])# update sum of versions
  id <- bin2int(bits[4:6]) # ID

  if (id == 4L) { #literal value
    z <- bits[7:length(bits)]
    nval <- which(z[seq_along(z) %% 5L == 1L] == 0L)[1L]
    bits <<- bits[-seq_len(6L + nval*5L)]
    return(bin2int(z[seq_along(z) %% 5L != 1L & seq_along(z) <= nval*5L]))
  }
  else if (bits[7L] == 0L) { #operation__length
    tar_len <- length(bits) - 22L - bin2int(bits[7L + seq_len(15L)])  #target length
    bits <<- bits[-seq_len(22L)]
    val <- integer()
    while (length(bits) > tar_len) val <- c(val, find_value()) #stop when target is reached
  }
  else {#number of packages
    n_pack <- bin2int(bits[7L + seq_len(11L)])
    bits <<- bits[-seq_len(18L)]
    val <- sapply(seq_len(n_pack), \(k) find_value())
  }

  out_fun <- list(sum, prod, min, max, `>`, `<`, `==`)[[id + (id < 4)]]
  return(if (id < 4L) out_fun(val) else out_fun(val[1], val[2]))
}

#part2 -------
sprintf("%.f", find_value())

#part1---------
sv
