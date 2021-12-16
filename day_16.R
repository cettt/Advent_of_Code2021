data16 <- readLines("Input/day16.txt")

bin2int <- function(y) sum(y * 2^(length(y):1 - 1L))

map <- sort(apply(expand.grid(0:1, 0:1, 0:1, 0:1), 1,  paste, collapse = ""))
map <- setNames(map, c(0:9, LETTERS[1:6]))

bits <- as.integer(unlist(strsplit(map[strsplit(data16, "")[[1]]], "")))
idx <- 1L
sv <- 0L #sum of versions

find_value <- function() {

  sv <<- sv + bin2int(bits[idx + 0:2])# update sum of versions
  id <- bin2int(bits[idx + 3:5]) # ID

  if (id == 4L) { #literal value
    z <- bits[(idx + 6):length(bits)]
    nval <- which(z[seq_along(z) %% 5L == 1L] == 0)[1]
    idx <<- idx + 6L + nval*5L
    return(bin2int(z[seq_along(z) %% 5L != 1L & seq_along(z) <= nval*5]))

  } else { #operation
    out_fun <- list(sum, prod, min, max, `>`, `<`, `==`)[[id + (id < 4)]]

    if (bin2int(bits[idx + 6L]) == 0) {#length
      idx_tar <- idx + 22L + bin2int(bits[idx + 6L + 1:15])  #target index
      idx <<-  idx + 22L
      val <- integer()
      while (idx < idx_tar) val <- c(val, find_value()) #stop when target is reached
    } else {#number of packages
      n_pack <- bin2int(bits[idx + 6L + 1:11])
      idx <<- idx + 18L
      val <- sapply(seq_len(n_pack), \(k) find_value())
    }
    return(if (id < 4L) out_fun(val) else out_fun(val[1], val[2]))
  }
}

#part2 -------
sprintf("%.f", find_value())

#part1---------
sv
