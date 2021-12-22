parse_data <- function(x) {
  c(grepl("on", x), as.numeric(strsplit(gsub(".*x=", "", gsub(",[yz]=", "..", x)), "\\.\\.")[[1]]))
}

x <- unname(sapply(readLines("Input/day22.txt"), parse_data))
n <- which(abs(x[2,]) > 50)[1] - 1L

a <- array(0L, rep(101, 3))

for (i in seq_len(n)) {
  z <- x[-1,i] + 51
  a[z[1]:z[2], z[3]:z[4], z[5]:z[6]] <- x[1,i]
}

part1 <- sum(a)
part1

#part2--------
si_list <- lapply(21:ncol(x), \(k) x[,k])
names(si_list) <- paste0("_", 21:ncol(x), "_")

check_intersect <- function(i, j) {
  x1 <- si_list[[i]]
  x2 <- si_list[[paste0("_", j, "_")]]

  lo <- pmax(x1[c(2,4,6)], x2[c(2,4,6)])
  up <- pmin(x1[c(3,5,7)], x2[c(3,5,7)])

  if (any(lo > up)) NULL else c(x1[1], c(lo, up)[c(1,4, 2, 5, 3, 6)])
}

for (j in (2+n):ncol(x)) {
  int_vec <- character()
  for (i in paste0("_", seq(j - n - 1) + n, "_")) {
    res <- check_intersect(i, j)
    if (!is.null(res)) int_vec <- c(int_vec, i)
  }
  if (length(int_vec) > 0) {
    int_vec2 <- unique(as.character(unlist(sapply(int_vec, \(p) grep(p, names(si_list), value = TRUE)))))
    int_vec3 <- int_vec2
    for (del in int_vec) {

      int_vec3 <- gsub(del, "_", int_vec3)
    }
    int_vec2 <- int_vec2[int_vec3 == "_"]

    for (k in int_vec2) {
      res <- check_intersect(k, j)
      if (!is.null(res)) si_list <- c(si_list, setNames(list(res), paste0(k, j, "_")))
    }
  }
}

size_cube <- function(z) {
  prod(diff(z)[c(2,4,6)] + 1) * z[1]
}

size <- 0
for (j in (n+1):ncol(x)) {

  i_li <- si_list[grepl(paste0("_", j, "_$"), names(si_list))]
  size_vec <- sapply(i_li, size_cube)
  sign_vec <- sapply(names(i_li), \(z) length(gregexpr("_", z)[[1]]))

  if (x[1, j] == 1L) {
    size <- size + sum(size_vec * (-1)^sign_vec)
  } else {

    size <- size + sum(size_vec * (-1)^sign_vec)# - size_vec[sign_vec == 2]
  }
}

print(size + part1, 16)
