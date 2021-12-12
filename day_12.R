data12 <- read.table("Input/day12.txt", sep = "-")

map <- cbind(c(data12[,1], data12[,2]), c(data12[,2], data12[,1]))
map <- map[map[,2] != "start",]
sc <- setdiff(map[grepl("[a-z]", map[,1]), 1], c("start", "end"))

count_paths <- function(p, sc_vec, part1 = TRUE) {

  if (p == "end") return(1)
  if (p %in% sc) {
    # print(p); print(sc_vec);
    sc_vec[p] <- sc_vec[p] + 1L
    }
  id <- paste(p, paste0(sc_vec, collapse = ""), sep = "_")
  if (id %in% names(envir$res)) return(envir$res[id])
  if (any(sc_vec > 1) & part1) {
    res <- 0
  } else if (!part1 & (any(sc_vec > 2) | sum(sc_vec > 1) > 1)) {
    res <- 0
  } else
  {
    ne <- setdiff(map[map[,1] == p, 2], names(sc_vec[sc_vec > 1]))
    res <- if (length(ne) == 0) 0L else sum(sapply(ne,  count_paths, sc_vec = sc_vec, part1 = part1))
  }

  envir$res <- c(envir$res, setNames(res, id))
  return(res)

}

envir <- environment()
envir$res <- NULL
sc_vec <- setNames(rep(0, length(sc)), sc)
count_paths("start", sc_vec, TRUE)


#part2-----------
envir$res <- NULL
count_paths("start", sc_vec, FALSE)
