data12 <- read.table("Input/day12.txt", sep = "-")

map <- setNames(c(data12[,1], data12[,2]), c(data12[,2], data12[,1]))
map <- map[map != "start"]

sc <- setdiff(map[grepl("[a-z]", map)], c("start", "end")) #small caves
lookup <- integer()

count_paths <- function(p, part1, sc_vec = setNames(integer(length(sc)), sc)) {

  if (p == "end") return(1L)
  if (p %in% sc) sc_vec[p] <- sc_vec[p] + 1L

  id <- paste(p, paste0(sc_vec, collapse = ""), sep = "_")
  if (id %in% names(lookup)) return(lookup[id])

  invalid <- any(sc_vec > 2) | sum(sc_vec > 1) > 1 | (part1 & any(sc_vec > 1))
  res <- if (invalid) 0L else sum(sapply(map[names(map) == p], count_paths, part1, sc_vec))

  lookup <<- c(lookup, setNames(res, id))
  return(res)
}

#part1-----
count_paths("start", part1 = TRUE)

#part2-----------
lookup <- integer()
count_paths("start", part1 = FALSE)

#first we create a vector which is our map: we use the names attribute
#   we remove start from the map such that there cannot be any paths which go back to start
#   we make a list of the small caves
#   the main function is recursive: in each step we update the list of small caves visited (sv_vec)
#    we use sv_vec to determine if a path is valid or not.
#    we create a global lookup which saves all subresults.
