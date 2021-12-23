# x <- as.matrix(read.fwf("day23.txt", width = rep(1, 13), comment.char = ""))

# x <- as.matrix(read.fwf("day23_p1.txt", width = rep(1, 13), comment.char = ""))

#functions--------

is.home <- function(df) {
  sapply(seq_along(df[,1]), \(i) Re(df$co[i]) == df$dest[i] & all(subset(df, Re(co) == Re(df$co[i]) & Im(co) > Im(df$co[i]))$dest == df$dest[i]))
}

bfs <- function(q, maze) {
  j <- 1L

  while (j <= length(q)) {
    new_edge <- setdiff(maze[abs(q[j] - maze) == 1], q)
    q <- c(q, new_edge)
    j <- j + 1L
  }
  return(q)
}
hallway_co <- c(1,2,4,6,8,10,11) + 1i

find_valid_dest <- function(df, id) {

  cor_room <- df[Re(df$co) == df$dest[id], ] #filled positions in correct room

    pos_target <- complex()

    if (nrow(cor_room) == 0 | all(cor_room$is_home)) { #check if you can enter correct room
    pos_target <- c(pos_target, df$dest[id] + (wid - nrow(cor_room)) * 1i)
  }
  if (Im(df$co[id]) != 1) pos_target <- c(pos_target, hallway_co) #if we are not in the hallway add hallway

  res <- intersect(bfs(df$co[id], setdiff(map, df$co[-id])), pos_target)

  if (length(res) == 0) {
    return(data.frame(old_co = complex(), id = integer(), co = complex(), home = logical(), energy = numeric()))
  }

  data.frame(
    old_co = df$co[id],
    id = id,
    co = res,
    energy = compute_energy(df$co[id], res)*en_vec[df$pos[id]]
  )
}

aggregate_valid_dest <- function(game) {

  game0 <- game[!game$is_home,]
  all_dest <- do.call(rbind, lapply(1:nrow(game0), \(i) find_valid_dest(game, game0$id[i])))
  if (any(Im(all_dest$co) != 1)) return(subset(all_dest, Im(co) != 1)[1,])
  all_dest
}


update_game <- function(game, id, new_co) { #update game: who(id) goes where (new_co)
  game$co[id] <- new_co
  game$is_home[id] <- Re(new_co) == game$dest[id]
  return(game)
}


compute_energy <- function(z1, z2) {
  ifelse(Im(z1) == 1 | Im(z2) == 1, l1_dist(z1, z2), (Im(z1) - 1) + (Im(z2) - 1) + abs(Re(z1) - Re(z2)))
}

l1_dist <- function(z1, z2) abs(Re(z1) - Re(z2)) + abs(Im(z1) - Im(z2))

en_vec <- c("A" = 1, "B" = 10, "C" = 100, "D" = 1000)


df2id <- function(df) {
  id0 <- c(setNames(df$co, df$pos), setNames(setdiff(map, df$co), rep(".", length(map) - nrow(df))))
  paste0(names(id0[order(Re(id0), Im(id0))]), collapse = "")

}


id2df <- function(id) {
  id0 <- strsplit(id, "")[[1]]
  data.frame(co = map[id0 != "."], pos = id0[id0 != "."])
}

#go---------
input_file <- "Input/day23_p2.txt"
# input_file <- "test.txt"
x <- as.matrix(read.fwf(input_file, width = rep(1, 13), comment.char = ""))
map <- apply(which(x != "#" & x != " ", arr.ind = TRUE), 1, \(x) x[2] - 1 + (x[1] - 1) * 1i)

mappos <- x[x != "#" & x != " " & !is.na(x)]

wid <- max(Im(map))

pos_d2 <- data.frame(
  pos = mappos[mappos != "."], co = map[mappos != "."], id = seq_len(sum(mappos != "."))
)

pos_d2$dest = c("A" = 3, "B" = 5, "C" = 7, "D" = 9)[pos_d2$pos]

pos_d2$is_home <- is.home(pos_d2)

game0 <- pos_d2
game <- game0

min_energy <- function(game) {

  id <- df2id(game)

  if (id %in% names(envir$lu)) return(envir$lu[id])

  if (all(game$is_home)) return(0)

  next_dest <- aggregate_valid_dest(game)

  if (nrow(next_dest) == 0) {
    res <- Inf
  } else {
    game_list <- lapply(1:nrow(next_dest), \(j) update_game(game, next_dest[j,]$id, next_dest[j,]$co))

    res <- min(sapply(seq_along(game_list), \(j) next_dest[j,]$energy + min_energy(game_list[[j]])))
  }
  envir$lu <- c(envir$lu, setNames(res, id))
  return(res)
}

envir <- environment()
envir$lu <- numeric()

min_energy(game)
#51722 too low
