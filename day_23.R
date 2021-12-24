# input_file <- "Input/day23_p2.txt"
input_file <- "test.txt"
x <- as.matrix(read.fwf(input_file, width = rep(1, 13), comment.char = ""))

z <- complex(real = rep(1:ncol(x), each = nrow(x)), imaginary = rep(1:nrow(x), ncol(x)))
map <- z[grepl("\\w|\\.", x)]
bottom <- max(Im(map))
state0 <- z[x %in% LETTERS[1:4]][order(x[x %in% LETTERS[1:4]])]

label <- rep(1:4, each = length(state0) / 4)
targ_room <- sort(Re(map[Im(map) > 2]))
en_vec <- rep(10^(0:3), each = length(state0) / 4)
hallway <- map[Im(map) == 2 & !Re(map) %in% targ_room]



find_new_states <- function(state) {
  amp <- .Internal(which(Re(state) %in% targ_room[!sapply(targ_room, \(x) all(targ_room[Re(state) == x] == x))]))
  amp <- c(amp, .Internal(which(Im(state) == 2)))

  new_states <- list()
  for (a in amp) {
    if (!state[a] %in% hallway) {

      if (!any(Re(state) == Re(state[a]) & Im(state) < Im(state[a]))) {
        occ_hallway <- state[Im(state) == 2]
        hw_r <- min(Re(occ_hallway[Re(occ_hallway) > Re(state[a])]), 13)
        hw_l <- max(Re(occ_hallway[Re(occ_hallway) < Re(state[a])]), 1)
        targ <- hallway[Re(hallway) > hw_l & Re(hallway) < hw_r]

        if (length(targ) > 0) {
          new_states <- c(new_states, lapply(targ, \(z) {x <- state; x[a] <- z; return(x)}))
        }
      }
    } else if (all(targ_room[Re(state) == targ_room[a]] == targ_room[a])) { #check if target room is free

      if (sum(Re(state[Im(state) == 2]) %in% seq(Re(state[a]), targ_room[a])) == 1L) { #if the way to the room is free
        state[a] <- targ_room[a] + (bottom - sum(Re(state) == targ_room[a])) * 1i
        return(list(state))
      }
    }
  }
  return(new_states)

}

state2int <- \(st) sum(sapply(map, \(x) c(label[st == x], 0)[1]) * 5^(seq_along(map) - 1))

compute_energy <- function(state1, state2) {
  sum((abs(Re(state1 - state2))  + abs(Im(state1 - state2)))*en_vec)
}
solved_state <- map[Im(map) != 2]


cur_int <- state2int(state0)
state_en_vec <- 0L
state_vec <- state2int(state0)
solved_state_int <- state2int(solved_state)
q <- collections::priority_queue(list(state0), priorities = 0L)

while (cur_int != solved_state_int) {
  cur <- q$pop()
  cur_int <- state2int(cur)
  cur_en <- state_en_vec[state_vec == cur_int]
  new_states <- find_new_states(cur)

  for (ns in new_states) {
    ns_id <- state2int(ns)
    ns_en <- compute_energy(cur, ns) + cur_en
    if (!ns_id %in% state_vec) {
      state_vec <- c(state_vec, ns_id)
      state_en_vec <- c(state_en_vec, ns_en)
      q$push(ns, priority = -ns_en)
    } else if (state_en_vec[state_vec == ns_id] > ns_en) {
      state_en_vec[state_vec == ns_id] <- ns_en
      q$push(ns, priority = -ns_en)
    }
  }
}
cur_en
