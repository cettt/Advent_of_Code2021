move_amphipods <- function(inp) {

  z <- complex(real = rep(1:ncol(inp), each = nrow(inp)), imaginary = rep(1:nrow(inp), ncol(inp)))
  map <- z[grepl("\\w|\\.", inp)]
  targ_room <- as.integer(sort(Re(map[Im(map) > 2])))
  bottom <- nrow(inp) - 1L
  map <- map[Im(map) > 2 | !Re(map) %in% targ_room] # remove spots before entrance from map

  state0 <- z[inp %in% LETTERS[1:4]][order(inp[inp %in% LETTERS[1:4]])]
  solved_state <- map[Im(map) != 2]

  label <- rep(1:4, each = length(state0) / 4)
  en_vec <- rep(10^(0:3), each = length(state0) / 4)
  hallway <- as.integer(Re(map[Im(map) == 2]))

  classify_room <- function(room, state) {
    idx <- Re(state) == room
    if (all(targ_room[idx] == room)) 2L else if (sum(idx) == bottom - 2) 1L else 0L
  }

  find_new_states <- function(state) {
    Rest <- as.integer(Re(state))
    Imst <- as.integer(Im(state))
    room_cat <- sapply(c(4L, 6L, 8L, 10L), classify_room, state = state)
    occ_hallway <- as.integer(Rest[Imst == 2L])


    for (a in .Internal(which(Imst == 2L))) {
      if (room_cat[targ_room[a] / 2L - 1L] == 2L) { #check if target room is free
        hw_r <- min(occ_hallway[occ_hallway > Rest[a]], 13L)
        hw_l <- max(occ_hallway[occ_hallway < Rest[a]], 1L)
        if (targ_room[a] < hw_r & targ_room[a] > hw_l) { #if the way to the room is free
          state[a] <- targ_room[a] + (bottom - sum(Rest == targ_room[a])) * 1i
          return(list(state)) #if we find an amphipod which can go home it goes home
        }
      }
    }


    if (sum(room_cat == 0L) == 1L) {
      amp <- .Internal(which(Rest %in% c(4L, 6L ,8L ,10L)[room_cat == 0L]))
    } else {
      amp <- .Internal(which(Rest %in% c(4L, 6L, 8L, 10L)[room_cat != 2L]))
    }

    new_states <- list()

    for (a in amp) {

      if (!(state[a] - 1i) %in% state) {
        hw_r <- min(occ_hallway[occ_hallway > Rest[a]], 13L)
        hw_l <- max(occ_hallway[occ_hallway < Rest[a]], 1L)

        if (room_cat[targ_room[a] / 2L - 1L] == 2L) {
          if (targ_room[a] < hw_r & targ_room[a] > hw_l) {
            state[a] <- targ_room[a] + sign(Rest[a] - targ_room[a]) + 2*1i
            return(list(state))
          }
        }

        targ <- hallway[hallway > hw_l & hallway < hw_r] + 2*1i
        if (length(targ) > 0) {
          new_states <- c(new_states, lapply(targ, \(z) {x <- state; x[a] <- z; return(x)}))
        }
      }
    }

    return(new_states)
  }

  pow5 <- 5^(seq_along(map) - 1)
  state2int <- \(st) sum(sapply(map, \(x) c(label[st == x], 0)[1]) * pow5)

  compute_energy <- function(state1, state2) {
    sum((abs(Re(state1 - state2)) + abs(Im(state1 - state2)))*en_vec)
  }


  cur_int <- state2int(state0)
  state_en_vec <- 0L
  state_vec <- cur_int
  # parent_vec <- NA_real_
  solved_state_int <- state2int(map[Im(map) != 2])
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
      } else {
        idx <- state_vec == ns_id
        if (state_en_vec[idx] > ns_en) {
          state_en_vec[idx] <- ns_en
          q$push(ns, priority = -ns_en)
        }
      }
    }
  }
  return(cur_en)
}

#part 1--------
data23 <- as.matrix(read.fwf("Input/day23.txt", width = rep(1, 13), comment.char = ""))
move_amphipods(data23)


#part2-------
new_lines <- do.call(rbind, strsplit(c("  #D#C#B#A#  ", "  #D#B#A#C#  "), ""))
new_mat <- rbind(data23[1:3, ], new_lines, data23[4:5, ])
move_amphipods(new_mat)


#explanation

# The main idea is to use Dijkstra's algorithm:
# - each  state is stored as a numeric using the function state2int
# - In order to find adjacent states we do some pruning:
#   - First, we check if any amphipod in the hallway can move: if yes only that amphipod moves
#   - Second, we classify all the rooms into on of three categories
#    - 1: room is unchanged since start
#    - 2: either the room is empty or only the correct amphipods are inside
#    - 0: everything else
#   amphipods in room with category 2 do not have to move anymore
#   if there is one room with category 0 only the top amphipod in that room moves
#   otherwise we allow the top amphipods in rooms with category 0 and 1 to move
