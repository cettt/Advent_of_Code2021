data05 <- sapply(strsplit(readLines("Input/day05.txt"), "\\D+"), as.integer)

z_lines <- apply(data05, 2, \(x) seq(x[1], x[3]) + seq(x[2], x[4])*1i)

#part1----
idx <- colSums(data05[1:2, ] == data05[3:4, ]) == 1
length(unique(unlist(z_lines[idx])[duplicated(unlist(z_lines[idx]))]))

#part2----
length(unique(unlist(z_lines)[duplicated(unlist(z_lines))]))
#using table to find unique values is 5-10 slower than this `ugly` approach
