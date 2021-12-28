data24 <- strsplit(readLines("Input/day24.txt"), " ")

var_vec <- sapply(data24[seq_along(data24) %% 18 %in% c(5, 6, 16)], \(z) z[3])
var_mat <- matrix(as.integer(var_vec), ncol = 3, byrow = TRUE)

eq_idx <- which(var_mat[,2] < 9)
df <- data.frame(x1 = eq_idx, x2 = 0, x1min = 0, x2min = 0, x1max = 0, x2max = 0)

for (k in seq_along(eq_idx)) {
  df[k, 2] <- max(setdiff(seq_len(eq_idx[k]), c(df[,1], df[,2])))
  b <- var_mat[df[k, 2], 3] + var_mat[eq_idx[k], 2]
  df[k, c(4, 6)] <- range(seq_len(9)[seq_len(9) + b > 0 & seq_len(9) + b < 10])
  df[k, c(3, 5)] <- df[k, c(4, 6)] + b
}

res <- rbind(as.matrix(df[,c(1,3,5)]), as.matrix(df[,c(1,3,5) + 1]))

#part 1-----
paste0(res[order(res[,1]), 3], collapse = "")

#part2------
paste0(res[order(res[,1]), 2], collapse = "")


#explanation:
# first we can observe that the instruction (almost) repeats after 18 lines.
#   i.e. lines 1, 19, 37 are the same, and so are 2, 20, 38 etc.
# we call each set of 18 (almost identical) lines a buckets
# each bucket starts with an input instruction and there are exactly 14 buckets.
#   so each bucket handles on of the 14 digits
# analyzing one of these buckets reveals the following pattern:
#    - we first set w to current digit d
#    - next we check if (z %% 26 + var2) is equal to d
#        - if yes: z is set to z / var1
#        - if no;  z is set to z / var1 * 26 + d + var3
#    - note that var1, var2 and var3 are parameters:
#       - var1 appears in the fifth line of each bucket (i.e. line 5, 23, 41, ...)
#       - var2 appears in the sixth line of each bucket (i.e. line 6, 24, 42, ...)
#       - var3 appears in the  16th line of each bucket (i.e. line 18,34, 60, ...)
#   - in fact all the other lines (which do not contain var1, var2, var3) are equal across buckets.
# In order for z to be equal to zero at the end we need to have equality as often as possible
# Equality can be achieved if var2 < 9.
# So every bucket where var2 < 9 we get a linear equation in z and the current digit d
#  This equation involves z %% 26
#  By observing the pattern we see that z %% 26 is equal to d[j] + var3[j]
#    where j is the last index where equality was not achieved
# Therefore each equality is a condition on two separate digits
#   these equations always are of the form d[j] + var3[j] + var2[i] = d[i]
# We see that in total we get 7 simple equations each linking two digits
# From there it is easy to find the correct answer
# To verify the following function could be used:
# run_prog <- function(d) {
#   z <- 0
#   for (k in 1:14) {
#     check <- (z %% 26 + var_mat[k, 2] != d[k])
#     z <- floor(z / var_mat[k, 1])
#     if (check) z <- z * 26 + d[k] + var_mat[k, 3]
#   }
#   return(z)
# }
