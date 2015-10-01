page_rank <- function(R, M, B, N, P = FALSE) {
  Error <- 10 ^ -6
  Distance <- .Machine$integer.max
  Index <- 0
  while (Distance > Error & Index < 10000) {
    A <- B * M + (1 - B) * matrix(1, N, N) / N
    Temp <- R
    R <- A %*% R
    # Distance <- sum((R - Temp^2)
    Distance <- dist(rbind(t(R), t(Temp)))
    Index <- Index + 1
    if(P == FALSE) next
    if(Index == 4 | Index == 5) {
      cat(Index, " : ", R, "\n")
    }
  }
  R
}
# Q1
B <- 0.7
N <- 3
M <- matrix(c(0, 0, 0, 0.5, 0, 0, 0.5, 1, 1), 3, 3, TRUE)
R <- matrix(c(1 / N, 1 / N, 1 / N), 3, 1, TRUE)
r <- page_rank(R, M, B, N)
cat("Q1:", r * 3, "\n")
# Q2
B <- 0.85
N <- 3
M <- matrix(c(0, 0, 1, 0.5, 0, 0, 0.5, 1, 0), 3, 3, TRUE)
R <- matrix(c(1 / N, 1 / N, 1 / N), 3, 1, TRUE)
r <- page_rank(R, M, B, N)
cat("Q2:", r, "\n")
# Q3
B <- 1
N <- 3
M <- matrix(c(0, 0, 1, 0.5, 0, 0, 0.5, 1, 0), 3, 3, TRUE)
R <- matrix(c(1, 1, 1), 3, 1, TRUE)
r <- page_rank(R, M, B, N, TRUE)
cat("Q3:", r, "\n")
# Q4
# 2, 24 + 30
# 3, 15 + 21 + 24 + 30
# 5, 15 + 30
# 7, 21 + 49

