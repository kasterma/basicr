test_f1 <- function(x) {
  x <- x + x
  x
}

test_f2 <- function(x) {
  on.exit(print(paste("x was:", x)))
  x <- x + 3
  x + 2
}

nu_val <- 0.7

run_sample <- function(nu = nu_val, n = 50) {
  xs <- rbinom(n = n, size = 1, prob = nu)
  mean(xs)
}
