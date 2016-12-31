library(testthat)

# Convert a Data Frame to a Numeric Matrix
data.matrix(data.frame(x = c(1,2,3), y = 4))
data.matrix(data.frame(x = c(1,2,3), y = c("a", "b", "c")))
data.matrix(data.frame(x = c(1,2,3), y = c("a", "b", "c"), stringsAsFactors = FALSE))

match(c(1,2,3), c(0,0,1,3,3))
match(c(0,1,2), c(0,1,2), incomparables = c(0,1), nomatch = -1)

# mapply use for zipping
expect_equal(mapply(c, c(11,22,33), c(44,55,66), SIMPLIFY = FALSE),
             list(c(11,44), c(22, 55), c(33, 66)))

# Using assignment in the argument to a function can lead to surprising
# behavior; lazy evaluation can lead to an argument not always being evaluated.
f_a <- function(x, y) {
  3 * y
}

f_a(x <- 3, 5)
expect_error(x, regexp = "object 'x' not found", fixed = TRUE)


expect_equal(c(list(1,2,3), list(4,5,6)), list(1,2,3,4,5,6))
expect_equal(c(list(1,2,3), list(4,5,6), recursive = TRUE), 1:6)
expect_equal(c(list(1, list(2,3)), list(list(4,list(5,6))), recursive = TRUE),
             1:6)
expect_equal(c(list(x = 2, y = 4), recursive = TRUE), c(x = 2, y = 4))
expect_equal(c(list(x = 2, y = 4), recursive = TRUE, use.names = FALSE), c(2,4))
