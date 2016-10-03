# working out factors as used in PGM

library(testthat)

new_factor <- function(idxs, scope_sizes, values) {
  stopifnot(!missing(idxs), !missing(scope_sizes), !missing(values))
  list(idxs = idxs, scope_sizes = scope_sizes, values = values)
}

factor1 <- new_factor(1, 2, c(0.11, 0.89))
factor2 <- new_factor(c(2, 1), c(2, 2), c(0.59, 0.41, 0.22, 0.78))
factor3 <- new_factor(c(3, 2), c(2, 2), c(0.39, 0.61, 0.06, 0.94))

index_to_assignment <- function(idx, scope_sizes) {
  idx_repeated <- matrix(rep(idx - 1, length(scope_sizes)), nrow = length(idx))
  scope_cp <- cumprod(c(1, scope_sizes[-length(scope_sizes)]))
  t(floor( t(idx_repeated) / scope_cp) %% scope_sizes + 1)
}

test_that("index_to_assignment agrees with given octave version", {
  expect_equal(index_to_assignment(c(1,2,3), c(2,2)),
               structure(c(1, 2, 1, 1, 1, 2), .Dim = c(3L, 2L)))
  expect_equal(index_to_assignment(c(1,2,3,4), c(3, 2)),
               structure(c(1, 2, 3, 1, 1, 1, 1, 2), .Dim = c(4L, 2L)))
})

assignment_to_index <- function(assignments, scope_sizes) {
  scope_cp <- cumprod(c(1, scope_sizes[-length(scope_sizes)]))
  idxs <- (assignments - 1) %*% scope_cp + 1
  as.double(idxs)
}

test_that("assignment_to_index agrees with given octave version", {
  expect_equal(assignment_to_index(index_to_assignment(c(1,2,3,4), c(2, 2)),
                                   c(2,2)),
               c(1,2,3,4))
  expect_equal(assignment_to_index(c(3, 2), c(3, 2)), 6)
})
