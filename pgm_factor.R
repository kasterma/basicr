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

union_with_locations <- function(idxs_a, idxs_b) {
  vars <- union(idxs_a, idxs_b)
  a_map <- match(vars, idxs_a, nomatch = 0)
  b_map <- match(vars, idxs_b, nomatch = 0)

  list(vars = vars, a_map = a_map, b_map = b_map)
}

factor_product <- function(f1, f2) {
  if (is.null(f1$idxs)) {
    return(f2)
  }
  if (is.null(f2$idxs)) {
    return(f1)
  }

  union_wl <- union_with_locations(f1$idxs, f2$idxs)

  idxs <- union_wl$vars
  scope_sizes <- pmax(f1$scope_sizes[union_wl$a_map],
                      f2$scope_sizes[union_wl$b_map])

  assignm <- index_to_assignment(1:prod(scope_sizes), scope_sizes)
  a_idx <- assignment_to_index(assignm[,union_wl$a_map, drop = FALSE],
                               f1$scope_sizes)
  b_idx <- assignment_to_index(assignm[,union_wl$b_map, drop = FALSE],
                               f2$scope_sizes)
  values <- f1$values[a_idx] * f2$values[b_idx]

  new_factor(idxs, scope_sizes, values)
}

test_that("factor_product agrees with given octave version", {
  expect_equal(factor_product(factor1, new_factor(c(), c(), c())),
              factor1)
  expect_equal(factor_product(new_factor(c(), c(), c()), factor1),
               factor1)
  expect_equal(factor_product(factor1, factor2),
               list(idxs = c(1, 2),
                    scope_sizes = c(2, 2),
                    values = c(0.0649, 0.1958, 0.0451, 0.6942)))
  expect_equal(union_with_locations(c(1,2,3), c(2,3,4)),
               list(vars = c(1,2,3,4),
                    a_map = c(1,2,3,0),
                    b_map = c(0,1,2,3)))
})
