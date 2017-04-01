#' https://en.wikipedia.org/wiki/Perplexity

library(testthat)
library(purrr)

entropy <- function(ps) {
  stopifnot(all(ps >= 0))
  ps <- ps %>% keep(~ .x > 0) %>% {./sum(.)}  ## rounding error problems
  print(ps)
  - sum(ps * log2(ps))
}

entropy(c(1,1,1,1,1))
entropy(c(1,1,1,1,1,1,1,1))
entropy(c(1, 0.1, 0.1))
entropy(c(1,1,1))
entropy(c(2, 0.1, 0.1))
entropy(c(1,0,0))

persplexity <- function(ps) {
  2 ^ entropy(ps)
}
