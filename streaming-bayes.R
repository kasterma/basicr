# http://koaning.io/bayesian-propto-streaming-algorithms.html

library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(testthat)

n <- 25

pts <- data.frame(x = runif(n, min = 0, max = 2)) %>%
  mutate(y = 2.5 + 3.5 * x + rnorm(n, mean = 0, sd = 0.3))
ggplot(pts, aes(x, y)) + geom_point()

likelihood <- function(x, y, n = 100, bmin = -5, bmax = 5) {
  lik_f <- function(b0, b1) exp(-(b0 + b1 * x - y)**2 / 2)  # no need to normalize, done on return

  w0 <- seq(from = bmin, to = bmax, length.out = n)
  w1 <- seq(from = bmin, to = bmax, length.out = n)
  ws <- crossing(w0, w1) %>% mutate(val = lik_f(w0, w1)) %>% mutate(val = val / sum(val))
}

vals_12 <- likelihood(1, 2)
ggplot(vals_12, aes(w0, w1, color = val)) + geom_point(size = 2) +
  scale_colour_gradientn(colours = rev(rainbow(3)))

vals <- pts %>% rowwise %>% do(data.frame(x = .$x, y = .$y, likelihood(.$x, .$y, n = 20))) %>% ungroup
ggplot(vals, aes(w0, w1, color = val)) + geom_point() +
  facet_wrap(~ x, nrow = 5) +
  scale_colour_gradientn(colours = rev(rainbow(2)))

## next is nonsense, but normalized per graph to have max = 1
ggplot(vals %>% group_by(x) %>% mutate(val = val / max(val)), aes(w0, w1, color = val)) + geom_point() +
  facet_wrap(~ x, nrow = 5) +
  scale_colour_gradientn(colours = rev(rainbow(2)))

plt_dim <- 50
init <- do.call(likelihood, as.list(c(pts[1,], plt_dim))) %>% mutate(val = 1)
dists <- list(init %>% mutate(id = 0))
for (idx in seq_len(n)) {
  new_vals <- do.call(likelihood, as.list(c(pts[idx,], plt_dim)))$val
  dists[[idx + 1]] <- dists[[idx]] %>% mutate(id = idx, val = val * new_vals) %>%
    mutate(val = val / sum(val))
}
all_vals <- Reduce(rbind, dists[-1])

ggplot(all_vals, aes(w0, w1, color = val)) + geom_point(size = 0.5) +
  facet_wrap(~ id, nrow = 5) +
  scale_colour_gradientn(colours = rev(rainbow(3)))
