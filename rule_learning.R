#' Learning rules for generating numbers

library(ggvis)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(dplyr)
library(pryr)
library(testthat)

# plot three graphs next to eachother:
# 1) prior
# 2) likelihood
# 3) posterior

# set up set of rules with distribution

RANGE <- data.frame(x = seq_len(100))

make_mult_rule <- function(m) {
  nos <- RANGE %>% filter(x %% m == 0)
  list(contains = f(x, x %in% nos$x), size = nrow(nos), nos = nos)
}

expect_equal(c(make_mult_rule(50)$contains(6),
               make_mult_rule(50)$contains(50),
               make_mult_rule(50)$contains(66)),
             c(FALSE, TRUE, FALSE))
expect_equal(table(sapply(RANGE, make_mult_rule(50)$contains)),
             structure(c(98L, 2L),
                       .Dim = 2L,
                       .Dimnames = structure(list(c("FALSE", "TRUE")),
                                             .Names = ""), class = "table"))

make_power_rule <- function(m) {
  nos <- RANGE %>% mutate(x = m ** x) %>% filter(x <= max(RANGE$x))
  list(contains = f(x, x %in% nos$x), size = nrow(nos), nos = nos)
}

expect_equal(c(make_power_rule(3)$contains(4),
               make_power_rule(3)$contains(27),
               make_power_rule(3)$contains(99)),
             c(FALSE, TRUE, FALSE))
expect_equal(table(sapply(RANGE, make_power_rule(3)$contains)),
             structure(c(96L, 4L),
                       .Dim = 2L,
                       .Dimnames = structure(list(c("FALSE", "TRUE")),
                                             .Names = ""), class = "table"))

make_given_set_rule <- function(xs) {
  nos <- data.frame(x = xs)
  list(contains = f(x, x %in% nos$x), size = nrow(nos), nos = nos)
}

expect_equal(c(make_given_set_rule(c(3,4,5))$contains(6),
               make_given_set_rule(c(3,4,5))$contains(4),
               make_given_set_rule(c(3,4,5))$contains(99)),
             c(FALSE, TRUE, FALSE))
expect_equal(table(sapply(RANGE, make_given_set_rule(c(3,4,5))$contains)),
             structure(c(97L, 3L),
                       .Dim = 2L,
                       .Dimnames = structure(list(c("FALSE", "TRUE")),
                                             .Names = ""), class = "table"))

rules <- list(powers_of_2 = make_power_rule(2),
              powers_of_3 = make_power_rule(3),
              powers_of_4 = make_power_rule(4),
              even = make_mult_rule(2),
              odd = make_given_set_rule(RANGE %>% filter(x %% 2 == 1)),
              mult_of_3 = make_mult_rule(3),
              mult_of_4 = make_mult_rule(4),
              all = make_given_set_rule(RANGE$x))

prior <- c(rep(2,3), rep(4, 2), rep(1,2), 5)
prior <- prior/sum(prior)

# given some numbers compute likelihood

rule_lik <- function(rule, D) {
  if (all(sapply(D, rule$contains))) {
    p <- 1/rule$size
    p ^ length(D)
  } else {
    0.0
  }
}

lik <- function(D) {
  sapply(rules, f(rule, rule_lik(rule, D)))
}

# compute posterior

post <- function(D, likelihood = NULL) {
  if (missing(likelihood)) {
    prior * lik(D)
  } else {
    prior * likelihood
  }
}

# plot in three graphs
# also with ggplot2

dat_p1 <- data.frame(n = names(rules), p = prior)

dat_p1 %>%
  ggvis(x = ~p, y = ~n) %>%
  layer_rects(x2 = 0, height = band(), fill := "darkblue")

ggplot(dat_p1) +
  geom_bar(aes(x = n, y = p, fill = "darkblue"), stat = "identity") +
  coord_flip() +
  theme(legend.position = "none")


dat_p2 <- data.frame(n = names(rules), p = lik(16))

dat_p2 %>%
  ggvis(x = ~p, y = ~n) %>%
  layer_rects(x2 = 0, height = band(), fill := "darkblue")

ggplot(dat_p2) +
  geom_bar(aes(x = n, y = p, fill = "darkblue"), stat = "identity") +
  coord_flip() +
  theme(legend.position = "none")


dat_p3 <- data.frame(n = names(rules), p = post(16))

dat_p3 %>%
  ggvis(x = ~p, y = ~n) %>%
  layer_rects(x2 = 0, height = band(), fill := "darkblue")

ggplot(dat_p3) +
  geom_bar(aes(x = n, y = p, fill = "darkblue"), stat = "identity") +
  coord_flip() +
  theme(legend.position = "none")

# combine the three graphs into one

p1 <- ggplot(dat_p1) +
  geom_bar(aes(x = n, y = p, fill = "darkblue"), stat = "identity") +
  coord_flip() +
  theme(legend.position = "none")

p2 <- ggplot(dat_p2) +
  geom_bar(aes(x = n, y = p, fill = "darkblue"), stat = "identity") +
  coord_flip() +
  theme(legend.position = "none")

p3 <- ggplot(dat_p3) +
  geom_bar(aes(x = n, y = p, fill = "darkblue"), stat = "identity") +
  coord_flip() +
  theme(legend.position = "none")

grid.arrange(p1, p2, p3, nrow = 1)

# point estimate

pt_prob <- function(D, x) {
  rule_ps <- post(D)
  name_2_prob <- function(name) {
    if (rules[[name]]$contains(x))
      rule_ps[name] * 1/rules[[name]]$size
    else
      0.0
  }
  setNames(sapply(names(rule_ps), name_2_prob), names(rule_ps))
}

test_that("pt_prob works as advertised", {
  res_16_2 <- pt_prob(16, 2)
  expect_true(res_16_2['powers_of_2'] > res_16_2['all'])
  expect_equal(unname(res_16_2['odd']), 0)
  expect_equal(unname(res_16_2['mult_of_4']), 0)
  expect_equal(res_16_2,
               structure(c(0.00264550264550265, 0, 0, 7.61904761904762e-05,
                           0, 0, 0, 2.38095238095238e-05),
                         .Names = c("powers_of_2","powers_of_3",
                                    "powers_of_4", "even", "odd", "mult_of_3",
                                    "mult_of_4", "all")))
  expect_equal(res_16_2, post(c(16,2)))
})
