library(pryr)
library(testthat)
library(ggplot2)

rdirichlet <- function(alphas) {
  ys <- sapply(alphas, f(alpha, rgamma(1, alpha)))
  sum_y <- sum(ys)
  sapply(ys, f(y, y/sum_y))
}

test_that("rdirichlet gives a dirichlet", {
  expect_equal(sum(rdirichlet(c(1.3, 1.5))), 1.0)
})

vals <- replicate(1000, rdirichlet(c(.3,.3,.3)))

df <- data.frame(x=vals[1,],y=vals[2,])
ggplot(df,aes(x=x,y=y))+
  stat_density2d(aes(fill=..level..), geom="polygon", n = 600) +
  scale_fill_gradient(low="blue", high="green") +
  geom_line(data=data.frame(x=c(0,1), y = c(1,0)))

sum(x > 0.5 & y > 0.5)
