
# Concentration of measure ------------------------------------------------

# quick experiment showing some concentration of measure phenomenom
#
# The below shows that the probability mass in high dimensions is close to
# radius 1; where the density is still highest at the origin.

# setup -------------------------------------------------------------------

library(pryr)
library(ggplot2)

# radial component iid sum normals ----------------------------------------

dist_0 <- function(x) sqrt(sum(x^2))

draw <- f(dist_0(rnorm(n))/sqrt(n))

low_dim <- replicate(10000, draw(1))
med_dim <- replicate(10000, draw(10))
high_dim <- replicate(10000, draw(10000))
dat <- rbind(data.frame(xs = low_dim, lab = "low"),
             data.frame(xs = med_dim, lab = "med"),
             data.frame(xs = high_dim, lab = "high"))

ggplot(dat, aes(x = xs, color = lab)) + geom_histogram(binwidth = 0.05)
