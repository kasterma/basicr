#' clustering in one dimension
#'
#' One use of this would be to determine sessions of users based on intervals
#' of activity.  Then the question is how to decide on the number of intervals.



# setup -------------------------------------------------------------------

library(pryr)
library(foreach)
library(ggplot2)

# gen data ----------------------------------------------------------------

xs <- runif(20, min = 0, max = 3)

plot(xs, rep(0, length(xs)))


# clustering --------------------------------------------------------------

run_outputs <- foreach(idx = seq(10)) %do% kmeans(xs, 4)
do.call(cbind, lapply(run_outputs, f(x, sort(x$centers))))

ggplot(data.frame(x=xs), aes(x=x, y = 0)) + geom_point()
