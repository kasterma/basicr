## plot a bar graph for the random vector xs with bar width computed by
## quantile function.  And the bars should have the right width.

library(ggvis)
library(dplyr)

set.seed(481)
xs <- rnorm(n = 1000)
qs <- quantile(xs, probs = c(0, 0.001, 0.01, 0.4, 1))
breaks <- qs
names(breaks) <- NULL
breaks[1] <- breaks[1] - 0.001
breaks[length(breaks)] <- breaks[length(breaks)] + 0.001

intervals <- data.frame(x_min = breaks[-length(breaks)],
                        x_max = breaks[-1])

get_interval <- function(x) {
  data.frame(x = x, intervals %>% filter(x > x_min) %>% filter( x <= x_max))
}

get_interval(xs[10])

dat <- data.frame(x = xs) %>% rowwise %>% do(get_interval(.$x))

xs_cut <- cut(xs, breaks = breaks)
intervals$names <- levels(xs_cut)
dat <- table(xs_cut)
dat <- as.data.frame(dat)

plt_dat <- merge(dat, intervals, by.x = "xs_cut", by.y = "names")

plt <- plt_dat %>% ggvis() %>% layer_rects(x = ~x_min + 0.02, x2 = ~x_max - 0.02, y = 0, y2 = ~Freq)
plt
export_png(plt, file = "~/Desktop/test.png")

cxs <- cut(xs, breaks = quantile(xs))
data.frame(x = cxs) %>% ggvis(~x) %>% layer_histograms()   # error: range not meaningful for factors
data.frame(x = cxs) %>% ggvis(~x) %>% layer_bars()         # not what we want; bars are shown but labelled with the interval
cut(xs, breaks = qs, labels = qs)
cut(xs, breaks = qs, labels = qs[1:4], include.lowest = TRUE)
levels(cut(xs, breaks = qs, labels = qs[1:4], include.lowest = TRUE))
quantile(xs, probs = c(0, 0.001, 0.01, 0.1, 1))
quantile(xs, probs = c(0, 0.001, 0.01, 0.1, 1), names = FALSE)





## functions to look at later:
##
## ecdf (I think we might be trying to plot part of its graph)
