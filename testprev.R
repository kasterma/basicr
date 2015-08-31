## not too many per time
##
## We have a data frame with events happening at different times.  These
## events happen to different items identified by id.  We want to see that
## per given time length there are not more than 2 events.
##
## A nice generalization would be to have not more than k events per time
## length.

library(dplyr)
library(data.table)
library(pryr)
library(magrittr)
library(testthat)

## set up testdata -------------------------------------------------------------

df1 <- data.frame(id=1, time = c(1,2,5), val = runif(3))
df2 <- data.frame(id=2, time = c(2,1,3), val = runif(3))
df_test <- rbind(df1, df2)

## some dplyr solutions --------------------------------------------------------

df_test %>% group_by(id) %>% arrange(time) %>%
  mutate(since_last = time - lag(time),
         since_before_last = time - lag(time, 2)) %>% rowwise %>%
  mutate(ct = sum(c(since_last <= 2, since_before_last <= 2), na.rm = TRUE))

df_test %>% group_by(id) %>% arrange(time) %>%
  mutate(since_last = time - lag(time),
         since_before_last = time - lag(time, 2)) %>%
  ungroup %>%
  mutate(ct = rowSums(cbind(since_last <= 2, since_before_last <= 2), na.rm = TRUE))

#' sum booleans related to time differences
#' has property that if is.na(x), then also is.na(y)
sumtimeboolean <- function(x,y) ifelse(is.na(x), 0, ifelse(is.na(y), x, x + y))
## using pryr::f we could also write this in the following form
sumtimeboolean <- f(ifelse(is.na(x), 0, ifelse(is.na(y), x, x + y)))
## Note: f correctly identifies that x and y are to be the arguments

expect_equal(sumtimeboolean(1, NA), 1)
expect_equal(sumtimeboolean(c(1,1), c(NA, 3)), c(1,4))
expect_equal(sumtimeboolean(c(NA, 1, 2), c(1, NA, 2)), c(0, 1, 4))

df_test %>% group_by(id) %>% arrange(time) %>%
  mutate(last = time - lag(time),
         llast = time - lag(time, 2),
         ct = sumtimeboolean(last <= 2, llast <= 2))

## the following is not correct, ct is computed per group as sum
## of all booleans in group
df_test %>% group_by(id) %>% arrange(time) %>%
  mutate(last = time - lag(time),
         llast = time - lag(time, 2),
         ct = sum(c(last <= 2, llast <= 2), na.rm = TRUE))

## some data.table solutions ---------------------------------------------------

dt_test <- as.data.table(df_test)

dt_test[order(time)
        ][,.(time,
             val,
             since_last = time-lag(time),
             since_before_last = time - lag(time, 2)),
          id][,.(val, since_last, since_before_last,
                 ct = sum(c(since_last <= 2, since_before_last <= 2), na.rm = TRUE)),
              .(id, time)]

dt_test[order(time)
        ][,.(time,
             val,
             since_last = time-lag(time),
             since_before_last = time - lag(time, 2)),
          id][,.(id, time, val, since_last, since_before_last,
                 ct = rowSums(cbind(since_last <= 2, since_before_last <= 2), na.rm = TRUE)),]


prevf <- function(l, t) {
  lapply(t, f(t0, as.list(Filter(f(tt, tt <= t0 && t0 - 3600 * 24 <= tt), l))))
}

df3 <- df_test %>% group_by(id) %>% arrange(time) %>%
  do({d <- data.frame(time = .$time); d$prev=prevf(.$time, .$time); d}) %>%
  mutate(len = sapply(prev, length))

bb <- tbl_df(read.csv("~/Downloads/bs.csv",
                      header = FALSE,
                      sep = "\t",
                      col.names = c("iid", "id", "timestamp"))) %>%
  mutate(timestamp = timestamp/1000)

bb %>% group_by(iid, id) %>% slice(1L) %>% ungroup %>%
  group_by(id) %>%x0
  arrange(timestamp) %>%
  mutate(timediff = timestamp - lag(timestamp)) %>%
  filter(timediff < 60)

ww <- tbl_df(read.csv("~/downloads/ws.csv",
                      header = FALSE,
                      sep = "\t",
                      col.names = c("iid", "id", "timestamp"))) %>%
  mutate(timestamp = timestamp/1000)

abc1 <- ww %>% group_by(iid, id) %>% slice(1L) %>% ungroup

xx <- split(abc1, abc1$id)

prevdf <- function(dat1) {
    ts <- dat1$timestamp
    data.frame(timestamp = ts, id = dat1$id,
               len = sapply(prevf(ts, ts), length)) %>%
                   filter(len >= 2)
}

yy <- unname(lapply(xx, prevdf))
#yy
ll <- do.call("rbind", yy)

abc %<>% group_by(id)

abc %<>%
  do({d <- data.frame(timestamp = .$timestamp);
      d$len = sapply(prevf(.$timestamp, .$timestamp), length);
      d})

limit_reached <- abc %>%
  mutate(len = sapply(prev, length)) %>%
  filter(len >= 2)
