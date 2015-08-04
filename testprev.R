# not too many per time

library(dplyr)
library(pryr)
library(magrittr)

df1 <- data.frame(id=1, time = c(1,2,5), val = runif(3))
df2 <- data.frame(id=2, time = c(2,1,3), val = runif(3))
df_test <- rbind(df1, df2)

sumlast <- function(x,y) ifelse(is.na(x), 1, ifelse(is.na(y), 1 + x, 1 + x + y))

df_test %>% group_by(id) %>% arrange(time) %>%
  mutate(last = time - lag(time),
         llast = time - lag(time, 2),
         ct = sumlast(last <= 2, llast <= 2))

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
  group_by(id) %>%
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
