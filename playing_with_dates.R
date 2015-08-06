
# setup -------------------------------------------------------------------

library(lubridate)
library(dplyr)
library(magrittr)

# simple tests ------------------------------------------------------------

class(ymd("2015-03-10"))
seq(from = ymd("2015-03-10"), to = ymd("2015-03-15"), by = "day")

ymd("20151410")  # fails to parse; month is 14
ymd(20151210)    # works!
ymd_hm("2015-12-10 11:10")
(zz <- ymd_hm("2015-12-10 11:10", tz = "CEST"))
with_tz(zz, tz = "UTC")

(xx <- Sys.time() + 1:3)
as.vector(xx)
dput(xx)
(yy <- .Internal(Sys.time()))
dput(yy)
class(yy)
is.integer(yy)
is.double(yy)
dput(as.integer(yy))


str(lubridate::now())
x <- lubridate::now()
as.numeric(x)
as.POSIXlt(x)
str(as.POSIXlt(x))
unclass(as.POSIXlt(x))
unclass(x)
.Internal(inspect(x))
.Internal(inspect(unclass(x)))

year(x)
minute(x)
x
minute(x) <- 45
x
minute(x) <- 97
x

NUM <- 30
times <- ymd_hm("2015-08-01 10:00") + seconds(sample(seq(60*60*2), size = NUM))
events <- sample(c("A", "B"), size = NUM, replace = TRUE)
df1 <- tbl_df(data.frame(times, events)) %>% arrange(times)
df1 %>% filter(minute(times) < 30)  ## only the events in each first half hour
df1 %>% filter(hour(times) == 10)   ## only the events in the firs hour
df1 %<>% filter(hour(times) == 10) %>%
  arrange(events)


zz <- ymd_hm("2015-12-10 11:10", tz = "CEST")
with_tz(lubridate::now(), tz = "UTC")
with_tz(lubridate::now(), tz = "UTC")

with_tz(now(), tz="Etc/UTC") %within% interval(ymd_hm("2015-08-01 9:00"),ymd_hm("2015-08-01 13:00"))

i1 <- interval(ymd_hm("2015-08-01 9:00"),ymd_hm("2015-08-01 13:00"))
minutes(i1)
minutes(int_flip(i1))
