
# setup -------------------------------------------------------------------

library(lubridate)

# simple tests ------------------------------------------------------------

class(ymd("2015-03-10"))
seq(from = ymd("2015-03-10"), to = ymd("2015-03-15"), by = "day")