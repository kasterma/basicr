
# setup -------------------------------------------------------------------

library(lubridate)

# simple tests ------------------------------------------------------------

class(ymd("2015-03-10"))
seq(from = ymd("2015-03-10"), to = ymd("2015-03-15"), by = "day")

(xx <- Sys.time() + 1:3)
as.vector(xx)
dput(xx)
(yy <- .Internal(Sys.time()))
dput(yy)
class(yy)
is.integer(yy)
is.double(yy)
dput(as.integer(yy))