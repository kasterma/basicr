# somewhere in the transformations performed by sapply we simplify the
# POSIXct objects to timestamps.  Make clear where this happens.

library(stringi)
library(pryr)

millis_to_date <- function(tsmillis) {
  as.POSIXct(tsmillis/1000, origin = "1970-01-01")
}

dat <- data.frame(dataunit = c("timestamp:1429494726643", "timestamp:1429491123398"))

lapply(stri_match_all_regex(dat$dataunit,
                            "timestamp:([^,]*)"),
       f(x, millis_to_date(as.numeric(x[2]))))

sapply(stri_match_all_regex(dat$dataunit,
                            "timestamp:([^,]*)"),
       f(x, millis_to_date(as.numeric(x[2]))))

millis_to_date(sapply(stri_match_all_regex(dat$dataunit,
                                           "timestamp:([^,]*)"),
                      f(x, as.numeric(x[2]))))
