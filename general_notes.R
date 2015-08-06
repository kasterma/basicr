library(dplyr)

?paste
example(paste)

cut(c(1,2,3), breaks = c(0,2.5,6), labels = c("low", "high"))

# always use in communicating about R so that others know what I am looking at
sessionInfo()

# looking up functions
getAnywhere(`%s+%`)
stringi::`%s+%`

# for the list.files function, using argument full.names = TRUE makes that
# the path argument is also output; i.e. in using files below we do not
# need to paste0("data/", filename) since the data/ is already there
files <- list.files(pattern = "arabweeks-week-.*\\.csv", "data/", full.names = TRUE)

structure(c(128675575, 91892653), .Names = c("day1", "day2")) %>%
  format(big.mark = ",") %>%
  print(quote = FALSE)

xx <- rep(1, 1000)
format(object.size(xx), units = "auto")
format(structure(100000000, class = "object_size"), unit = "MB")

zz <- textConnection("foo", "w")
writeLines(c("testit11", "testit21"), zz)
close(zz)
foo

?as.numeric   ## issues with numeric VS double

.1 == .3/3
all.equal(.1, .3/3)

methods(print)
methods(class="lm")

conflicts(detail = TRUE)

find("c")

