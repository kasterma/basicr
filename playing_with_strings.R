# playing with strings


# setup -------------------------------------------------------------------

library(stringi)
library(stringr)


# stringi -----------------------------------------------------------------
# 
# http://docs.rexamine.com/R-man/stringi/stringi-package.html

stri_join(c("a", "b", "c"), c("x", "y"))
'ID_' %stri+% 1:5

stri_detect_fixed("hi ho hi", "hi")
stri_detect_charclass("HI HO HI", "\\p{Lu}")
stri_detect_coll

stri_extract("hi ho hi ha", regex = "h[^o]*h")
stri_extract_first_regex("hi ho hi ha", pattern = "h[^o]*h")

stri_sub("abcdef", 2, 4)

x <- "abcdef"
stri_sub(x, 2, 4) <- "xx"
x
stri_sub(x, 2, 4) <- "efghi"
x

stri_extract("this: that, more", regex = "this: (that), more")
stri_match_all("this: that, more this: something, more", regex = "this: ([^,]*), more")
stri_match_all("this: that, more this: something, more",
               regex = "this: ([^,]*), more")[[1]][,2]

stri_match_all(c("this: that, more this: something, more", "this", "thisthis"),
               regex = c("this: ([^,]*), more", "this"))


stri_length(c('123', "2345"))

stri_escape_unicode("a\u0105!")
stri_unescape_unicode(stri_escape_unicode("a\u0105!"))

# stringr -----------------------------------------------------------------



str_c(c("a", "b", "c"), c("w", "x", "y", "z"))
str_c(c("a", "b", "c"), c("w", "x", "y", "z"), collapse = ":")

# the next lines no longer make sense.  In a previous version of the stringr
# library they were inspired by understanding the source.
str_c
Filter(function(x) length(x) > 0, list(c(1), c(), c("1","2")))
?is.atomic
is.atomic(c())
is.atomic(list(list()))
is.atomic(list())

paste(c("a", "b", "c"), c("w", "x", "y", "z"))
paste(c("a", "b", "c"), c("w", "x", "y", "z"), collapse = ":")
paste(c("a", "b", "c"), c("w", "x", "y", "z"), collapse = ":", sep="#")

paste(list(list(x=3)), c("x", "y"))
str_c(list(list(x=3)), c("x", "y"))

?paste0
# Noteworthy in this man page is that paste0 is claimed to be sligthly more 
# efficient.

