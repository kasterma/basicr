# Convert a Data Frame to a Numeric Matrix
data.matrix(data.frame(x = c(1,2,3), y = 4))
data.matrix(data.frame(x = c(1,2,3), y = c("a", "b", "c")))
data.matrix(data.frame(x = c(1,2,3), y = c("a", "b", "c"), stringsAsFactors = FALSE))

match(c(1,2,3), c(0,0,1,3,3))
match(c(0,1,2), c(0,1,2), incomparables = c(0,1), nomatch = -1)

# mapply use for zipping
mapply(c, c(11,22,33), c(44,55,66), SIMPLIFY = FALSE)
