library(data.table)
library(ggplot2)

dt1 <- data.table(id = seq(20), x = rnorm(20))[,y := 3 * x]

nr <- 30
dt2 <- data.table(id = seq(nr), z = sample(c(1,2), size = nr, replace = TRUE))

dt1[order(x)]
qplot(x,y, data = dt1)
dt2[order(z, -id)]

setkey(dt2)
setkey(dt1, id)

dt1[dt2]  # needs dt1 have a key that is used for the join
dt1[data.table(id = c(1,2,33,34))]
dt1[c(1,2,33,44)]

dt3 <- data.table(id = seq(10), z = sample(c(1,2), size = 10, replace = TRUE), key = "id")

dt4 <- dt2[sample(c(TRUE, FALSE), size = 20, replace = TRUE)]

dt5 <- dt1[sample(c(TRUE, FALSE), size = 20, replace = TRUE)]

dt1[dt3]
dt3[dt1]

merge(dt1, dt2)
merge(dt1, dt3)
merge(dt3, dt1)

merge(dt5, dt4, all.x = TRUE)
merge(dt5, dt4, all.y = TRUE)


dt6 <- data.table(id2 = seq(20), a = rnorm(20), id = rep(1, 20), key = "id2")

dt1[dt6]

dtA <- data.table(x = c(1,1,1,2,2), y = c("a", "b", "c", "d", "e"), z = c(11,22,33,44,55))
dtB <- data.table(x = c(1,2), y = c("c", "c"))
dtA[dtB, on = .(x, z > 33)]
dtA[dtB, on = .(x), z := 66]
dtA

dtA <- data.table(x = c(1,1,1,2,2), y = c("a", "b", "c", "d", "e"), z = c(11,22,33,44,55))
dtA[dtB, on = .(x, y), z := 66]
dtA

dtA <- data.table(x = c(1,1,1,2,2), y = factor(c("a", "b", "c", "d", "e")), z = c(11,22,33,44,55))


dtA[,.(yy := factor(y))]
setkey(dtA, y)
