library(data.table)

dt1 <- data.table(id = seq(20), x = rnorm(20))[,y := 3 * x]
dt2 <- data.table(id = seq(20), z = sample(c(1,2), size = 20, replace = TRUE))

setkey(dt2)
setkey(dt1, id)

dt1[dt2]  # needs dt1 have a key that is used for the join

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
