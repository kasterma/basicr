library(foreach)

foreach(i = seq(from = 1, to = 4)) %do% sqrt(i)
foreach(i = 1:4, j = 3:4) %do% (i + j)
foreach(i = 1:4, j = 3:4) %do% {i + j}
foreach(i = 1:4, .combine = 'c') %do% sqrt(i)

library(pryr)

foreach(i = 1:4, j = 4:7, .combine = rbind) %do%
    data.frame(x = sqrt(i), y = sqrt(j))

res1 <- foreach(i = 1:4, j = 4:7) %do%
    data.frame(x = sqrt(i), y = sqrt(j))
do.call('rbind', res1)

ff <- function(x,y) {
    print(paste("x", x))
    print(paste("y", y))
    rbind(x,y)
    do.call('rbind', as.list(c(x,y)))
}
foreach(i = 1:4, .combine = ff) %do% sqrt(i)

gg <- function(...) {
    print(match.call())
    print(c(...))
    do.call('rbind', as.list(c(...)))
}
foreach(i = 1:4, .combine = gg) %do% sqrt(i)
foreach(i = 1:4, .combine = gg, .multicombine = TRUE) %do% sqrt(i)
foreach(i = 1:7, .combine = gg, .multicombine = TRUE, .maxcombine = 3) %do% sqrt(i)

library(doParallel)
registerDoParallel(cores = 4)
foreach(i = 4:1, .combine = 'c') %dopar% { Sys.sleep(3 * i); i }
foreach(i = 4:1, .combine = 'c', .inorder = FALSE) %dopar% { Sys.sleep(runif(1)); i }

## not succeeding to get anything to appear out of order yet
foreach(i = 4000:1, .combine = 'c', .multicombine = FALSE, .inorder = FALSE) %dopar% {  i }

# library(iterators)  already included, but this is where iter comes from
foreach(r = iter(data.frame(x=c(1,2,3),y = c(4,5,6)), by="row"),
        y = c(4,5,6)) %do% {list(r,y)}


sim <- f(a, b, 4 * a + b)
foreach(b = c(1,2,3,4), .combine = 'cbind') %do% {
    foreach(a = c(1,2,3,4), .combine = 'c') %do% {
        sim(a,b)
    }
}

foreach(b = c(1,2,3,4), .combine = 'cbind') %:%
    foreach(a = c(1,2,3,4), .combine = 'c') %do% {
        sim(a,b)
    }

foreach(a = c(1,2,3,4), .combine = 'c')

reps <- foreach(b = c(1,2,3), .combine = 'cbind') %:%
    foreach(a = c(1,2,3), .combine = 'c') %:%
        when(a >= 2)

reps %do% sim(a,b)
reps %do% (a*b)
reps %dopar% (a + 5*b)
