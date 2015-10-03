#' Sessionize
#'
#' Making sessions is an important concept, but the base, the notion of session,
#' is fuzzy.  Think of looking up a subject on wikipedia, and then getting stuck
#' looking up all sorts of related or unrelated things.  Is this one session,
#' or one directed session followed by a random browsing session?  So first
#' question should be
#'
#'    What do you want to make sessions for?
#'
#' This should direct how you make sessions.
#'
#' Sessions can last through switching devices starting to browse on phone, then
#' continuing on computer.  Also can last over long pauses, think of leaving
#' a page open in a tab to return to intermittendly.
#'
#' Some problems are solved by having users log in.
#'
#' Working on making sessions

library(DBI)

items.db <- dbConnect(RSQLite::SQLite(), "items-db.sqlite")

dbListTables(items.db)

df1 <- data.frame(id = seq(100), val = runif(100))

dbWriteTable(items.db, "testdata", df1)

df1 <- df1[order(df1$val),]

dbWriteTable(items.db, "testdata", df1, overwrite = TRUE)

df1 <- within(df1, delay_from_previous <- c(NA, diff(df1$val)))

dbWriteTable(items.db, "testdata", df1, overwrite = TRUE)

plot(df1$val)

## looking at
hist(df1$delay_from_previous)
mean(df1$delay_from_previous, na.rm = TRUE)
## we think that 0.02 as an interval for breaking sessions should give
## us a nice set of sessions.

df1 <- within(df1, new_session <- delay_from_previous > 0.02)

plot(df1$val)
for(idx in which(df1$new_session)) {
    abline(v=idx)
}

xx <- which(c(df1$val,0) - c(0,df1$val) > 0.02)
plot(df1$val)
for(idx in xx) {
  abline(v=idx)
}



## plotting the idea of time based sessionizing

xs <- sort(runif(4, min = 0, max = 5))
xs_df <- data.frame(xs=xs)
xs_df <- within(xs_df, {nx <- c(xs[-1], 5); ny <- nx - xs})

library(ggplot2)
ggplot(xs_df) +
  geom_segment(aes(x=xs, y=0, xend=nx, yend=ny)) +
  geom_segment(aes(x=c(xs[-1],5),y=ny, xend=c(xs[-1],5), yend=0)) +
  geom_abline(intercept = 1, slope = 0, color = "red")
