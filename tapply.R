xx <- 
structure(list(Year = c(1971L, 1971L, 1971L, 1971L, 1971L, 1971L,
                        1971L, 1971L, 1971L, 1971L, 1971L, 1971L, 1971L, 1971L, 1971L,
                        1971L, 1971L, 1971L, 1971L, 1971L, 1971L, 1971L, 1971L, 1971L,
                        1971L, 1971L, 1971L, 1971L, 1971L, 1971L, 1971L, 1971L, 1971L,
                        1971L, 1971L, 1971L, 1971L, 1971L, 1971L, 1971L, 1971L, 1971L,
                        1971L, 1971L), Month = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                                 2L, 2L, 2L), Rain = c(58.9, 74.6, 17.7, 7.8, 1.2, 1, 5.3, 0.7,
                                                                       1.2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                       0, 10.4, 17.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), .Names = c("Year",
                                                                                                                                          "Month", "Rain"), class = "data.frame", row.names = c(NA, -44L
                                                                                                                                          ))

with(xx, tapply(Rain, list(Month, Year), function(x) sum(x > .1)))

library(dplyr)

xx %>% filter(Rain > .01) %>% group_by(Year, Month) %>% summarise(length(Year))
xx %>% group_by(Year, Month) %>% summarise(count = sum(Rain > 0.1))
