library(tidyr)
library(iterators)
library(foreach)

test_df1 <- data.frame(x = c(1,2,3,3), y = c(4,5,6,6), z = c(11,22,33,66))

tidyr::unite(test_df1, col = "jj", x, y)

tidyr::unite_(test_df1, col = "jj", from = c("x", "y"))

length(unique(with(test_df1, paste(x, y))))

tidyr::complete(data.frame(x=c(0,1), y = c(1,0)), x,y)

test_df2 <- tidyr::complete(data.frame(x = c(0,1), y = c(0,1), z = c(0,1)),
                  x, y ,z)

passes <- function(dat_in, ncol_check = 2, nrow_max = 3) {
  ncol_dat <- ncol(dat_in)
  to_check <- combn(seq(ncol_dat), ncol_check)
  cts <- foreach(idxs = iterators::iter(to_check, by = "col")) %do%
    length(unique(with(dat_in, do.call(paste, lapply(names(dat_in)[idxs], as.name)))))
  all(cts <= nrow_max)
}

check_num_rows <- function(dat_in, nrows_check = 4, ncol_check = 2, nrow_max = 3) {
  nrow_dat <- nrow(dat_in)
  to_check <- combn(seq(nrow_dat), nrows_check)
  res <- foreach(idxs = iterators::iter(to_check, by = "col")) %do%
    passes(dat_in[idxs,], ncol_check, nrow_max)
  any(as.logical(res))
}

test_f <- function(x) {
  e <- parent.frame()
  print(e)
}

test_g <- function() {
  test_f()
}
