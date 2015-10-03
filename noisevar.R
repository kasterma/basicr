library(foreach)

id <- 1:100
x <- rnorm(length(id))
y <- rnorm(length(id))

df.test <- data.frame(id, x ,y)

testrun <- function(dat, i) {
    fit <- lm(y ~ x, dat[1:(i-1),])
    dat$y[i] - predict(fit, data.frame(x=dat$x[i]))
}

res <- foreach(i = 2:length(id), .combine = c) %do% testrun(df.test, i)

y2 <- 0.2*x + 1* rnorm(length(id))
df.test2 <- data.frame(id, x, y = y2)

res2 <- foreach(i = 2:length(id), .combine = c) %do% testrun(df.test2, i)

cor(x,y)
cor(x,y2)

df.test3 <- df.test
rand.size <- 100
df.test3$y[1:rand.size] <- sample(df.test3$y[1:rand.size])
fit <- lm(y ~ x, df.test3)
deviance(fit)



y3 <- as.factor(y2 > 0)

df.test3 <- data.frame(id, x, y = y3)

fit <- glm(y ~ x, df.test3, family = "binomial")
