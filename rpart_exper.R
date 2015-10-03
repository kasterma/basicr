# rpart mentioned as library in Practical Data Science

library(rpart)

df1 <- data.frame(x = c(1,1,1,1,1,1,2,2,2), y = c(1,1,1,1,1,1,2,2,1), z = 1:9)

ll <- glm(y ~ x , df1, family = "gaussian")

with(df1, plot(x, y))
abline(ll)

rr1 <- rpart(y ~ x, data = df1, control = rpart.control(minsplit=3))

plot(rr1, branch = 0, uniform = TRUE, margin = 0.2)
text(rr1, use.n = TRUE, all = TRUE, fancy = TRUE)
