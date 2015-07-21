# The basic learning methods

# Setup ------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(glmnet)
library(caret)
library(ggplot2)
library(GGally)

# Linear Models ----------------------------------------------------------------

df1 <- data.frame(x = c(1,2,3,1,2,3), y = c(2,1,1,2,1,1))
summary(df1)
glimpse(df1)

fit.lm <- lm(y ~ x, df1)
summary(fit.lm)
predict(fit.lm, data.frame(x=c(1,2,3,4)))

plot(df1)
abline(fit.lm)

ggplot(df1 %>% mutate(y.pred = predict(fit.lm, data.frame(x = x))),
       aes(x = x, y = y)) + geom_point() + geom_line(aes(y = y.pred))

ggpairs(df1) # wierd output b/c smoothing

# logistic regression -----------------------------------------------------

df2 <- data.frame(x = c(1,2,3,1,2,3), y = c(0,0,1,1,0,1))

glm(y ~ x, data = df2, family = "binomial")

# regularization ----------------------------------------------------------


# splines GAMs ------------------------------------------------------------


# trees -------------------------------------------------------------------

library(tree)
library(ISLR)
data(Carseats)
Carseats <- tbl_df(Carseats) %>% mutate(High = as.factor(ifelse(Sales <= 8,"No"," Yes")))
tree.carseats <- tree(High ~ . - Sales, Carseats)
summary(tree.carseats)
tree.carseats
plot(tree.carseats)
text(tree.carseats, pretty = 0)
predict(tree.carseats, Carseats[1,])
predict(tree.carseats, Carseats[2,])




# SVM ---------------------------------------------------------------------


# PCA ---------------------------------------------------------------------


# k-means -----------------------------------------------------------------


