## http://appliedpredictivemodeling.com/blog/2015/7/28/feature-engineering-versus-feature-extraction
## data from:
## http://www.biomedcentral.com/1471-2105/8/340

library(data.table)
library(ggplot2)

dat1 <- fread("~/Downloads/1471-2105-8-340-s1.csv")
dat_blog <- dat1[,.(Class, A=MorphologyV2Cell.EqSphereAreaCh1, B=MorphologyV2Cell.PerimCh1),]
str(dat_blog)

ggplot(dat_blog, aes(x = A, y = B, color = Class)) + geom_point()

cor(dat_blog[,list(A, B)])
