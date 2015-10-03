m <- 100
mean_samples <- function(n=10){
  X <- matrix(rnorm(n*m), nrow = m, ncol = n)
  return(apply(X,1,mean))
}

B <- matrix(0.0,100,20)
for (i in 1:20) {
  B[,i] <- mean_samples(i*10)
}
colnames(B) <- as.character(seq(10,200,by = 10))
boxplot(B)
