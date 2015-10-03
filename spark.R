#from: http://opiateforthemass.es/articles/six-lines-to-install-and-start-sparkr-on-mac-os-x-yosemite/

spark_path <- strsplit(system("brew info apache-spark",intern=T)[6],' ')[[1]][1] # Get your spark path
.libPaths(c(file.path(spark_path,"libexec", "R", "lib"), .libPaths())) # Navigate to SparkR folder
library(SparkR) # Load the library

sc <- sparkR.init()
sqlContext <- sparkRSQL.init(sc)
df <- createDataFrame(sqlContext, iris)
head(df)
