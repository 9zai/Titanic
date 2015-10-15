setwd("/Users/9zai/Desktop/competition/Titanic/")
# the assignmet is not specify which variable should we plot out!!!!!
train <- read.csv("train.csv", header = T)
library(ggplot2)

p <- ggplot(train,aes(Age,Sex))
p + geom_boxplot()

