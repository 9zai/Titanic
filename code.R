setwd("/Users/9zai/Desktop/competition/Titanic/")
trainData <- read.csv("train.csv",header = TRUE)
testData <- read.csv("test.csv",header = TRUE)
plot(density(trainData$Age,na.rm = TRUE))
plot(density(testData$Fare,na.rm = TRUE))

counts <- table(trainData$Survived,trainData$Sex)
barplot(counts,xlab="Gender",ylab="Number of people")

counts[2]/(counts[1]+counts[2])
counts[4]/(counts[3]+counts[4])

Pclass_sur <- table(trainData$Survived,trainData$Pclass)
barplot(Pclass_sur,xlab="Bin class",ylab="number of people")

Pclass_sur[2]/(Pclass_sur[1]+Pclass_sur[2])
Pclass_sur[4]/(Pclass_sur[3]+Pclass_sur[4])
Pclass_sur[6]/(Pclass_sur[5]+Pclass_sur[6])

