setwd("/Users/9zai/Desktop/competition/Titanic/")
trainData <- read.csv("train.csv",header = TRUE)
testData <- read.csv("test.csv",header = TRUE)

library(rpart)
library(randomForest)

trainData$family_size <- trainData$SibSp+trainData$Parch+1
testData$family_size <- testData$SibSp+testData$Parch+1

dicTree <- rpart(Survived ~ Pclass+Sex+Age+Fare+Embarked+family_size, 
                 data = trainData, method = "class")

plot(dicTree)
text(dicTree)

my_pre <- predict(dicTree,testData,type="class")
my_sol <- data.frame(PassengerId = testData$PassengerId, Survived = my_pre)

write.csv(my_sol,file = "my_sol.csv", row.names = FALSE)
