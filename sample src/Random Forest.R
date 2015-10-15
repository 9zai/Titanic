setwd("/Users/9zai/Desktop/competition/Titanic/")
set.seed(5)

library(randomForest)
train <- read.csv("train.csv",header = TRUE, stringsAsFactors = F)
test <- read.csv("test.csv",header = TRUE,stringsAsFactors = F)
#clean the variables first
extraFeatures <- function(data){
  features <- c("Pclass","Sex","Age","Fare","SibSp","Parch","Embarked")
  
  val <- data[,features]
  val$Age[is.na(val$Age)] = mean(val$Age,na.rm =T)
  val$Fare[is.na(val$Fare)] = median(val$Fare,na.rm = T)
  val$Embarked[val$Embarked=="" ] = "S"
  val$Sex <- as.factor(val$Sex)
  val$Embarked <- as.factor(val$Embarked)
  return(val)
}

my_forest <- randomForest(extraFeatures(train),as.factor(train$Survived),ntree = 100,importance = T)

my_sol <- data.frame(PassengerId = test$PassengerId)
my_sol$Survived <- predict(my_forest,extraFeatures(test))

write.csv(my_sol,file = "my_sol.csv", row.names = FALSE)
#The as.factor function change the survived into factor
#Make sure to adopt the exact same order of variables

# my_forest <- randomForest(as.factor(X) ~ Y + Z , data=train, importance=TRUE, ntree=10)
# ntree = 1000??? why?
#When running the function, two graphs appear: 
#the accuracy plot shows how much worse the model would perform without the included variables. So a high decrease (= high value x-axis) links to a high predictive variable. 
#The second plot is the Gini coefficient. The higher the variable scores here, the more important it is for the model.