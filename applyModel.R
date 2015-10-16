# Run this script after run the cleanData
# Apply some pre process method to test 
# Create the added attributes to test
# --Add cv to the train to decide cutoff, pruned, and exam etc
# --Add some other learning algorithm on it
#current 0.79904 (glm - 0.6 only)
#basic svm only 0.78947
raw.test <- read.csv("test.csv",header =T)

sumUpSurvived <- function(data,r1,r2,r3,r4){
  for(i in c(1:418)){
    count <- 0
    if(r1[i] ==1){
      count <-count+1
    }
    if(r2[i] ==1){
      count <-count+1
    }
    if(r3[i] ==1){
      count <-count+1
    }
    if(r4[i] == 1){
      count <-count+1
    }
    if(count>=2){
      data[i,"Survived"] <-1
    }else{
      data[i,"Survived"] <-0
    }
  }
  return(data$Survived)
}

probcutoff <- 0.6
raw.test$Pclass <- as.factor(raw.test$Pclass)
raw.test$Title <- getTitle(raw.test)
raw.test$Family <- getFamily(raw.test)
raw.test$Fare <- getFare(raw.test)
cTitleAgeRelation(raw.test)
raw.test[raw.test$Title=="Ms","Title"]<- "Mr"# two special Title Dona - Don/ Ms - Mr
raw.test[raw.test$Title=="Dona","Title"]<- "Don"
test.missTitle <- c("Master","Miss","Mr","Mrs")
raw.test$Age <- replaceByMedian(raw.test$Age,raw.test$Title,test.missTitle)
 
raw.test$Boat <- getBoat(raw.test)
raw.test$Title <- raw.test$Title
test.attrName <- c("PassengerId","Age","Pclass","Sex","Fare","Embarked","Family","Boat","Title")
test <- raw.test[,test.attrName]

library(rpart)
library(randomForest)
library(e1071)
dtree <- rpart(Survived~.,data = train)
ranforest <- randomForest(Survived~.,data = train[,-9],ntree=300,importance = F)
glm <- glm(Survived~.,data = train,family = binomial)
svm <- svm(Survived~.,data = train[,-9],probability = T)
#nb <- naiveBayes(Survived~.,data = train)

result.dtree <- as.numeric(predict(dtree,test,type="class"))-1#set to 0 & 1
result.randomFor <- as.numeric(predict(ranforest,newdata = test[,-9]))-1#set to 0 & 1
result.glm <- as.numeric(predict(glm,newdata = test,type="response") > probcutoff)
result.svm <- as.numeric(predict(svm,newdata = test[,-9],probability= T))-1 #set to 0 & 1
#result.nb <- predict(nb,newdata=test,type ="raw")

my_sol <- data.frame(PassengerId = test$PassengerId)
my_sol$Survived <- sumUpSurvived(my_sol,result.dtree,result.randomFor,result.glm,result.svm)
#my_sol$Survived <- result.svm
write.csv(my_sol,file = "my_sol.csv", row.names = FALSE)

