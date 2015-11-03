increaseDis <- function(data){
  data$glm <- data$glm
  data$svm <- (data$svm+0.75)^2# add 1-cutoff to increase distance
  data$ada <- (data$ada+0.55)^2
  data$randomF <- (data$randomF+0.48)^2
  return(data)
}
trainEnssamble <- function(te){
  result <- predict( model.glm,newdata = te)
  as.numeric(result)
  re.train.newFea$glm <<- as.numeric(result)
  
  result <- predict( model.svm,newdata = te,probability = T)
  result <- attr(result,"prob")
  result <- result[,which(colnames(result)==1)]
  re.train.newFea$svm <<- as.numeric(result)
  
  result <- predict( model.randomF,newdata = te,type="prob")
  result = result[,"1"]
  re.train.newFea$randomF <<- as.numeric(result)
  
  result <- predict( model.ada,newdata = te,type='probs')
  result = result[,2]
  re.train.newFea$ada <<- as.numeric(result)
  
  re.train.newFea <- increaseDis(re.train.newFea)
  enssamble <- naiveBayes(Survived~.,data = data.frame(re.train.newFea))
  return(enssamble)
}
sumUpSurvived <- function(data,sumup.input){
  model <- trainEnssamble(re.train[,re.attrName])
  result <- predict(model,newdata = sumup.input,type="class")
  data$Survived <- result
  return(data$Survived)
}

getResult <-function(prob,cutoff){ #trim result with higher accuracy
  result <- rep.int(99,418)
  result[which(prob>(cutoff))] <- 1
  result[which(prob<(cutoff))] <- 0
  return(result)
}

#main
result.glm <- getResult(prob.glm,-0.3)
result.svm <- getResult(prob.svm,0.45)
result.ada <- getResult(prob.ada,0.45)
result.randomF <- getResult(prob.randomF,0.52)
re.train.newFea <- NULL
re.train.newFea$Survived <- re.train$Survived

test2 <- NULL
test2$glm <- as.numeric(prob.glm)
test2$svm <- as.numeric(prob.svm)
test2$ada <- as.numeric(prob.ada)
test2$randomF <- as.numeric(prob.randomF)
test2 <- increaseDis(test2)
re.attrName <- c("Age","Pclass","Sex","Fare","Title","Pos",
"Mother","Child","Embarked","Family","Boat")

my_sol <- data.frame(PassengerId = raw.test$PassengerId)
result.sum <- sumUpSurvived(my_sol,test2)
c.all <- NULL
c.all$sum <- result.sum
c.all$glm <- result.glm
c.all$svm <- result.svm
c.all$ada <- result.ada
c.all$randomF <- result.randomF
my_sol$Survived <- result.sum
write.csv(my_sol,file = "my_sol.csv", row.names = FALSE)
#

