dtree <- rpart(Survived~.,data = train)
prob.result <- predict(dtree,newdata = test)
prob.result = prob.result[,2]/rowSums(prob.result)

pre.class.re <- getResult(prob_result,0.17)
class.result <- predict(dtree,newdata = test,type="class")

randFor <- randomForest(Survived~.,data = train,ntree=5000,importance = T)
prob.ran.result <- predict(randFor,newdata = test,type="prob")
class.ran.result <- predict(randFor,newdata= test)

prob.ran.result = prob.ran.result[,"1"]/rowSums(prob.ran.result)
prob.class <- getResult(prob.ran.result,0.52)