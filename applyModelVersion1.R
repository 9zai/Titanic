# Run this script after run the cleanData
# Apply some pre process method to test 
# Create the added attributes to test
# --Add cv to the train to decide cutoff, pruned, and exam etc
# --Add some other learning algorithm on it
# current 0.79904 (glm - 0.6 only)
# basic svm only 0.78947
# the problems are how to know the miss classifiation value
# 1 -> 0 or 0 -> 1 The single glm is outperform than the combine model.
# if cutoff reduce by 0.1, around 200 "0" will become 99
library(rpart)
library(randomForest)
library(e1071)
library(class)
library(neuralnet)
library(ada)

getDtree <- function(tr,te,pruned = F){#removed
  dtree <- rpart(Survived~.,data = tr)
  result <- predict(dtree,newdata = te)
  if(pruned){
    pfit<- prune(dtree, cp=dtree$cptable[which.min(dtree$cptable[,"xerror"]),"CP"])
    result <- predict(pfit, newdata=te)
  }
#  result = result[,2]/rowSums(result)
  result = result[,2]
  result
}
getRandomForest <- function(tr,te){
  randomF <- randomForest(Survived~.,data = tr,ntree=5000,importance = T)
  model.randomF <<- randomF
  result <- predict(randomF,newdata = te,type="prob")
#  result = result[,"1"]/rowSums(result)
  result = result[,"1"]
  result
}
getGlm <-function(tr,te){
  glm <- glm(Survived~.,data = tr,family = binomial)
  model.glm <<- glm
  result <- predict(glm,newdata = te)
  result <- as.numeric(result)
  result[which(result<0)] <- 0
  result <- (result)/(1+result)
}
getSvm <-function(tr,te){
  svm <- svm(Survived~.,data = tr,probability = T)
  model.svm <<- svm
  result <- predict(svm,newdata = te,probability = T)
  result = attr(result,"prob")
#  result = result[,which(colnames(result)==1)]/rowSums(result)
  result = result[,which(colnames(result)==1)]
  result
}
getAda <- function(tr,te){
  ada <- ada(Survived~.,data = tr)
  model.ada <<- ada
  result <- predict(ada,newdata = te,type='probs')
#  result = result[,2]/rowSums(result)
  result = result[,2]
  result
}

do.classification<- function(train.data,test.data,cl.name){
  switch(cl.name,
         dtree={
           getDtree(train.data,test.data,F)
         },
         randomF={
           getRandomForest(train.data,test.data)
         },
         glm={
           getGlm(train.data,test.data)
         },
         svm={
           getSvm(train.data,test.data)
         },
         ada={
           getAda(train.data,test.data)
         })
}

k.fold.validation <- function(data,cl.name = 'svm',k.fold = 10,probcutoff = 0.5){
  #defualt use dtree,10-fold
  n.obs <- nrow(data)
  set.seed(123)
  s <- sample(n.obs)
  errors <- dim(k.fold)
  probs <- NULL
  actuals <- NULL
  for(i in 1:k.fold){
    test.idx <- which(s %% k.fold == (i-1))
    train.set <- data[-test.idx,]
    test.set <- data[test.idx,]
    cat(k.fold,'-fold CV run',i,cl.name,':',
        '#training:',nrow(train.set),
        '#testing',nrow(test.set),'\n')
    result.prob <- do.classification(train.set,test.set,cl.name)
    predicted <- as.numeric(result.prob > probcutoff)
    result.actual <- test.set$Survived
    confusion.matrix <- table(result.actual,factor(predicted,levels = c(0,1)))
    confusion.matrix
    
    error = (confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)  
    errors[i] = error
    cat('\t\terror=',error,'\n')
    probs = c(probs,result.prob)
    actuals = c(actuals,result.actual)
  }
  
  avg.error = mean(errors)
  cat(k.fold,'-fold CV results:','avg error=',avg.error,'\n')
  return(avg.error)
}

myClassifier <- function(do.cv =T,do.test =F){
  if(do.cv){
    try.cl <- 'randomF'
    k.fold.validation(train,try.cl,probcutoff= 0.52)
  }
  if(do.test){
    prob.glm <<- getGlm(train,test)
    prob.svm <<- getSvm(train,test)
#    result.neuro <<- getNeu(train,test)
    prob.ada <<- getAda(train,test)
    prob.randomF <<- getRandomForest(train,test)
  }
}
#main
prob.glm <- NULL
model.glm <- NULL
prob.svm <- NULL
model.svm <- NULL
prob.ada <- NULL
model.ada <- NULL
prob.randomF <- NULL
model.randomF <- NULL
myClassifier(do.cv =F,do.test=T)