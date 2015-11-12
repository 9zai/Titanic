# glm,dtree,svm three simplest method
# ensemble to a classifier
library(rpart)
library(plyr)
library(ada)
library(e1071)
# glm first
rbin.rownames <- function(list1,list2){
  n <- length(list1) + length(list2)
  list <- c(1:n)
  j <-1
  k <- 1
  l1 <- data.frame(list1)
  l2 <- data.frame(list2)
  name1 <- as.numeric(rownames(l1))
  name2 <- as.numeric(rownames(l2))
  for(i in c(1:n)){
    if(name1[j]==(891+i)){
      list[i] = l1[j,1]
      j <- j+1
    }else{
      list[i] <- l2[k,1]
      k = k+1
    }
  }
  return(list)
}
do.classification<- function(train.set,test.set,cl.name){
  switch(cl.name,
         dtree={
           getDtree(train.set,test.set)
         },
         randomF={
           #           getRandomForest(train.set,test.set)
         },
         glm={
           getGlm(train.set,test.set)
         },
         svm={
           getSvm(train.set,test.set)
         },
         ada={
           getAda(train.set,test.set)
         },
         ensemble={
           getEnsemble(train.set,test.set)
         })
}
getGlm <- function(train.set,test.set){
  glm <- glm(Survived~Age+Pclass+Fare+Family,data=train.set,family=binomial)
  result <- predict(glm,newdata = test.set,type="response")
  return(result)
}
getDtree <- function(train.set,test.set){
  dtree <- rpart(Survived~as.factor(Pclass)+Sex+Title+Pos+Embarked+Boat,data=train.set)
  result <- predict(dtree,newdata=test.set,type="prob")
  result <- result[,2]
  return(result)
}
getAda <- function(train.set,test.set){
  ada <- ada(Survived~.,data=train.set)
  result <- predict(ada,newdata=test.set,type="probs")
  result <- result[,2]
  return(result)
}
getSvm <- function(train.set,test.set){
  svm <- svm(Survived~.,data = train.set,probability = T)
  result <- predict(svm,newdata = test.set,probability = T)
  result = attr(result,"prob")
  result = result[,which(colnames(result)==1)]
  return(result)
}
getEnsemble <- function(train.set,test.set){
  dtree <- getDtree(train.set,test.set)
  onBoat.glm <- getGlm(train.set[which(2==as.numeric(train.set$Boat)),],
                       test.set[which(2==as.numeric(test.set$Boat)),])
  outBoat.glm <- getGlm(train.set[which(1==as.numeric(train.set$Boat)),],
                        test.set[which(1==as.numeric(test.set$Boat)),])
  glm <- rbin.rownames(onBoat.glm,outBoat.glm)  
  result <- 0.5*dtree + 0.5*glm
  return(result)
}

leave.one.out <- function(train.set,cl.name,cutoff = 0.5){
  n <- nrow(train.set)
  err <- 0
  for(i in c(1:n)){
    vi.train <- train.set[-i,]
    vi.test <- train.set[i,]
    
    pre <- do.classification(vi.train,vi.test,cl.name)
    pre <- as.numeric(pre > cutoff)
    act <- as.numeric(vi.test$Survived) -1
    if(pre != act){
      err = err+1
    }
  }
  err <- err/n
  cat(cl.name,'error: ',err,'\n')
}
test.class <- function(t.result,upbound = 0.8,lowbound = 0.2){
  t.result[which(t.result<lowbound)] <- 0
  t.result[which(t.result>upbound)] <- 1
  return(t.result)
}
iter.addNewTrain <-function(result){
  name <- as.numeric(names(result))
  n <- length(result)
  for(i in c(1:n)){
    if((0.1>result[i]) || (0.9<result[i])){
      raw.full[name[i],"Survived"] <<- result[i]
    }
  }
}
#main
#leave.one.out(train,"ada",cutoff=0.5)

t.result <- getEnsemble(train,test)
fir.result <- test.class(t.result)
iter.addNewTrain(fir.result)
sec.train <- raw.full[which(!is.na(raw.full$Survived)),train.attrName]
sec.test <- raw.full[which(is.na(raw.full$Survived)),test.attrName]

t.sec.result <- getEnsemble(sec.train,sec.test)
sec.result <- test.class(t.sec.result)
# newt.result <- getAda(sec.train,sec.test)
# names(newt.result) <- rownames(sec.test)
# newt.result <- as.numeric(newt.result)-1

#tem get
# re.sum <- t.result
# j<-1
# for(i in c(1:418)){
#   if(t.result[i]>0.1&&t.result[i]<0.9){
#     re.sum[i] <- newt.result[j]
#     j <- j+1
#   }
# }

