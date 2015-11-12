# glm,dtree,svm three simplest method
# ensemble to a classifier
library(rpart)
library(plyr)
library(ada)
library(e1071)
# glm first
rbin.rownames <- function(list1,list2){#assum the names are sorted
  n <- length(list1) + length(list2)
  size.list1 <- length(list1)
  size.list2 <- length(list2)
  list <- c(1:n)
  j <-1
  k <- 1
  name1 <- as.numeric(names(list1))
  name2 <- as.numeric(names(list2))
  for(i in c(1:n)){
      if(name1[j]<name2[k]){
        list[i] = list1[j]
        if(j<size.list1){
          j =j+1
        }
      }else{
        list[i] = list2[k]
        if(k<size.list2){
          k = k+1
        }
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
           #getRandomForest(train.set,test.set)
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
  svm <- svm(Survived~.,data = train.set,probability = T,kernel="polynomial")
  result <- predict(svm,newdata = test.set,probability = T)
  result = attr(result,"prob")
  result = result[,which(colnames(result)==1)]
  return(result)
}
getEnsemble <- function(train.set,test.set){
  dtree <- getSvm(train.set,test.set)
  onBoat.glm <- getGlm(train.set[which(2==as.numeric(train.set$Boat)),],
                       test.set[which(2==as.numeric(test.set$Boat)),])
  outBoat.glm <- getGlm(train.set[which(1==as.numeric(train.set$Boat)),],
                        test.set[which(1==as.numeric(test.set$Boat)),])
  glm <- rbin.rownames(onBoat.glm,outBoat.glm)  
  result <- 0.3*dtree + 0.7*glm
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

k.fold.validation <- function(data,cl.name = 'svm',k.fold = 10,probcutoff = 0.5){
  #defualt use svm,10-fold
  n.obs <- nrow(data)
  set.seed(123)
  s <- sample(n.obs)
  errors <- c(NULL)
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
    err <- test.set[which(result.actual!=predicted),]
    errors <- c(errors,err)
  }
  
  return(errors)
}


test.class <- function(t.result,upbound = 0.8,lowbound = 0.2){
  t.result[which(t.result<lowbound)] <- 0
  t.result[which(t.result>upbound)] <- 1
  return(t.result)
}
get.undefined.inst <- function(result){
  n <- length(result)
  undefined <- vector()
  for(i in c(1:n)){
    if(result[i]>0.01 && result[i]<0.99){
      undefined <- c(undefined,result[i])
    }
  }
  return(undefined)
}
iter.addNewTrain <-function(result){
  name <- as.numeric(names(result))
  n <- length(result)
  for(i in c(1:n)){
    if((0.01>result[i]) || (0.99<result[i])){
      raw.full[name[i],"Survived"] <<- result[i]
    }
  }
}
sumResult <- function(first,second,r.svm){
  n <- length(first)
  sumResult <- rep(-1,n)
  for(i in c(1:n)){
    if(second[i]<0.01){
      sumResult[i] <- 0
      next
    }
    if(first[i]>0.5&&second[i]>0.5){
      sumResult[i] <- 1
      next
    }
#     if(r.svm[i]+0.2 > second[i] && r.svm[i]<0.2){
#       sumResult[i] <- 0
#       next
#     }
#     if(first[i]>0.5&&second[i]>first[i]){
#       sumResult[i]<-1
#       next
#     }
#     if(first[i]<0.5&&second[i]<first[i]&&r.svm[i]<0.6){
#       sumResult[i]<-0
#       next
#     }
#     if(r.svm[i]>0.9){
#       sumResult[i] <-1
#       next
#     }
#     if(r.svm[i]<0.1){
#       sumResult[i] <- 0
#       next
#     }
#     if(second[i]<0.5&&second[i]<first[i]-0.15){
#       sumResult[i] <- 0
#       next
#     }
#     if(first[i]<0.4&&second[i]<0.4&&r.svm[i]<0.5){
#       sumResult[i]<-0
#       next
#     }
#     if(first[i]>0.5&&second[i]>0.5){
#       sumResult[i] <- 1
#       next
#     }
     sumResult[i] <- 0
  }
  return(sumResult)
}
collect.result <- function(sol,sum.result){
  n <- length(sol)
  j <- 1
  for(i in c(1:n)){
    if(sol[i]>0.01&&sol[i]<0.99){
      sol[i] <- sum.result[j]
      j<- j+1
    }
  }
  return(sol)
}
#main
#leave.one.out(train,"svm",cutoff=0.5)
err <- k.fold.validation(train,"Ensemble")
# t.result <- getEnsemble(train,test)
# fir.result <- test.class(t.result,upbound=0.9,lowbound=0.1)
# fir.undefined <- get.undefined.inst(fir.result)
# iter.addNewTrain(fir.result)
# sec.train <- raw.full[which(!is.na(raw.full$Survived)),train.attrName]
# sec.test <- raw.full[which(is.na(raw.full$Survived)),test.attrName]
# 
# t.sec.result <- getEnsemble(sec.train,sec.test)
# 
# sec.result <- test.class(t.sec.result)
# 
# t.svm <- getDtree(sec.train,sec.test)
# t <- cbind(fir.undefined,sec.result,t.svm)
# three.sum <- sumResult(fir.undefined,sec.result,t.svm)
# my_sol <- fir.result
# my_sol <- collect.result(my_sol,three.sum)
# 
# soluction <- data.frame(PassengerId = test$PassengerId)
# soluction$Survived <- my_sol
# write.csv(soluction,file = "my_sol.csv", row.names = FALSE)

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


