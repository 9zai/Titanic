getBestCutoff <- function(cl.name){
  meanErr <- c(1:100)
  bestCutoff <- 0
  minErr <- 1
  for(i in 1:100){
    cutoff <- (i-1)/100
    meanErr[i] <- k.fold.validation(train,cl.name,probcutoff = cutoff)
    if(meanErr[i]<minErr){
      minErr <- meanErr[i]
      bestCutoff <- cutoff
    }
  }
  plot(meanErr,type="b")
  return(bestCutoff)
}
getGlmCutoff <- function(){
  meanErr <- c(1:100)
  bestCutoff <- 0
  minErr <- 1
  cl.name <- "glm"
  for(i in 1:40){
    cutoff <- (i-20)/10
    meanErr[i] <- k.fold.validation(train,cl.name,probcutoff = cutoff)
    if(meanErr[i]<minErr){
      minErr <- meanErr[i]
      bestCutoff <- cutoff
    }
  }
  plot(meanErr,type="b")
  return(bestCutoff)
}
getResult <-function(prob,cutoff){
  result <- c(1:418)
  result[which(prob>cutoff)] <- 1
  result[which(prob<cutoff)] <-0
  return(result)
}
#main
#cutoff.dtree <- getBestCutoff("dtree")#0.17
#cutoff.glm <- getGlmCutoff()#-0.3
cutoff.randomF <- getBestCutoff("randomF")#0.52
#cutoff.ada <- getBestCutoff("ada")#0.45
#cutoff.svm <- getBestCutoff("svm")#0.25

#verison 1 glm -0.3 svm 0.25 randomF 0.52 ada 0.45 dtree 0.17
#version 2 glm -0.2 svm 0.49 

randFor <- randomForest(Survived~.,data = train,ntree=5000,importance = T)
result <- predict(randFor,newdata = test)

