getParSur <- function(youndata){
  lastNameS <- regexpr("\\. [A-Z ]{1,20}",youndata$Name,T)
  lastNameE <- lastNameS + attr(lastNameS,"match.length") -1
  lastName <- substr(youndata$Name,lastNameS+2,lastNameE)
  
  train.parent.vector <- regexpr(paste("\\.",lastName),raw.train$Name,fixed = T)
  test.parent.vector <- regexpr(paste("\\.",lastName),raw.train$Name,fixed = T)
  parent <- c(train.parent.vector,test.parent.vecotr)
  #return(ParSur)
}


#main
summary(as.factor(my_sol$Survived))
remain <- raw.test[which(my_sol$Survived == 99),]
len <- nrow(remain)
#View(remain)
#final assgin
for(i in 1:len){
  
}

write.csv(my_sol,file = "my_sol.csv", row.names = FALSE)
