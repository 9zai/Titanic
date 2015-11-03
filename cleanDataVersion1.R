#clean the data,fill the missing value
setwd("/Users/9zai/Desktop/competition/Titanic/data")
library(Hmisc)

getTitle <- function(data){
  title.dot <- regexpr("\\,[A-Z ]{1,20}\\.",data$Name,T)
  title.comm <- title.dot + attr(title.dot,"match.length") -1
  data$Title <- substr(data$Name,title.dot+2,title.comm -1)
  return(data$Title)
}
getFamily <- function(data){
  data$Family <- data$SibSp + data$Parch
  return(data$Family)
}
getBoat <- function(data){
  data$Boat <- 0
  data[data$Sex == 'female','Boat'] <- 1
  data[data$Age <15,'Boat'] <-1
  return(as.factor(data$Boat))
}
getFare <- function(data){
  data$Fare[data$Fare == 0] <- NA
  data$Fare <- replaceByMedian(data$Fare,data$Pclass,levels(data$Pclass))
  return(data$Fare)
}
cTitleAgeRelation <- function(data){
  options(digits=2)
  data$Title <- as.factor(getTitle(data))
  bystats(data$Age,data$Title,
          fun = function(x)c(Mean=mean(x),Median=median(x)) )
}
replaceByMedian <- function(Missing,Filter,missCategory){
  for( level in missCategory){
    Missing[which(Filter == level )] <- impute(Missing[which(Filter == level )])
    #the impute function fill the NA automatically
  }
  return(Missing)
}

#main
raw.train <- read.csv("train.csv",header =T)

raw.train$Survived <- as.factor(raw.train$Survived)
raw.train$Pclass <- as.factor(raw.train$Pclass)
summary(raw.train)# show a summary

raw.train[raw.train$Embarked=="","Embarked"] <- "S" 
raw.train$Embarked <- factor(raw.train$Embarked)
#boxplot of ages by passenger traveling--help filling the missing in age
#boxplot for numeric mosaicplot for factoric
#boxplot(Age~Pclass,data = raw.train)
#mosaicplot(Survived~Pclass,data = raw.train)

# replace age by title
raw.train$Title <- getTitle(raw.train)
cTitleAgeRelation(raw.train)
miss.title <- c("Dr","Master","Miss","Miss","Mr","Mrs")
raw.train$Age <- replaceByMedian(raw.train$Age,raw.train$Title,miss.title)
# replace fare by Pclass
raw.train$Fare <- getFare(raw.train)

raw.train$Family <- getFamily(raw.train)
raw.train$Boat <- getBoat(raw.train)
raw.train$Title <- raw.train$Title
attrName <- c("Survived","Age","Pclass","Sex","Fare","Embarked","Family","Boat")
#Title is hard to hindle and not really meaningful
train <- raw.train[101:891,attrName]
re.train <- raw.train[1:100,attrName]
#------------- clean the test data -------------------------
raw.test <- read.csv("test.csv",header =T)

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
test.attrName <- c("Age","Pclass","Sex","Fare","Embarked","Family","Boat")
test <- raw.test[,test.attrName]

detach("package:Hmisc", unload=TRUE)