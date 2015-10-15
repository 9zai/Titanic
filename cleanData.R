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
  data$Family <- as.factor(data$SibSp + data$Parch)
  return(data$Family)
}
getBoat <- function(data){
  data$Boat <- 0
  data[data$Sex == 'female','Boat'] <- 1
  data[data$Age <15,'Boat'] <-1
  return(as.factor(data$Boat))
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
raw.train$Fare[raw.train$Fare == 0] <- NA
raw.train$Fare <- replaceByMedian(raw.train$Fare,raw.train$Pclass,levels(raw.train$Pclass))

raw.train$Family <- getFamily(raw.train)
raw.train$Boat <- getBoat(raw.train)

attrName <- c("Survived","Age","Pclass","Sex","Fare","Embarked","Family","Boat","Title")
train <- raw.train[,attrName]
