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
  data[data$Sex == 'female' ,'Boat'] <- 1
  data[data$Age >65,'Boat'] <-0
  data[data$Age <15,'Boat'] <-1
  return(as.factor(data$Boat))
}
getFare <- function(data){
  data$Fare[data$Fare == 0] <- NA
  data$Fare <- replaceByMedian(data$Fare,data$Pclass,levels(as.factor(data$Pclass)))
  return(data$Fare)
}
getPos <-function(data){
  data$TicketNum<-gsub("[A-Z ./]{1,10}","",data$Ticket,ignore.case = T)
  data$num<-as.numeric(data$TicketNum)
  data$num[which(is.na(data$num))] <- 0
  PosEle <- matrix(data = NA,nrow = 1309,ncol=3)
  PosEle[,1] <- data$num
  PosEle[,2] <-as.numeric(data$Pclass)
  PosEle[,3] <- data$Fare
  Pos<-kmeans(PosEle,centers = 9,nstart = 20)
  data$CabinPos<-Pos$cluster
  data$CabinPos<-factor(data$CabinPos)
  levels(data$CabinPos)<-c('UpFront','UpMiddle','UpEnd',
                           'CenFront','CenMiddel','CenEnd',
                           'BottomFront','BottomMiddle','BottomEnd')
  return(data$CabinPos)
}
cTitleAgeRelation <- function(data){#And reppace
  options(digits=2)
  data$Title <- as.factor(getTitle(data))
  bystats(data$Age,data$Title,
          fun = function(x)c(Mean=mean(x),Median=median(x)) )
  miss.title <- c("Dr","Master","Miss","Mr","Mrs","Ms")
  data$Age <- replaceByMedian(data$Age,getTitle(data),miss.title)
  return(data$Age)
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
raw.test <- read.csv("test.csv",header =T)
raw.test$Survived <- NA

raw.full <- rbind(raw.train,raw.test)
summary(raw.full)

raw.full[raw.full$Embarked=="","Embarked"] <- "S"
raw.full$Embarked <- factor(raw.full$Embarked)
raw.full$Title <- getTitle(raw.full)
Miss <- c("Dona","Lady","Ms","Mlle","Jonkheer")
Mrs <- c("Mme")
Mr <- c("Capt","Col","Don","Dr","Major","Sir","the Countess","Rev")
raw.full[which(raw.full$Title %in% Miss),"Title"] <- "Miss"
raw.full[which(raw.full$Title %in% Mrs),"Title"] <- "Mrs"
raw.full[which(raw.full$Title %in% Mr),"Title"] <- "Mr"
raw.full$Age <- cTitleAgeRelation(raw.full)

raw.full$Fare <- getFare(raw.full)
raw.full$Boat <- getBoat(raw.full)
raw.full$Family <- getFamily(raw.full)
raw.full$Pos <- getPos(raw.full)

raw.full$Survived <- factor(raw.full$Survived)
raw.full$Title <- factor(raw.full$Title)

train.attrName <- c("Survived","Age","Pclass","Sex","Fare","Title","Pos",
                    "Embarked","Family","Boat")
test.attrName <- c("Age","Pclass","Sex","Fare","Title","Pos",
                    "Embarked","Family","Boat")

train <- raw.full[which(!is.na(raw.full$Survived)),train.attrName]
test <- raw.full[which(is.na(raw.full$Survived)),test.attrName]
test$PassengerId <- row.names(test)

rm(raw.test)
rm(raw.train)
rm(Miss)
rm(Mr)
rm(Mrs)
detach("package:Hmisc", unload=TRUE)