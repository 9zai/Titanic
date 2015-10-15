# Run this script after run the cleanData
# Apply some pre process method to test 
# Create the added attributes to test
# After testing, decide whether missing value should be add.
#current 0.77512 - best 0.789(Random Forest)
library(rpart)
raw.test <- read.csv("test.csv",header =T)

raw.test$Pclass <- as.factor(raw.test$Pclass)
raw.test$Title <- getTitle(raw.test)
raw.test$Family <- getFamily(raw.test)
cTitleAgeRelation(raw.test)
raw.test[raw.test$Title=="Ms","Title"]<- "Mr"# two special Title Dona - Don/ Ms - Mr
raw.test[raw.test$Title=="Dona","Title"]<- "Don"
test.missTitle <- c("Master","Miss","Mr","Mrs")
raw.test$Age <- replaceByMedian(raw.test$Age,raw.test$Title,test.missTitle)
 
raw.test$Boat <- getBoat(raw.test)

test.attrName <- c("PassengerId","Age","Pclass","Sex","Fare","Embarked","Family","Boat","Title")
test <- raw.test[,test.attrName]
dtree <- rpart(Survived~.,data = train)

result.dtree <- predict(dtree,test,type="class")

my_sol <- data.frame(PassengerId = test$PassengerId, Survived = result.dtree)
write.csv(my_sol,file = "my_sol.csv", row.names = FALSE)