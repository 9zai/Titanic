setwd("/Users/9zai/Desktop/competition/Titanic/")
column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
train <- read.csv("train.csv",colClasses = column.types,header = T,stringsAsFactors = F)

barplot(table(train$Survived),
        names.arg = c("Perished","Survived"),
        main = "Survived",col = "black")

library(vcd)
mosaicplot(train$Pclass ~ train$Survived, main="Class ",shade = F,color = T)
mosaicplot(train$Embarked ~ train$Survived,main = "Embarked", shade = F,color=T)
boxplot(train$Age ~ train$Survived,main="Age")

summary(train$Name)
summary(train$Age)