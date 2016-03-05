# load libraries
library(foreign)
install.packages("rpart")
library(rpart)
install.packages("randomForest")
library(randomForest)

# load data
test<-read.csv("C:/Users/m/Documents/Machine Learning/Final Project/Data/test.csv")
train<-read.csv("C:/Users/m/Documents/Machine Learning/Final Project/Data/train.csv")

# clean data
clean_data <- function(train_df, test_df) {
  test_df$Survived <- NA
  full_df<-rbind(train_df, test_df) 
  
  full_df$Name <- as.character(full_df$Name)
  
  # reduce number of titles
  full_df$Title <- sapply(full_df$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
  full_df$Title <- sub(' ', '', full_df$Title)
  #table(full_df$Title)
  full_df$Title[full_df$Title %in% c('Mme')] <- 'Mrs'
  full_df$Title[full_df$Title %in% c('Mlle')] <- 'Miss'
  full_df$Title[full_df$Title %in% c('Don', 'Sir')] <- 'Sir'
  full_df$Title[full_df$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
  full_df$Title <- factor(full_df$Title)
  
  # create a family size feature
  full_df$FamilySize <- full_df$SibSp + full_df$Parch + 1
  full_df$Surname <- sapply(full_df$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
  full_df$FamilyID <- paste(as.character(full_df$FamilySize), full_df$Surname, sep="")
  full_df$FamilyID[full_df$FamilySize <= 3] <- 'Small'
  #table(full_df$FamilyID)
  full_df$FamilyID <- factor(full_df$FamilyID)
  
  # predict missing Age value
  Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data=full_df[!is.na(full_df$Age),], method="anova")
  full_df$Age[is.na(full_df$Age)] <- predict(Agefit, full_df[is.na(full_df$Age),])
  
  # predict missing Embarked and Fare values
  #which(full_df$Embarked == '')
  full_df$Embarked[c(62,830)] = "S"
  full_df$Embarked <- factor(full_df$Embarked)
  #which(is.na(full_df$Fare))
  full_df$Fare[1044] <- median(full_df$Fare[full_df$Pclass==3], na.rm=TRUE)
  
  # Creating a new familyID2 variable that reduces the factor level of falilyID so that the random forest model
  # can be used
  full_df$FamilyID2 <- full_df$FamilyID
  full_df$FamilyID2 <- as.character(full_df$FamilyID2)
  full_df$FamilyID2[full_df$FamilySize <= 4] <- 'Small'
  full_df$FamilyID2 <- factor(full_df$FamilyID2)
  
  return(full_df)
}

# Fit different Model
train.set<-full.data[1:891,]
test.set<-full.data[892:1309,]

# Random Forest
RF.fit<-randomForest(as.factor(Survived)~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2, data=train.set, importance=TRUE, ntree=2000)
RF.pred<-predict(RF.fit,train.set)
RF.score<-sum(train.set$Survived == RF.pred)/nrow(train.set)


