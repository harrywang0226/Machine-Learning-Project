## load libraries
library(foreign)
#install.packages("rpart")
library(rpart)
#install.packages("randomForest")
library(randomForest)
#install.packages("e")
library(e1071)
#install.packages("xgboost")
library(xgboost)
library(ggplot2)
library(Ckmeans.1d.dp)
library(class)

## load data
test<-read.csv("C:/Users/m/Documents/Machine Learning/Final Project/Data/test.csv")
train<-read.csv("C:/Users/m/Documents/Machine Learning/Final Project/Data/train.csv")

## clean data
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

###################################################################################

## Fit different Model
full.data<-clean_data(train,test)
train.set<-full.data[1:891,]
test.set<-full.data[892:1309,]

# Random Forest
RF.fit<-randomForest(as.factor(Survived)~Sex + Pclass + Age + Title + FamilySize + FamilyID2, data=train.set, importance=TRUE, ntree=2000)
RF.pred<-predict(RF.fit,train.set)
RF.score<- sum(train.set$Survived == RF.pred)/(nrow(train.set))

rows<-nrow(train.set)
k<-10
for(i in 1:k){
  ind<-sample(1:rows,rows/k)
  RF.fit<-randomForest(as.factor(Survived)~Sex + Pclass + Age + Title + FamilySize + FamilyID2, data=train.set[-ind,], importance=TRUE, ntree=2000)
  RF.pred<-predict(RF.fit, train.set[ind,])
  RF.score<-sum(train.set$Survived[ind] == RF.pred)/length(ind)
  print(RF.score)
}


# Logistic Regression
LR.fit<-glm(Survived~Sex*Age + Sex*FamilySize + Sex*Pclass + Sex*Title + Pclass*Age + Pclass*FamilySize + Pclass*Title + Age*Title + Age*FamilySize + Title*FamilySize, data=train.set ,family=binomial)
LR.pred<-predict.glm(LR.fit, type="response")
LR.pred<-ifelse(LR.pred>0.5,1,0)
LR.score<-sum(train.set$Survived == LR.pred)/(nrow(train.set))

# SVM
model<-Survived~Sex + Pclass + Age +  Title + FamilySize
rows<-nrow(train.set)
# 10-fold
k<-10
score<-0
for (i in 1:k) {
  ind<-sample(1:rows, rows/k)
  SVM.fit<-svm(model, data = train.set[-ind, ], kernel="radial")
  SVM.pred<-predict(SVM.fit, train.set[ind, ])
  SVM.pred<-ifelse(SVM.pred > 0.5, 1, 0)
  newScore<-sum(train.set$Survived[ind] == SVM.pred)/length(ind)
  print(newScore)
  score<-score + newScore
}
SVM.score = score/k

SVM.fit<-svm(model,data=train.set,kernel="radial")


# xgboost
full.data2<-full.data[, -c(1,4,9, 11, 15,17)]
full.data2$Pclass <- as.numeric(full.data2$Pclass)-1
full.data2$Sex <- as.numeric(full.data2$Sex) -1
full.data2$Embarked <- as.numeric(full.data2$Embarked) -1
full.data2$Title <- as.numeric(full.data2$Title) -1
full.data2$FamilySize <- as.numeric(full.data2$FamilySize) -1
full.data2$FamilyID <- as.numeric(full.data2$FamilyID) -1
full.data2 <- as.matrix(full.data2)
train.set2<-full.data2[1:891,]
test.set2<-full.data2[892:1309,]

param <- list("objective" = "binary:logistic")

xgboost.cv = xgb.cv(param=param, data = train.set2[, -c(1)], label = train.set2[, c(1)], nfold =10, nrounds =15)

xgboost.fit <- xgboost(param =param, data = train.set2[, -c(1)], label = train.set2[, c(1)], nrounds=15)

names <- dimnames(train.set2)[[2]]
importance.matrix <- xgb.importance(names, model = xgboost.fit)
xgb.plot.importance(importance.matrix)

#pred_xgboost_test <- predict(fit_xgboost, test.set2[, -c(1)])
xgboost.pred.train <- predict(xgboost.fit, train.set2[, -c(1)])

# find the best cut-off:
proportion <- sapply(seq(.3,.7,.01),function(step) c(step,sum(ifelse(xgboost.pred.train<step,0,1)!=train.set2[, c(1)])))
dim(proportion)
# applying the best cut-off 
xgboost.pred.train <- ifelse(xgboost.pred.train<proportion[,which.min(proportion[2,])][1],0,1)
head(xgboost.pred.train)
xgboost.score <- sum(train.set2[, c(1)] == xgboost.pred.train)/nrow(train.set2)
# applying the best cut-off on the test set
#xgboost.pred.test <- ifelse(pred_xgboost_test<proportion[,which.min(proportion[2,])][1],0,1)
test <- as.data.frame(test) # Conveting the matrix into a dataframe

# KNN
full.data3<-full.data[, c(3,5,6,7,8,10,12,13,14)]
full.data3$Pclass <- as.numeric(full.data3$Pclass)
full.data3$Sex <- as.numeric(full.data3$Sex)
full.data3$Embarked <- as.numeric(full.data3$Embarked)
full.data3$Title <- as.numeric(full.data3$Title)
full.data3$FamilySize <- as.numeric(full.data3$FamilySize)
train.set3<-full.data3[1:891,]
test.set3<-full.data3[892:1309,]
cl<-factor(train.set[,c(2)])
KNN.result<- knn(train.set3,test.set3,cl,k=10)


##################################################################################
RF.result<- predict(RF.fit,test.set)
RF.result<- data.frame(RF.result)
RF.result<- cbind(test.set$PassengerId, RF.result)

SVM.result<- predict(SVM.fit,test.set)
SVM.result<- ifelse(SVM.result>0.5,1,0)
SVM.result <- data.frame(SVM.result)
SVM.result <- cbind(test.set$PassengerId,SVM.result)

xgboost.result <- predict(xgboost.fit, test.set2[, -c(1)])
xgboost.result <- ifelse(xgboost.pred.test<proportion[,which.min(proportion[2,])][1],0,1)
xgboost.result <- data.frame(xgboost.result)
xgboost.result <- cbind(test.set$PassengerId,xgboost.result)

LR.result<-predict.glm(LR.fit,newdata=test.set, type="response")
LR.result<-ifelse(LR.result>0.5,1,0)
LR.result<- data.frame(LR.result)
LR.result<- cbind(test.set$PassengerId, LR.result)

KNN.result<- data.frame(KNN.result)
KNN.result<- cbind(test.set$PassengerId, KNN.result)

write.csv(SVM.result, file="result.csv",row.names = F, quote = F)
#write.table(RF.result, file = "result.csv",col.names=c("Passengerid","Survived"), sep=",")

