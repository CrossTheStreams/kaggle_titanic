# Andrew Hautau
# Assignment 7
# Titanic Kaggle Competition

source('lib/packages.r')
source('lib/data_prep.r')
source('lib/unsupervised_learning.r')

train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')

train <- prep.data(train)
prepped.test <- prep.data(test,training=F)
 
boxplot(train$Survived,train$Fare)
boxplot(train[which(train$Survived == F),"Fare"])

columns <- names(train)[-which(names(train) %in% c("PassengerId","Name","Pclass","Cabin","Age","SibSp", "Survived","Parch", "Embarked", "Sex", "Ticket", "Fare"))]

signal <- mut.info(train[sample(1:nrow(train),size=200),],columns)
plot.signal(signal)

# Need to impute age in order to take advantage of the variable...
with.age <- train[which(is.na(train$Age)),]
# Miss/Master indicates a child
with.age[which(with.age$Age < 14),"Name"]     
# A higher Fare would probably indicate an older person?

train$Ticket

cor(train$Fare,train$Survived)

train$Pclass

# Based on analysis so far...

# Features with good signal without any adjustment seem to be sex, fares, first class, third class, b.cabin

variables <- c("female","Fare", "first.class","third.class")

train.survival.model <- function(data, variables) {

  target <- data$Survived
  model.train <- subset(data, select=c(variables,"Survived"))
  model.train[,variables] <- scale(model.train[,variables])

  model <- svm(Survived ~ female+Fare+first.class+third.class, data=model.train)

  prediction.data <- subset(model.train, select=variables)
  pred <- predict(model, prediction.data)
  tab <- table(pred=round(pred),true=as.factor(target))

  return(list(model=model,conf.matrix=tab,performance=classAgreement(tab)))
}

training.output <- train.survival.model(train,c("female","Fare", "first.class","third.class"))

model <- training.output$model

test.survival.model <- function (test.model,variables,test.data) { 

  test.data.scaled <- scale(subset(test.data, select =c(variables)))
  test.data.scaled[which(is.na(test.data.scaled[,2])),2] <- median(test.data.scaled[,2],na.rm=T)
  test.pred <- predict(test.model, test.data.scaled)  
  write.csv(cbind(PassengerId=test$PassengerId,Survived=round(test.pred)),
            paste("titanic_submission_",format(Sys.time(), "%m_%d_%y_%X.csv"),sep=""), 
            row.names=F)
}

test.survival.model(model,variables,prepped.test)
