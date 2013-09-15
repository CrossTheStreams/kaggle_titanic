# Andrew Hautau
# Assignment 7
# Titanic Kaggle Competition

train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')

source('lib/packages.r')
source('lib/unsupervised_learning.r')
source('lib/data_prep.r')

train <- prep.data(train)
prepped.test <- prep.data(test,training=F)
 
boxplot(main="Fare Distributions, Died and Survived",train$Survived,train$Fare,outline=T)
boxplot(train[which(train$Survived == T),"Fare"])
cor(train$Fare,train$Survived)

columns <- names(train)[-which(names(train) %in% c("PassengerId","Name","Pclass","Cabin","Age","SibSp", "Survived","Parch", "Embarked", "Sex", "Ticket", "Fare"))]

columns <- c("Fare1","Fare2","Fare3","Fare4")

signal <- signal.metrics(train[sample(1:nrow(train),size=200),],columns)

plot.signal(signal)

# Need to impute age in order to take advantage of the variable...
with.age <- train[which(!is.na(train$Age)),]
# Miss/Master indicates a child
with.age[which(with.age$Age < 14),"Name"]     
# A higher Fare would probably indicate an older person?


# Based on analysis so far...

# Features with good signal without any adjustment seem to be sex, fares, first class, third class, b.cabin

variables <- c("female","Fare", "first.class","third.class","Young","Fare1","Fare2","Fare3","Fare4")

cluster.variables <- c("c1","c2","c3","c4","c5","c6","c7")

train <- titanic.kmeans(train,variables,7)
prepped.test <- titanic.kmeans(prepped.test,variables,7)


titanic.svm.model <- function(data, variables) {

  target <- data$Survived
  model.train <- subset(data, select=c(variables,"Survived"))
  model.train[,variables] <- scale(model.train[,variables])

  model <- svm(Survived ~ female+Fare+first.class+third.class+Young+Fare1+Fare2+Fare3+Fare4, data=model.train)

  prediction.data <- subset(model.train, select=variables)
  pred <- predict(model, prediction.data)
  tab <- table(pred=round(pred),true=as.factor(target))

  return(list(model=model,conf.matrix=tab,performance=classAgreement(tab)))
}

titanic.random.forest <- function (data,variables) {

  model.data <- subset(data, select=c(variables,"Survived"))
  model.data[,variables] <- scale(model.data[,variables])
  
  fit <- randomForest(Survived ~ female + Fare + first.class + third.class + Young + Fare1 + Fare2 + Fare3+ Fare4,   data=model.data)

}

training.output <- titanic.svm.model(train,variables)

rf.model <- titanic.random.forest(train,variables)

pred <- sapply(training.output$predicted,function(x){return((x - 0.5) > 0)})

svm.model <- training.output$model

test.survival.model <- function (test.model,variables,test.data,model.type="svm") { 

  test.data.scaled <- scale(subset(test.data, select =c(variables)))
  test.data.scaled[which(is.na(test.data.scaled[,2])),2] <- median(test.data.scaled[,2],na.rm=T)
  if (model.type == "randomforest") {
    test.pred <- sapply(predict(test.model, test.data.scaled),function(x){return((x - 0.5) > 0)})
  }
  else {
    test.pred <- predict(test.model, test.data.scaled)  
  }

  write.csv(cbind(PassengerId=test$PassengerId,Survived=round(test.pred)),
            paste("titanic_submission_",format(Sys.time(), "%m_%d_%y_%X.csv"),sep=""), 
            row.names=F)
}
test.survival.model(model,variables,prepped.test)

test.survival.model(model,variables,prepped.test,model.type="random.forest")

