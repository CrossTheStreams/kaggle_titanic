# I still need to get the syntax right for the formulas so they can be dynamic.
# Right now, model functions are being given statically 
# Should be improved to use ?formula objects

# SVM model
titanic.svm.model <- function(data, variables) {

  target <- data$Survived
  model.train <- subset(data, select=c(variables,"Survived"))
  model.train[,variables] <- scale(model.train[,variables])

  model <- svm(Survived ~ female+Fare+first.class+third.class+Young+Fare1+Fare2+Fare3+Fare4, data=model.train)

  prediction.data <- subset(model.train, select=variables)
  pred <- predict(model, prediction.data)
  tab <- table(pred=round(pred),true=as.factor(target))

  return(list(model=model,conf.matrix=tab,performance=classAgreement(tab),predictions=pred))
}

# Random Forest
titanic.random.forest <- function (data,variables) {

  model.data <- subset(data, select=c(variables,"Survived"))
  model.data[,variables] <- scale(model.data[,variables])
  
  fit <- randomForest(Survived ~ female + Fare + first.class + third.class + Young + Fare1 + Fare2 + Fare3+ Fare4,   data=model.data)

}

# Ensemble using SVM and Random Forest
titanic.ensemble <- function(svm.model,rf.model,variables,test.data,training.set) { 

    test.data.scaled <- scale(subset(test.data, select =c(variables)))
    test.data.scaled[which(is.na(test.data.scaled[,2])),2] <- median(test.data.scaled[,2],na.rm=T)

    training.survived <- training.set$Survived
    training.rf.pred <- training.set$rf.pred
    training.svm.pred <- training.set$svm.pred

    training.set.scaled <- scale(subset(training.set, select =c(variables)))
    training.set.scaled[which(is.na(training.set.scaled[,2])),2] <- median(training.set.scaled[,2],na.rm=T)

    # Make predictions with both models on test data
    test.rf.pred <- round(predict(rf.model, test.data.scaled))
    test.svm.pred <- round(predict(svm.model, test.data.scaled))
    
    # Mean values for attributes we're predicting on where random forest predicted correctly
    rf.training <- training.set.scaled[which(training.rf.pred == training.survived),]
    rf.sum <- rf.training[1,]
    for (i in 2:nrow(rf.training)){
      rf.sum <- rf.sum + rf.training[i,]
    }
    rf.means <- rf.sum/nrow(rf.training)

    # same for svm
    svm.training <- training.set.scaled[which(training.svm.pred == T & training.survived == T),]    
    svm.sum <- svm.training[1,]
    svm.means <- svm.sum/nrow(svm.training)

    for (i in 2:nrow(svm.training)){
      svm.sum <- svm.sum + svm.training[i,]
    }
    svm.means <- svm.sum/nrow(svm.training)

    test.pred <- c()

    # Use test data predictions that are closest to the correct mean 
    for (i in 1:nrow(test.data.scaled)) {
      rf.error <- sum(abs(test.data.scaled[i,] - rf.means))
      svm.error <- sum(abs(test.data.scaled[i,] - svm.means))

      if (rf.error < svm.error) {         
        test.pred[i] <- test.rf.pred[i]
      }
      else {
        test.pred[i] <- test.svm.pred[i] 
      }
    }

    write.csv(cbind(PassengerId=test.data$PassengerId,Survived=round(test.pred)),
            paste("titanic_submission_",format(Sys.time(), "%m_%d_%y_%X.csv"),sep=""), 
            row.names=F)
    
    # Also produce predictions for training set

    training.pred <- c()
    for (i in 1:nrow(training.set.scaled)) {
      rf.error <- sum(abs(training.set.scaled[i,] - rf.means))
      svm.error <- sum(abs(training.set.scaled[i,] - svm.means))

      if (rf.error < svm.error) {         
        print("rf wins")
        training.pred[i] <- training.rf.pred[i]
      }
      else {
        print("svm wins")
        training.pred[i] <- training.svm.pred[i] 
        if (training.svm.pred[i] == training.rf.pred[i]) {
          print("damn, it was the same anyhow") 
        }
        else { 
          print("yeehaw, different prediction!") 
        }
      }
    }

    return(list(test.pred=test.pred,train.pred=training.pred,train.survived=training.survived))   

}

# Apply a single SVM or Random Forest model to the test set.
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

