prep.data <- function(data, training=T) {

  data$male <- data$Sex == "male"
  data$female <- data$Sex == "female"

  #data$Parch
  #data$SibSp

  #data$Fare
  #data$Embarked
  #data$Embarked.S <- data$Embarked == "S"
  #data$Embarked.C <- data$Embarked == "C"
  #data$Embarked.Q <- data$Embarked == "Q"

  data$first.class <- data$Pclass == 1
  data$second.class <- data$Pclass == 2
  data$third.class <- data$Pclass == 3

  # Impute age using SibSp and Parch variables
  for (i in 1:nrow(data)) {
    if (is.na(data[i,"Age"])) {
      parch <- data[i,"Parch"]  
      sibsp <- data[i,"SibSp"]
      parch.mean.age <- mean(data[which(data$Parch == parch),"Age"],na.rm=T)
      sibsp.mean.age <- mean(data[which(data$SibSp == sibsp),"Age"],na.rm=T)
      if (!is.na(parch.mean.age) & !is.na(sibsp.mean.age)) {
        data[i,"Age"] <- mean(parch.mean.age,sibsp.mean.age) 
      }
      else {
        data[i,"Age"] <- median(data$Age,na.rm=T)
      }
    } 
  }

  # "Young" feature based on "Age" under 16 and having "Master" in name.

  data$Young <- F
  data[which(data$Age < 16),"Young"] <- T
  data[grep("Master",data$Name),"Young"] <- T

  # Bin Fare according to quantile

  data$Fare1 <- data$Fare2 <- data$Fare3 <- data$Fare4 <- F
  fare.quantiles <- quantile(data$Fare,na.rm=T)
  for (i in 1:nrow(data)) {  
     fare <- data[i,"Fare"]
     if (!is.na(fare)){
       fare.q <- sum(fare.quantiles < fare)
       if (fare.q != 0) {
         data[i,paste("Fare",fare.q,sep="")] <- T  
       }
     }
  } 

  # Does Cabin contain one of these letters?
  # Assign to categorical columns
  # This could be improved later by some sort of imputation...
  for (x in c("a","b","c","d","e","f")) {
    col.name <- paste(x,".cabin",sep="")
    data[,col.name] <- rep(F,nrow(data))
    data[grep(toupper(x), data$Cabin),col.name] <- T
  }

  if (training) {
    data$Fare <- rm.outlier(data$Fare,fill=T)  
  }
  else {
   data[which(is.na(data$Fare)),"Fare"] <- median(data$Fare,na.rm=T) 
  }

  return(data)
}


age.names <- data.frame(Name=train$Name,Age=train$Age,Survived=train$Survived,Parch=train$Parch,SibSp=train$SibSp)

age.names$Young <- F

age.names[which(age.names$Age < 16),"Young"] <- T

age.names[grep("Miss",age.names$Name),"Age"]

age.names[grep("Master",age.names$Name),"Young"] <- T

cor(age.names[which(!is.na(age.names$Young)),"Young"],age.names[!is.na(age.names$Young),"Survived"])



age.names$YoungAdult <- F

age.names[which(age.names$Age > 20 & age.names$Age < 30),"YoungAdult"] <- T

cor(age.names$YoungAdult,age.names$Survived)



age.names$Older <- F

age.names[which(age.names$Age > 50),"Older"] <- T

cor(age.names$Older,age.names$Survived)






