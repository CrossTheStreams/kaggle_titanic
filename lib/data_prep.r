prep.data <- function(data, training=T) {

  data$male <- data$Sex == "male"
  data$female <- data$Sex == "female"

  data$Parch
  data$SibSp

  data$Fare
  data$Embarked
  data$Embarked.S <- data$Embarked == "S"
  data$Embarked.C <- data$Embarked == "C"
  data$Embarked.Q <- data$Embarked == "Q"

  data$first.class <- data$Pclass == 1
  data$second.class <- data$Pclass == 2
  data$third.class <- data$Pclass == 3

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

