# Andrew Hautau
# Assignment 7
# Titanic Kaggle Competition

train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')

source('lib/packages.r')
source('lib/unsupervised_learning.r')
source('lib/data_prep.r')
source('lib/models.r')

# This is creating a lot of categorical features for us to use.
train <- prep.data(train)
prepped.test <- prep.data(test,training=F)
 
# Distribution of Fare among died and survived.
boxplot(Fare ~ Survived, data=train, main="Fare Distributions, Died and Survived")
boxplot(train[which(train$Survived == T),"Fare"])
# Some correlation here.
cor(train$Fare,train$Survived)

# What looks promising among all categorical variables.
columns <- names(train)[-which(names(train) %in% c("PassengerId","Name","Pclass","Cabin","Age","SibSp", "Survived","Parch", "Embarked", "Sex", "Ticket", "Fare"))]
signal <- signal.metrics(train[sample(1:nrow(train),size=200),],columns)
plot.signal(signal)

variables <- c("female","Fare", "first.class","third.class","Young","Fare1","Fare2","Fare3","Fare4")

# Used this for assigning features created from kmeans clusters
# train <- titanic.kmeans(train,variables,7)
# prepped.test <- titanic.kmeans(prepped.test,variables,7)

train.samp <- train
# train.samp <- train[sample(1:nrow(train),size=300),]

# Training of SVM model
training.output <- titanic.svm.model(train.samp,variables)
svm.model <- training.output$model
train.samp$svm.pred <- round(training.output$predictions)

# Training of Random Forest
rf.model <- titanic.random.forest(train.samp,variables)
train.samp$rf.pred <- round(rf.model$predicted)


# Predict on test data with SVM model
test.survival.model(svm.model,variables,prepped.test,model.type="svm")
# Predict on test data with Random Forest
test.survival.model(rf.model,variables,prepped.test,model.type="random.forest")


# Predict on training set and test data using ensemble of SVM and Random Forest
ensemble.output <- titanic.ensemble(svm.model,rf.model,variables,prepped.test,train.samp)

# Let's look at some performance indicators 
confusionMatrix(data=train.samp$rf.pred,reference=train.samp$Survived)
confusionMatrix(data=train.samp$svm.pred,reference=train.samp$Survived)
confusionMatrix(data=ensemble.output$train.pred,reference=ensemble.output$train.survived)
