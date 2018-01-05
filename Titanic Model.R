getwd()
titanic.train<-read.csv(file="train.csv",stringsAsFactors = FALSE, header = TRUE)
titanic.test<-read.csv(file="test.csv",stringsAsFactors = FALSE)

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE
ncol(titanic.train)

#Creating the 13th col in test to match both datasets.
#I want to allign them to match them together and clean both
# datasets together.
titanic.test$Survived <- NA

#Binding both datasets together.
titanic.full <- rbind(titanic.train, titanic.test)
nrow(titanic.full)

#Replace the null values of Embarked with the mode.
table(titanic.full$Embarked)
titanic.full[titanic.full$Embarked == '',"Embarked"] <- 'S'

#There are a lot of ages missing.
table(is.na(titanic.full$Age))
median(titanic.full$Age, na.rm =TRUE)
age.median <- median(titanic.full$Age)
titanic.full[is.na(titanic.full$Age),"Age"] <- age.median

#Fare NA's
table(is.na(titanic.full$Fare))
fare.median <- median(titanic.full$Fare)
titanic.full[is.na(titanic.full$Fare),"Fare"] <- fare.median

#Categorical Casting
titanic.full$Pclass<- as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)

#Data is cleaned, now we can pass them back into train and test.
titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,]
dim(titanic.train)
titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]
dim(titanic.test)

titanic.train$Survived<-as.factor(titanic.train$Survived)

#Creating the model
survived.equation<-("Survived ~ Pclass + Sex + Age + SibSp + Parch +Fare + Embarked")
survived.formula<-as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)
titanic.model<-randomForest(formula = survived.formula, data=titanic.train, ntree= 500, mtry=3,nodesize=0.01*nrow(titanic.train),na.action=na.exclude)
features.euqtion<- "Survived ~ Pclass + Sex + Age + SibSp + Parch +Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)
Survived
PassengerId<- titanic.test$PassengerId
output.df<-as.data.frame(PassengerId)
output.df$Survived <- Survived
output.df

output.df[is.na(output.df$Survived),"Survived"] <- 0



write.csv(output.df, file = "kaggle_submission.csv", row.names = FALSE)
