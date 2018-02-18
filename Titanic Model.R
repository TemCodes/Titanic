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

#Clean age missing data.
table(is.na(titanic.full$Age))
boxplot(titanic.full$Age)
#We want to get rid of all the outliears.
upper.whisker.age <- boxplot.stats(titanic.full$Age)$stats[5]
outlier.filter <- titanic.full$Fare < upper.whisker.age
titanic.full[outlier.filter,]
age.equation = "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
age.model <- lm(
  formula = age.equation,
  data = titanic.full[outlier.filter,]
)
age.row<-titanic.full[
  is.na(titanic.full$Age),
  c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Embarked")
  ]
age.predictions <-predict(age.model, newdata = age.row)
titanic.full[is.na(titanic.full$Age),"Age"] <- age.predictions

#Cleaning fare missing data.
table(is.na(titanic.full$Fare))
boxplot(titanic.full$Fare)
#We want to get rid of all the outliears.
upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter <- titanic.full$Fare < upper.whisker
titanic.full[outlier.filter,]
fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(
    formula = fare.equation,
    data = titanic.full[outlier.filter,]
)
fare.row<-titanic.full[
    is.na(titanic.full$Fare),
    c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
]
fare.predictions <-predict(fare.model, newdata = fare.row)
titanic.full[is.na(titanic.full$Fare),"Fare"] <- fare.predictions

#Categorical Casting
titanic.full$Pclass<- as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)
titanic.full$Embarked<-as.ordered(titanic.full$SibSp)

#Data is cleaned, now we can pass them back into train and test.
titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,]
dim(titanic.train)
titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]
dim(titanic.test)

titanic.train$Survived<-as.factor(titanic.train$Survived)


#Creating the model______________________________________________________________________________________________________________________________________________
survived.equation<-("Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked")
survived.formula<-as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)
titanic.model<-randomForest(formula = survived.formula, data=titanic.train, ntree= 500, mtry=3,nodesize=0.01*nrow(titanic.train))
features.euqtion<- "Survived ~ Pclass + Sex + Age + SibSp + Parch +Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)
PassengerId<- titanic.test$PassengerId
output.df<-as.data.frame(PassengerId)
output.df$Survived <- Survived
output.df
table(output.df$Survived)
write.csv(output.df, file = "kaggle_submission.csv", row.names = FALSE)

