# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 


path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'

titanic_data <- import(path)
head(titanic_data)
tail(titanic_data)

#If we check, data is not shuffled, so if we divide we will take
#only the data of class 1 and 2

#Creates a random list from 1 to 1309
shuffle_index <- sample(1:nrow(titanic_data))
head(shuffle_index)
#We use the index to shuffle data
titanic_data <- titanic_data[shuffle_index,]
head(titanic_data)

#There are some variables that have NA's
library(dplyr)
# Drop unnecesary variables
titanic_data <- select(titanic_data,pclass,survived,sex,age)
#Converting pclass and survived into factors
titanic_data <- mutate(titanic_data,pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
                       survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes')),age = as.numeric(age))
titanic_data <- na.omit(titanic_data)
library(caTools)

set.seed(123)
split <- sample.split(titanic_data$survived, SplitRatio = 0.8) 

#Creating the training set
trainingSet <- subset(titanic_data, split == T)
#Creating the test set
testSet <- subset(titanic_data,split == F)

library(rpart)
library(rpart.plot)
library(caret)

#Creation of the model
fit <- rpart(survived~.,data = trainingSet)

#Showing the tree
rpart.plot(fit)

#Predictions
fit.survived.predicted <- predict(fit,testSet,type = 'class')

#Confusion matrix for evaluating the model
confusionMatrixSurvived <- confusionMatrix(fit.survived.predicted,testSet$survived)
confusionMatrixSurvived

prp(fit)
