# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

library(randomForest)
require(caTools)

#Importing data
data<- import("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", format = "csv")
head(data)

#Columns dont have a propper name
names(data) <- c("age", "sex", "cp", "trestbps", "choi", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thai", "num")

head(data)

summary(data)

#Mutation of the values
#Presence of heart disease goes from 0 (no presence) to 4, so we 
#will switch all the values >1 from 1
data$num[data$num > 1] <- 1
 
#There is a mean for "categorical" variables --> incorrect
#We need to convert them into factors
data <- transform(
  data,
  age=as.integer(age),
  sex=as.factor(sex),
  cp=as.factor(cp),
  trestbps=as.integer(trestbps),
  choi=as.integer(choi),
  fbs=as.factor(fbs),
  restecg=as.factor(restecg),
  thalach=as.integer(thalach),
  exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  ca=as.factor(ca),
  thai=as.factor(thai),
  num=as.factor(num)
)

summary(data)

#We switch all the columns with a ? for NA
data[data == "?"] <- NA

#We see that there are columns with NA values
colSums(is.na(data))

#We omit all the NA values
data <- na.omit(data)

#If we check summary after this, we will see that ? is still a class, so we need to cast it into factors
data$ca <- factor(data$ca)
data$thai <- factor(data$thai)

#After data treatment, we start with our model
set.seed(123)
split <- sample.split(data$num, SplitRatio = 0.8) 

#Creating the training set
trainingSet <- subset(data, split == T)
#Creating the test set
testSet <- subset(data,split == F)

#Creation of the random forest
rf <- randomForest(num~.,data=trainingSet)
rf

#Predictions
pred <- predict(rf,testSet)

library(caret)

#Confusion matrix
confusionMatrixSurvived <- confusionMatrix(pred,testSet$num)
confusionMatrixSurvived
