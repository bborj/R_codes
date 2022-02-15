# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

#Generating the dataset
social_data <- import("/Users/borja/Desktop/Curso R/Curso Machine Learning/Datasets/social.csv",, format = "csv")
#We take just the important data
social_data <- social_data[3:5]

head(social_data)

#Converting Into Factors
social_data <- transform(
  social_data,
  Purchased=as.factor(Purchased)
)

#We train the model
library(caTools)
set.seed(123)
split <- sample.split(social_data$Purchased,, SplitRatio = 0.8)

#Creating the training set
trainingSet <- subset(social_data, split == T)
#Creating the test set
testSet <- subset(social_data,split == F)

#Feature Scaling (es ideal en algoritmos que calculan distancias entre datos)
trainingSet[-3] <- scale(trainingSet[-3])
testSet[-3] <- scale(testSet[-3])

#Classificator
# Fitting SVM to the Training set
install.packages('e1071')
library(e1071)
svm_classifier <- svm(formula = Purchased ~ .,
                      data = trainingSet,
                      type = 'C-classification',
                      kernel = 'linear')
summary(svm_classifier)

#Predictions
prediction <- predict(svm_classifier, newdata = testSet[-3])

#Calculating the confussion matrix
library(caret)
confusionMatrixSurvived <- confusionMatrix(prediction,testSet$Purchased)
confusionMatrixSurvived

#Visualization of data
plot(social_data[,-3],col=(3)/2,pch=19)
abline(h=0,v=0,lty=3)
points(social_data[svm_classifier$index,c(1,2)],col="orange",cex=2)

#Getting the parameters of the hyperplane
w <- t(svm_classifier$coefs)
b <- svm_classifier$rho

abline(a=-b/w[1,2],b=-w[1,1]/w[1,2],col="blue",lty=3)
