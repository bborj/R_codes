#PREDICTING THE REVENUE USING LINEAR REGRESSION
# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

#Generating the dataset
sales <- import("/Users/borja/Desktop/Curso R/Curso Machine Learning/Datasets/revenue.xlsx")
#Showing the dataset
sales
#Summary
summary(sales)
#Head (first 6 rows)
head(sales)
#Plot of sales
plot(sales)

#Splitting the data into training and test data
#Random number
set.seed(2)
library(caTools)
#Splitting the data (70% training 30% training)
split <- sample.split(sales, SplitRatio = 0.7)
split
#Creating the training set
#It takes all the data from sales where split is TRUE
trainingSet <- subset(sales, split = T)
#Creating the test set
testSet <- subset(sales,split = F)

#Create the model
#Match up the Profit with the training set
Model <- lm(Profit ~., data=trainingSet)
#Si tuvieramos **** en los signif codes, querria decir que practicamente son los mismos datos
summary(Model)


#Prediction
prediction <- predict(Model, testSet)
prediction

#Confusion Matrix
confmatrix <- table(Actual_value = testSet$Profit, predicted_value = prediction > 0.5)
confmatrix

#Comparing prediction vs actual values
plot(testSet$Profit, type = "l", lty=1.8, col="red")
lines(prediction, type="l", lty=1.8,col="blue")

#Finding Accuracy
rmse <- sqrt(mean(prediction-sales$Profit)^2)
rmse

