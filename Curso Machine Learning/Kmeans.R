# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, tidyverse,rio) 

#USArrests
head(USArrests)
data <- USArrests

#Scaling the data
scaled_data <- scale(data)

#First rows of scaled data
head(scaled_data)

#Packages
install.packages("factoextra")
library(factoextra)

#Choosing the number of clusters
#We choose k=4 
set.seed(123)
#nstart = 25 quiere decir que R va a intentar 25
#inicios aleatorios asignando valores a las variables
#y elegira el que tenga menos variacion en los clusters
#lo recomendable suele ser 25 o 50, por defecto es 1 (muy bajo)
km.res <- kmeans(scaled_data,4,nstart=25)

km.res

#Computing the mean of each variables using the original data
aggregate(data, by=list(cluster=km.res$cluster),mean)

#Adding the clasifications to the data
data <- cbind(data,cluster = km.res$cluster)
data

#Accesing to the results of k-means
km.res$cluster
km.res$size
km.res$centers

#Visualization of data
#We need to use Principal Component Analysis because there are +2D
fviz_cluster(km.res,data,ellipse.type = "norm")
