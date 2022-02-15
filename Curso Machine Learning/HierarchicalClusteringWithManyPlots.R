# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

#Importing data
seeds_data <- read.csv("/Users/borja/Desktop/Curso R/Curso Machine Learning/Datasets/seeds_dataset.txt",sep = '\t',header = FALSE)

#We add column names
features_name <- c('area','perimeter','compactness','length.of.kernel','width.of.kernal','asymmetry.coefficient','length.of.kernel.groove','type.of.seed')
colnames(seeds_data) <- features_name

str(seeds_data)
summary(seeds_data)

#We shuffle data
#Creates a random list from 1 to 1309
shuffle_index <- sample(1:nrow(seeds_data))
head(shuffle_index)
#We use the index to shuffle data
seeds_data <- seeds_data[shuffle_index,]
head(seeds_data)
#We omit NA's
seeds_data <- na.omit(seeds_data)

#We store the labels in a separate variable
seeds_label <- seeds_data$type.of.seed
seeds_data$type.of.seed <- NULL
str(seeds_data)

#Scaling data
seeds_data <- as.data.frame(scale(seeds_data))
#Notice the mean of all the columns is 0 and the standard deviation is 1. Now that you have pre-processed your data it's time to build the distance matrix
summary(seeds_data)

#Build distance matrix, since all the values are continuous, we will use euclidean distance method
dist_mat <- dist(seeds_data, method = 'euclidean')

#Model
hclust_model <- hclust(dist_mat, method = 'average')
plot(hclust_model)

#Since there are just 3 classes
cut_avg <- cutree(hclust_model, k = 3)

plot(hclust_model)
#Plot
rect.hclust(hclust_model, k = 2, border = "blue")
rect.hclust(hclust_model, k = 3, border = "green4")
rect.hclust(hclust_model, k = 4, border = "gray")

install.packages('dendextend', dependencies = TRUE)
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_model)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)

#Now we will check how many observations were assigned to each cluster
library(dplyr)
seeds_df_cl <- mutate(seeds_data, cluster = cut_avg)
count(seeds_df_cl,cluster)
library(ggplot2)
ggplot(seeds_df_cl, aes(x=area, y = perimeter, color = factor(cluster))) + geom_point()
