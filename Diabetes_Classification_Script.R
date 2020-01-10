# Step 1. Getting the data -----------------------------------------

# Load the libraries
library(tidyverse)
library(caret)
library(skimr)
library(randomForest)

# Read the file
dataset <- read.csv("dataset.txt", sep = ",", header = TRUE, )

# Step 2. Exploratory Analysis -------------------------------------

# See how many observations and variables are available
str(dataset)
# Glimpse of mean, median and NA's
summary(dataset)
# Deal with NAs
dataset <- dataset[complete.cases(dataset),]
# Count how many zeros are in each variable
colSums(dataset[,-9] == 0, na.rm = TRUE)
# Convert zeros to NA
dataset[,c(2:6)] <- apply(dataset[,c(2:6)], 2, function(x) {ifelse(x==0, NA, x)} )
# Convert outcome into factor
dataset$Outcome <- as.factor(dataset$Outcome)
# See the descriptive stats from the data
skim(dataset)

# Step 3. Preprocess the data

# Set seed
set.seed(1979)
# Create Train and Test sets
index_train <- createDataPartition(dataset$Age, times = 1, p = 0.8, list = FALSE)
train_set <- dataset[index_train,]
test_set <- dataset[-index_train,]
median(train_set$SkinThickness, na.rm = TRUE)
probatorio <- ifelse(is.na(train_set$SkinThickness), 
                     median(train_set$SkinThickness, na.rm = TRUE), train_set$SkinThickness)
median(probatorio)
dim(train_set[,4:5])
median(probatorio)
train_set[,2:6] <- apply(train_set[,2:6], 2, function(x) {ifelse(is.na(x), median(x, na.rm = TRUE), x)})

apply(dataset[,-9], 2, FUN = median(na.rm = TRUE))
dataset_x <- dataset[,-9]
apply(dataset_x,2, median, na.rm = TRUE)
parmi <- apply(dataset_x,2, median, na.rm = TRUE)
      