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
# Convert outcome into factor, change name to Diabetes
dataset$Outcome <- as.factor(ifelse(dataset$Outcome == 1,"Yes","No"))
names(dataset)[9]<- "Diabetes"
# See the descriptive stats from the data
skim(dataset)
# Plot all variables and see the effects on outcome
dataset %>% gather(key = "Variable", value = "Measure", -Diabetes) %>% 
  ggplot(aes(Measure, fill = Diabetes)) + geom_density(alpha = 0.3) + 
  facet_wrap(~Variable,ncol = 4, scales = "free")
dataset %>% gather(key = "Variable", value = "Measure", -Diabetes) %>% 
  ggplot(aes(Diabetes, Measure, fill = Diabetes)) + geom_boxplot() + 
  facet_wrap(~Variable,ncol = 4, scales = "free")

# Step 3. Preprocess the data --------------------------------------

# Set seed
set.seed(1979)
# Create Train and Test sets
index_train <- createDataPartition(dataset$Age, times = 1, p = 0.8, list = FALSE)
train_set <- dataset[index_train,]
test_set <- dataset[-index_train,]
# For all NA values in variables 2 to 6, replace with the median
median(train_set$Insulin)
median(train_set$Pregnancies)
medians
medians <- apply(train_set[,2:6], 2, median, na.rm = TRUE)
train_set[,2:6] <- apply(train_set[,2:6], 2, function(x) {ifelse(is.na(x), median(x, na.rm = TRUE), x)})

apply(dataset[,-9], 2, FUN = median(na.rm = TRUE))
dataset_x <- dataset[,-9]
apply(dataset_x,2, median, na.rm = TRUE)
parmi <- apply(dataset_x,2, median, na.rm = TRUE)

#Step 2. Preprocess, make exploratory and variable importance analysis
range <- preProcess(train_set, method = "range")
train_set_ranged <- predict(range, newdata = train_set)

featurePlot(x = train_set_ranged[,1:8],
            y = train_set_ranged$Outcome,
            plot = "box",
            scales = list(x = list(relation = "free"),
                          y = list(relation = "free")))

featurePlot(x = train_set_ranged[,1:8],
            y = train_set_ranged$Outcome,
            plot = "density",
            scales = list(x = list(relation = "free"),
                          y = list(relation = "free")))

control <- rfeControl(functions = rfFuncs,
                      verbose = FALSE)
exploratory_varimp <- rfe(x = train_set_ranged[,1:8],
                          y = train_set$Outcome,
                          rfeControl = control)
exploratory_varimp

#Step 3. Create a ML model and obtain metrics
model_MARS <- train(Outcome ~., data = train_set_ranged, method = "earth")
model_MARS
plot(model_MARS, main = "Accuracy of MARS model")
plot(varImp(model_MARS), main = "Variable importance for MARS model")

#Step 4. Preprocess and predict on test set and obtain confusion matrix
range <- preProcess(test_set, method = "range")
test_set_ranged <- predict(range, newdata = test_set)

predictions <- predict(model_MARS, test_set_ranged)

confusionMatrix(reference = test_set_ranged$Outcome, data = predictions)

#Step 5. Create an ensemble model
models <- c("Rborist","rf","ranger","wsrf","glm","monmlp","gbm","earth")
fits <- lapply(models, function(model){
  print(model)
  train(Outcome ~ ., method = model, data = train_set_ranged)
})
names(fits) <- models

pred_ensemble <- sapply(fits, function(object) predict(object, newdata = test_set_ranged))
acc <- colMeans(pred_ensemble == test_set_ranged$Outcome)
acc
mean(acc)
varImp(fits$glm)
ind <- acc >= 0.81
votes <- rowMeans(pred_ensemble[,ind]=="1")
y_hat <- ifelse(votes>0.5,"1","0")
mean(y_hat==test_set_ranged$Outcome)
fits$glm$finalModel
confusionMatrix(reference = test_set_ranged$Outcome, data = as.factor(pred_ensemble[,5]))
      