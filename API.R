#load packages needed
library(tidyverse)
library(ggplot2)
library(caret)
library(Metrics)
library(rpart)
library(randomForest)
library(ranger)

set.seed(11)

#read in csv data
data_original <- read_csv('diabetes_binary_health_indicators_BRFSS2015.csv')
str(data_original)

deplicates <- data_original[duplicated(data_original), ]
nrow(deplicates)

#then we need to exclude data that are duplicated
#dataset data is updated here
data_no_duplicates <- data_original[!duplicated(data_original), ]
nrow(data_no_duplicates)

#change the names of levels of response variable Diabetes_binary
#use data_no_duplicates which only contain one factor variable as response
data_2 <- data_no_duplicates

data_2$Diabetes_binary <- factor(
  data_2$Diabetes_binary, 
  levels = c(0,1), 
  labels = c("No_diabetes", "Prediabetes_or_diabetes")
)

#split data accordingly
trainIndex_2 <- createDataPartition(data_2$Diabetes_binary, p = .7,
                                    list = FALSE,
                                    times = 1)

train_data_2 <-  data_2[trainIndex_2, ]
test_data_2 <- data_2[-trainIndex_2, ]

#check the dimensions of our training data and testing data frame
dim(train_data_2)
dim(test_data_2)

pre_proc_values_2 <- preProcess(train_data_2, method = c("center", "scale"))

#Scaling and centralizing train and test data sets.
train_transformed_2 <- predict(pre_proc_values_2, train_data_2)
test_transformed_2 <- predict(pre_proc_values_2, test_data_2)

rf_fit <- train(Diabetes_binary ~ .,
                data = train_transformed_2,
                method = "ranger",
                metric = "logLoss",
                num.trees = 100,
                trControl = trainControl(method = "cv", 
                                         number = 3,
                                         classProbs = TRUE,
                                         summaryFunction = mnLogLoss),
                tuneGrid = expand.grid(mtry = 1:sqrt(ncol(data_2)-1),
                                       splitrule = "extratrees",
                                       min.node.size = 100))
rf_fit

#using logLoss as our metric for classification model:
predicted_prob_rf <- predict(rf_fit,
                             newdata = select(test_transformed_2, -Diabetes_binary),
                             type='prob')
#convert variable Diabetes_binary from factor to numeric in order to use logLoss function
log_loss_rf <- logLoss(as.numeric(as.character(test_transformed_2$Diabetes_binary) == "Prediabetes_or_diabetes"),
                       predicted_prob_rf$Prediabetes_or_diabetes)