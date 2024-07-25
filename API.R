#load packages needed
library(tidyverse)
library(ggplot2)
library(caret)
library(Metrics)
library(rpart)
library(ranger)
library(plumber)

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

Now we want to convert numeric variables to factors according to previous sorted unique values for each variable.

data <- data_no_duplicates

data$Diabetes_binary <- factor(
  data$Diabetes_binary, 
  levels = c(0,1), 
  labels = c("No_diabetes", "Prediabetes_or_diabetes")
)

data$HighBP <- factor(
  data$HighBP,
  levels = c(0,1), 
  labels = c("No_high_BP", "High_BP")
)

data$HighChol <- factor(
  data$HighChol,
  levels = c(0,1), 
  labels = c("No_high_cholesterol", "High_cholesterol")
)

data$CholCheck <- factor(
  data$CholCheck,
  levels = c(0,1), 
  labels = c("No_cholesterol_check", "Cholesterol_check")
)

data$Smoker <- factor(
  data$Smoker,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$Stroke <- factor(
  data$Stroke,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$HeartDiseaseorAttack <- factor(
  data$HeartDiseaseorAttack,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$PhysActivity <- factor(
  data$PhysActivity,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$Fruits <- factor(
  data$Fruits,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$Veggies <- factor(
  data$Veggies,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$HvyAlcoholConsump <- factor(
  data$HvyAlcoholConsump,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$AnyHealthcare <- factor(
  data$AnyHealthcare,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$NoDocbcCost <- factor(
  data$NoDocbcCost,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$GenHlth <- factor(
  data$GenHlth,
  levels = c(1, 2, 3, 4, 5), 
  labels = c("Excellent", "Very good", "Good", "Fair", "Poor")
)

data$DiffWalk <- factor(
  data$DiffWalk,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$Sex <- factor(
  data$Sex,
  levels = c(0,1), 
  labels = c("Female", "Male")
)

data$Age <- factor(
  data$Age,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 
  labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80_or_older")
)

data$Education <- factor(
  data$Education,
  levels = c(1, 2, 3, 4, 5, 6), 
  labels = c("Never_attended_school_or_only_kindergarten", "Elementary", "Some_high_school", "High school_graduate", "Some_college_or_technical_school", "College_graduate")
)

data$Income <- factor(
  data$Income,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8), 
  labels = c("Less_than_10K", "10K_to_less_than_15K", "15K_to_Less_than_20K", "20K_to_less_than_25K", "25K_to_less_than_35K", "35K_to_less_than_50k", "50k_to_less_than_75k", "75k_or_more")
)

str(data)


## Split Data

#Split this data into a training and test set. Before modeling, let’s scale and centralized data.

trainIndex <- createDataPartition(data$Diabetes_binary, p = .7,
                                  list = FALSE,
                                  times = 1)

train_data <-  data[trainIndex, ]
test_data <- data[-trainIndex, ]

#check the dimensions of our training data and testing data frame
dim(train_data)
dim(test_data)

pre_proc_values <- preProcess(train_data, method = c("center", "scale"))

#Scaling and centralizing train and test data sets.
train_transformed <- predict(pre_proc_values, train_data)
test_transformed <- predict(pre_proc_values, test_data)

### Logistic regression Model 3

logistic_M3_fit <- train(Diabetes_binary ~ GenHlth + HighBP + DiffWalk + BMI + HighChol + Age + HeartDiseaseorAttack + PhysHlth + Income + PhysActivity, 
                         data = train_transformed, 
                         method = "glm",
                         family="binomial",
                         trControl=trctrl)


rf_fit_4 <- train(Diabetes_binary ~ .,
                data = train_transformed_2,
                method = "ranger",
                metric = "logLoss",
                num.trees = 100,
                trControl = trainControl(method = "cv", 
                                         number = 3,
                                         classProbs = TRUE,
                                         summaryFunction = mnLogLoss),
                tuneGrid = expand.grid(mtry = 4,
                                       splitrule = "extratrees",
                                       min.node.size = 100))

