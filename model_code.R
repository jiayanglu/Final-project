## Introduction

## Split Data

Split this data into a training and test set. Before modeling, let’s scale and centralized data.

```{r}
set.seed(11)


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
```

## Logistic Regression Models

### Logistic regression Model 1

```{r}
set.seed(11)

trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

logistic_M1_fit <- train(Diabetes_binary ~ ., 
                         data = train_transformed, 
                         method = "glm",
                         family="binomial",
                         trControl=trctrl)
summary(logistic_M1_fit)
```

### Logistic regression Model 2

```{r}
set.seed(11)

logistic_M2_fit <- train(Diabetes_binary ~ . - NoDocbcCost - Fruits - AnyHealthcare + PhysHlth:GenHlth + PhysHlth:DiffWalk + GenHlth:DiffWalk + Income:Education + Income:GenHlth + Income:DiffWalk,
                         data = train_transformed, 
                         method = "glm",
                         family="binomial",
                         trControl=trctrl)
summary(logistic_M2_fit)
```

### Logistic regression Model 3

```{r}
set.seed(11)

logistic_M3_fit <- train(Diabetes_binary ~ GenHlth + HighBP + DiffWalk + BMI + HighChol + Age + HeartDiseaseorAttack + PhysHlth + Income + PhysActivity, 
                         data = train_transformed, 
                         method = "glm",
                         family="binomial",
                         trControl=trctrl)
summary(logistic_M3_fit)
```

### Models comparison

Obtain log-loss value for each model:
  
  ```{r}
#using logLoss  as our metric for logistic M1 model:

#get predicted values corresponding to the probabilities that each observation in test data belongs to "Prediabetes or diabetes"
predicted_prob_M1 <- predict(logistic_M1_fit,
                             newdata = select(test_transformed, -Diabetes_binary), type='prob')

#convert variable Diabetes_binary from factor to numeric in order to use logLoss function
log_loss_logistic_M1 <- logLoss(as.numeric(as.character(test_transformed$Diabetes_binary) == "Prediabetes or diabetes"),
                                predicted_prob_M1$`Prediabetes or diabetes`)

#using logLoss  as our metric for logistic M2 model:

predicted_prob_M2 <- predict(logistic_M2_fit,
                             newdata = select(test_transformed, -Diabetes_binary), type='prob')

log_loss_logistic_M2 <- logLoss(as.numeric(as.character(test_transformed$Diabetes_binary) == "Prediabetes or diabetes"),
                                predicted_prob_M2$`Prediabetes or diabetes`)

#using logLoss  as our metric for logistic M3 model:

predicted_prob_M3 <- predict(logistic_M3_fit,
                             newdata = select(test_transformed, -Diabetes_binary), type='prob')

log_loss_logistic_M3 <- logLoss(as.numeric(as.character(test_transformed$Diabetes_binary) == "Prediabetes or diabetes"),
                                predicted_prob_M3$`Prediabetes or diabetes`)

#list of accuracy obtained from each logistic model
list(log_loss_logistic_M1 = log_loss_logistic_M1, log_loss_logistic_M2 = log_loss_logistic_M2, log_loss_logistic_M3 = log_loss_logistic_M3)
```

From the result after using logLoss(), we can see that all 3 logistic models have similar log-loss values (between 0.34 to 0.35). Therefore, considering the complexity of models, model 3 could be chosen as the best model among these three models since it is the simplest model among them.

## Classification Tree

```{r}

```


## Random Forest


## Final Model Selection